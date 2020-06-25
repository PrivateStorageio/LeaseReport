module Main where

import Prelude hiding
  ( putStrLn
  )

import System.IO
  ( withBinaryFile
  , hSeek
  , SeekMode(AbsoluteSeek)
  , IOMode(ReadMode)
  )

import Data.Time.Calendar
  ( fromGregorian
  )

import Data.Time.Clock
  ( UTCTime(UTCTime)
  , DiffTime
  , addUTCTime
  )

import Data.Text
  ( Text
  , intercalate
  , append
  , pack
  )
import Data.Text.IO
  ( putStrLn
  )
import Control.Exception
  ( try
  )

import System.Directory
  ( getCurrentDirectory
  )

import System.Directory.PathWalk
  ( Callback
  , pathWalkLazy
  )

import qualified Data.ByteString.Lazy as BL

import Data.Binary.Get
  ( Get
  , getWord32be
  , getLazyByteString
  , runGet
  )

import Data.Word
  ( Word32
  )

data ExpirationTime
  = ExpirationTime UTCTime
  deriving (Show)

data LeaseInfo
  = LeaseInfo ExpirationTime
  deriving (Show)

data PathReport
  = Failed Text
  | PathReport [LeaseInfo]
  deriving (Show)

type StorageReport = [(FilePath, PathReport)]

showReport :: StorageReport -> Text
showReport items =
  let
    formatOne :: (FilePath, PathReport) -> Text
    formatOne (path, report) = (pack . show) report `append` " " `append` (pack path)
    formatAll :: [(FilePath, PathReport)] -> [Text]
    formatAll = map formatOne
    joinLines :: [Text] -> Text
    joinLines = (`append` "\n") . (intercalate "\n")
  in
    joinLines . formatAll $ items

type ShareVisitor
  = (FilePath -> IO StorageReport)

data ShareKind = Immutable | Mutable deriving (Show, Eq)

determineShareKind :: FilePath -> IO ShareKind
determineShareKind path =
  let
    magic :: BL.ByteString
    magic = "tahoe mutable container v1\x75\x09\x44\x03\x8e"
  in
    withBinaryFile path ReadMode $ \share ->
    do
      headerBytes <- BL.hGet share (fromInteger . toInteger . BL.length $ magic)
      if headerBytes == magic
        then return Mutable
        else return Immutable


getLeases :: FilePath -> IO PathReport
getLeases sharefile = do
  kind <- determineShareKind sharefile
  case kind of
    Immutable -> do
      result <- readImmutableLeases sharefile
      case result of
        Left reason -> return . Failed $ reason
        Right leases -> return . PathReport $ leases
    Mutable -> do
      leases <- readMutableLeases sharefile
      return . PathReport $ leases


deserializeImmutableHeader :: Get (Integer, Integer, Integer)
deserializeImmutableHeader = do
  shareVersion <- getWord32be
  shareLength <- getWord32be
  leaseCount <- getWord32be
  return (toInteger shareVersion, toInteger shareLength, toInteger leaseCount)

deserializeImmutableLease :: Get (Integer, BL.ByteString, BL.ByteString, Integer)
deserializeImmutableLease = do
  owner <- getWord32be
  renewSecret <- getLazyByteString 32
  cancelSecret <- getLazyByteString 32
  expiration <- getWord32be
  return (toInteger owner, renewSecret, cancelSecret, toInteger expiration)

readImmutableLeases :: FilePath -> IO (Either Text [LeaseInfo])
readImmutableLeases path =
  withBinaryFile path ReadMode $ \share ->
  do
    headerBytes <- BL.hGet share 12
    if BL.length headerBytes /= 12
      then return . Left $ "Short header"
      else
      do
        let (shareVersion, shareLength, leaseCount) = runGet deserializeImmutableHeader headerBytes
        hSeek share AbsoluteSeek (shareLength + 12)
        leaseBytes <- BL.hGet share 72
        if BL.length leaseBytes /= 72
          then return . Left $ "Incomplete lease structure"
          else
          let
            (owner, renew, cancel, expiration) = runGet deserializeImmutableLease leaseBytes
          in
            return . Right $ [LeaseInfo . ExpirationTime . toUTCTime $ expiration]

toUTCTime :: Integer -> UTCTime
toUTCTime posixTimestamp =
  let
    epoch = UTCTime (fromGregorian 1970 1 1) 0
  in
    addUTCTime (fromInteger posixTimestamp) epoch

readMutableLeases :: FilePath -> IO [LeaseInfo]
readMutableLeases path = return []

getLeasesCallback :: (FilePath, [FilePath], [FilePath]) -> IO StorageReport
getLeasesCallback (dir, subdirs, files) = do
  let absfiles = map ((dir ++ "/") ++) files
  leaseInfo <- mapM getLeases absfiles
  return $ zip absfiles leaseInfo

getStorageReport :: FilePath -> IO StorageReport
getStorageReport root = do
  paths <- pathWalkLazy root
  reports <- mapM getLeasesCallback paths
  return $ foldl (++) [] reports

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  report <- getStorageReport cwd
  putStrLn $ showReport report
