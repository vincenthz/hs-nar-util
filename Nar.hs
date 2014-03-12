{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Options.Applicative
import Archive.Nar

import System.Directory
import System.FilePath
import System.Posix.Files
import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception

recurse :: FilePath
        -> (a -> FilePath -> IO a)
        -> (a -> FilePath -> IO (Bool, a))
        -> a
        -> IO a
recurse rootDir fFile fDir acc = loop acc rootDir
  where loop a dir = do
            content <- try $ getDir dir
            case content of
                Left (exn :: SomeException) -> return a
                Right l  -> foldM (processEnt dir) a l
        processEnt dir a ent = do
            let fp = dir </> ent
            stat <- getSymbolicLinkStatus fp
            case (isDirectory stat, isRegularFile stat) of
                (True,_)     -> do (process,a') <- fDir a fp
                                   if process
                                      then loop a' fp
                                      else return a'
                (False,True)  -> fFile a fp
                (False,False) -> return a
        getDir dir = filter (not . flip elem [".",".."]) <$> getDirectoryContents dir

recurse_ :: FilePath
         -> (FilePath -> IO ())
         -> (FilePath -> IO Bool)
         -> IO ()
recurse_ rootDir fFile fDir =
    recurse rootDir (\_ -> fFile) (\_ fp -> (\b -> (b, ())) <$> fDir fp) ()

data Opt =
      Extract { extractDirectory :: FilePath
              , extractArchive   :: FilePath
              , extractPaths     :: [FilePath]
              }
    | Create { createCompressed :: Bool
             , createArchive :: FilePath
             , createPaths   :: [FilePath]
             }
    deriving (Show,Eq)

opts = subparser
    (  command "xf" (info (extractOpts) (progDesc "extract an archive"))
    <> command "xzf" (info (extractOpts) (progDesc "extract an archive"))
    <> command "cf" (info (createOpts False) (progDesc "create an archive"))
    <> command "czf" (info (createOpts True) (progDesc "create an archive"))
    )
  where extractOpts =
            Extract <$> strOption (long "directory" <> short 'C' <> metavar "DIR")
                    <*> argument Just (metavar "<ARCHIVE>")
                    <*> arguments Just (metavar "[PATH..]")
        createOpts compressed =
            Create <$> pure compressed
                   <*> argument Just (metavar "<ARCHIVE>")
                   <*> arguments Just (metavar "[FILE..]")

getOpt = execParser (info opts idm)

doMain (Extract cdir archive paths) = do
    error "not implemented"
doMain args@(Create {}) = do
    alreadyExist <- doesFileExist $ createArchive args
    when alreadyExist $ error ("archive " ++ show (createArchive args) ++ " already exists")

    let narReg = defaultNarRegister
        compressAlg = if createCompressed args
                        then gzipCompression
                        else noCompression
        cipherAlg = noCipher

    createNar narReg (cipherAlg, compressAlg) (createArchive args) $ \appender -> do
        forM_ (createPaths args) $ \path -> do
            dir <- doesDirectoryExist path
            if dir
                then recurse_ path (afs appender) (const $ return True)
                else afs appender path
  where afs appender fp = do
            --putStrLn ("adding " ++ show fp)
            stat <- getSymbolicLinkStatus fp
            let isExecutable = False
            appender $ appendFileSimple fp (createCompressed args) isExecutable
main = getOpt >>= doMain
