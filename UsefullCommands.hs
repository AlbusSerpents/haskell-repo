module UsefullCommands 
(
        delete,
        renameWith,
        rename
)
where 

import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, renameDirectory, renameFile, removeDirectoryRecursive, removeFile)
import Glob (namesMatching)
import Utils (clear, isDir)

delete :: String -> IO()
delete pattern = do
        files <- namesMatching pattern
        deleteOneByOne files

deleteOneByOne :: [String] -> IO()
deleteOneByOne [] = return ()
deleteOneByOne (f:fs) = do
                dir <- isDir f
                if dir
                        then removeDirectoryRecursive f
                else
                        removeFile f
                deleteOneByOne fs

renameWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath
renameWith func path = do 
        let newPath = func path
        rename path newPath
        return newPath

rename :: FilePath -> FilePath -> IO()
rename old new = do
        func <- renameFunc old
        func new

renameFunc :: FilePath -> IO (FilePath -> IO())
renameFunc path = do
        dir <- isDir path
        if dir
                then return (renameDirectory path)
        else
                return (renameFile path)
