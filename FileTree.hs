module FileTree
(
        getFullDirectoryContent
)
where 

import System.FilePath ((</>), addTrailingPathSeparator)
import System.Posix (getFileStatus, isDirectory)
import Control.Monad (forM, mapM)
import System.Directory (getDirectoryContents)
import Data.List (delete)


getFullDirectoryContent :: FilePath ->  IO [FilePath]
getFullDirectoryContent "./" = return ([])
getFullDirectoryContent "../" = return ([])
getFullDirectoryContent filePath = do 
        results <- getLayer filePath
        execute results
        where 
                execute (files, []) = do return (map (filePath </>) files)
                execute (files, dirs) = do
                                sub <- mapM getFullDirectoryContent dirs
                                return ((map (filePath </>) files) ++ concat sub)  

getLayer :: FilePath -> IO ([FilePath],[FilePath])
getLayer path = do
        contents <- getDirectoryContents path
        marked <- forM (filter (\e -> e `notElem` selfRelatingDirectories) contents) (assignFileState path)
        return (foldr foldHelper ([],[]) marked)

isDir :: FilePath -> IO Bool
isDir path = do
        status <- getFileStatus path
        return (isDirectory status)

data FileState = File | Directory
selfRelatingDirectories = [".",".."]

assignFileState :: FilePath -> FilePath -> IO (FilePath, FileState)
assignFileState source path =  do
        status <- isDir (fullPath)
        if status 
                then return(addTrailingPathSeparator fullPath, Directory)
                else return(path, File)
        where fullPath = source </> path

foldHelper :: (FilePath, FileState) -> ([FilePath], [FilePath]) -> ([FilePath], [FilePath]) 
foldHelper (path, File) (files, dirs) = (path : files, dirs)
foldHelper (path, Directory) (files, dirs) = (files, path : dirs)

