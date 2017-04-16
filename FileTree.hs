module FileTree
(
        allChildFiles
)
where 

import System.FilePath ((</>), addTrailingPathSeparator)
import System.Posix (getFileStatus, isDirectory)
import Control.Monad (forM, mapM)
import System.Directory (getDirectoryContents)
import Data.List (delete)

data FileState = File | Directory

allChildFiles :: FilePath ->  IO [FilePath]
allChildFiles "./" = return ([])
allChildFiles "../" = return ([])
allChildFiles filePath = do 
        results <- getLayer filePath
        execute results
        where 
                execute (files, []) = do return (map (filePath </>) files)
                execute (files, dirs) = do
                                sub <- mapM allChildFiles dirs
                                return ((map (filePath </>) files) ++ concat sub)  

selfRelatingDirectories = [".",".."]

isDir :: FilePath -> IO Bool
isDir path = do
        status <- getFileStatus path
        return (isDirectory status)

getLayer :: FilePath -> IO ([FilePath],[FilePath])
getLayer path = do
        contents <- getDirectoryContents path
        marked <- forM (filter (\e -> e `notElem` selfRelatingDirectories) contents) (assignFileState path)
        return (foldr foldHelper ([],[]) marked)

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

