module FileTree
(
        allChildFiles
)
where 

import System.FilePath ((</>))
import System.Posix (getFileStatus, isDirectory)
import Control.Monad (forM)
import System.Directory (getDirectoryContents)

data State = File | Directory

allChildFiles :: FilePath ->  IO [FilePath]
allChildFiles file = undefined

isDir :: FilePath -> IO Bool
isDir path = do
        status <- getFileStatus path
        return (isDirectory status)

getLayer :: FilePath -> IO ([FilePath],[FilePath])
getLayer path = do
        contents <- getDirectoryContents path
        marked <- forM contents (\fp -> do 
                        status <- isDir (path </> fp)
                        if status 
                                then return(fp, Directory)
                                else return(fp, File)                        
                      )
        return (foldr foldHelper ([],[]) marked)

foldHelper :: (FilePath, State) -> ([FilePath], [FilePath]) -> ([FilePath], [FilePath]) 
foldHelper (path, File) (files, dirs) = (path : files, dirs)
foldHelper (path, Directory) (files, dirs) = (files, path : dirs)

