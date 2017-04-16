module FileTree
(
        allChildFiles
)
where 

import System.FilePath
import System.Posix (getFileStatus, isDirectory)

allChildFiles :: FilePath ->  IO [FilePath]
allChildFiles file = undefined
