module DarcsDen.State.Util where

charIsSane :: Char -> Bool
charIsSane = flip elem (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_")

isSane :: String -> Bool
isSane = all charIsSane

saneName :: String -> String
saneName = filter charIsSane

userDir :: String -> String
userDir un = "/jail/home/" ++ saneName un

repoDir :: String -> String -> FilePath
repoDir un rn = userDir un ++ "/" ++ saneName rn