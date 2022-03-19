-- fs406372
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . showString (show v)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showString "\n"
pprH = intercalateS $ showString " "

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS _ [] = id
intercalateS sep [l] = l
intercalateS sep (l:ls) = l . sep . intercalateS sep ls

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f l = pprV $ map f l

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
