compress :: String -> String
compress = filter (/= '1') . compress'

compress' :: String -> String
compress' []     = []
compress' (c:cs) = c : (show (repLen + 1)) ++ (compress' $ drop repLen cs)
  where repLen = length $ takeWhile (== c) cs

main :: IO ()
main = interact compress


