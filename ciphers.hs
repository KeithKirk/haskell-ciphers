main = do
	key <- askUserForKey
	putStrLn key
	

askUserForKey :: IO String
askUserForKey = do
	putStrLn "Please enter Key:"
	return getLine
