type Rotor = [(Int,Int)]
type RotorArray = [Rotor]
type Key = [Int]

charRotor = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']

charLimit = length charRotor - 1

initRotor = [(x, charLimit - x)|x<-[0..charLimit]]

{- Functions To Simulate the Operation of Rotors -}

setupRotors :: Key -> RotorArray
setupRotors [] = [] 
setupRotors key = (rotateRotor initRotor (head key)): setupRotors (tail key)

        
rotateRotors :: RotorArray -> Key -> RotorArray
rotateRotors (top:[]) key = (rotateRotor top 1) : []
rotateRotors (top:bottom) (currentKey:nextKey:furtherKeys) =
        if nextPosition == nextKey 
        then (rotateRotor top 1) : rotateRotors bottom (nextKey:furtherKeys)
        else top : rotateRotors bottom (nextKey:furtherKeys)
        where 
                nextPosition = fst (head (tail (head bottom)))

rotateRotor :: Rotor -> Int -> Rotor
rotateRotor rotor 0 = rotor
rotateRotor (top:bottom) amount = rotateRotor ((bottom) ++ [(top)]) (amount - 1)
        

{- Functions To Search the Rotors -}

findPosition :: (Eq a) => [a] -> a -> Int
findPosition rotor character =
        if head rotor == character 
        then 0
        else (findPosition (tail rotor) character) + 1
        
getPosition :: Rotor -> Int -> Int
getPosition rotor value =
        if snd (head rotor) == value 
        then 0
        else (getPosition (tail rotor) value) + 1

resolveRotors :: RotorArray -> Int -> Int
resolveRotors (top:[]) position = snd(top !! position)
resolveRotors (top:bottom) position = resolveRotors bottom (snd(top !! position))
        
resolveRotorsBackwards :: RotorArray -> Int -> Int
resolveRotorsBackwards (top:[]) position = getPosition top position
resolveRotorsBackwards (top:bottom) position = getPosition top (resolveRotorsBackwards bottom position)

{- Code To Encode Text -}

encodeString :: String -> RotorArray -> Key -> String
encodeString [] rotors key = []
encodeString (top:bottom) rotors key = (encodeChar rotors top) : (encodeString bottom (rotateRotors rotors key) key)

encodeChar :: RotorArray -> Char -> Char
encodeChar rotors plain = charRotor !! (resolveRotors rotors (findPosition charRotor plain))

encrypt :: String -> Key -> String
encrypt plainText key = encodeString plainText (setupRotors key) key

{- Code To Decode Text -}

decodeString :: String -> RotorArray -> Key -> String
decodeString [] rotors key = []
decodeString (top:bottom) rotors key = (decodeChar rotors top) : (decodeString bottom (rotateRotors rotors key) key)

decodeChar :: RotorArray -> Char -> Char
decodeChar rotors cipher = charRotor !! (resolveRotorsBackwards rotors (findPosition charRotor cipher))

decrypt :: String -> Key -> String
decrypt cipherText key = decodeString cipherText (setupRotors key) key
