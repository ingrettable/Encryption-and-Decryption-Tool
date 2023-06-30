import Data.Char
import System.Random
import Data.List

generateKey :: IO String
generateKey = do
    gen <- newStdGen
    let letters = ['A'..'Z']
        sequence = take 26 (nub (randomRs ('A', 'Z') gen))
    return sequence

encrypt :: String -> String -> String
encrypt key str = map replace str
    where
        replace ch
            | ch == ' ' = ' '
            | otherwise = let index = indexOf ['A'..'Z'] ch
                in if index == -1
                    then ch
                    else key !! index

decrypt :: String -> String -> String
decrypt key str = map replace str
    where
        replace ch
            | ch == ' ' = ' '
            | otherwise = let index = indexOf key ch
                in if index == -1
                    then ch
                    else ['A'..'Z'] !! index

indexOf :: String -> Char -> Int
indexOf str ch = go str 0
    where
        go [] _ = -1 
        go (i:elements) index
            | i == ch = index 
            | otherwise = go elements (index + 1)

splitString :: String -> [String]
splitString str = go str ""
    where
        go [] list = [reverse list]
        go (i:elements) list
            | i == '.'  = reverse list : go elements ""
            | otherwise = go elements (i:list)
            
convert :: String -> Int
convert str = go str 0
    where
        go [] counter = counter
        go (i:elements) counter
            | isDigit i = go elements (counter * 10 + digitToInt i)
            | otherwise = go elements counter

splitAndConvert :: String -> [Int]
splitAndConvert str = map convert (splitString str)

combine :: [Int] -> Int
combine elements =
    let [e1, e2, e3, e4] = elements
    in (e1 * 256^3) + (e2 * 256^2) + (e3 * 256) + e4

ipAddressToNum :: String -> Int
ipAddressToNum ipAddress =
    let elements = splitAndConvert ipAddress
    in combine elements

numToIpAddress :: Int -> String
numToIpAddress num = show e1 ++ "." ++ show e2 ++ "." ++ show e3 ++ "." ++ show e4
    where
        e1 = (num `div` (256^3)) `mod` 256
        e2 = (num `div` (256^2)) `mod` 256
        e3 = (num `div` 256) `mod` 256
        e4 = num `mod` 256

main :: IO ()
main = do
    key <- generateKey
    let plaintext = "HELLO WORLD."
        ciphertext = encrypt key plaintext
        decryptedText = decrypt key ciphertext
    putStrLn $ "Plaintext: " ++ plaintext
    putStrLn $ "Key: " ++ key
    putStrLn $ "Ciphertext: " ++ ciphertext
    putStrLn $ "Decrypted Text: " ++ decryptedText
    let number = 3232235521
    let ipAddress = numToIpAddress number
    putStrLn ipAddress
    let ipAddressStr = "192.168.0.1"
    let numberFromIP = ipAddressToNum ipAddressStr
    print numberFromIP

