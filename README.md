## Encryption-and-Decryption-Tool
This project demonstrates basic encryption and IP address conversion functionalities using Haskell. It includes functions for generating a random key, encrypting and decrypting messages, converting IP addresses to integers, and vice versa.

## How to Run
1. Make sure you have Haskell installed on your system.
2. Clone the repository and navigate to the project's directory.
3. To compile the code, use the following command: ghc --make main.hs
4. To run the program, execute the compiled binary: ./main
5. The program will generate a random key, encrypt a sample plaintext, and then decrypt it back to the original text. It will also convert an IP address to an integer and vice versa.

## Functions
### generateKey :: IO String
This function generates a random key containing unique uppercase letters.
#### encrypt :: String -> String -> String
Encrypts a given plaintext message using a provided key. It uses a simple substitution cipher, replacing each character in the message with the corresponding character from the key.
### decrypt :: String -> String -> String
Decrypts an encrypted message using the provided key, reversing the substitution cipher process.
### indexOf :: String -> Char -> Int
Helper function that returns the index of a character in a string. If the character is not found, it returns -1.
### splitString :: String -> [String]
Splits a given string into a list of strings using '.' as a delimiter.
### convert :: String -> Int
Converts a string containing digits into an integer.
### splitAndConvert :: String -> [Int]
Combines the splitString and convert functions to split an IP address string and convert it into a list of integers representing the four components.
### numToIpAddress :: Int -> String
Converts an integer back into an IP address string.
### ipAddressToNum :: String -> Int
Converts an IP address string into an integer.
### combine :: [Int] -> Int
Helper function to combine the four components of an IP address (list of integers) into a single integer.

## Example
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
    let ipAddressStr = "192.168.0.1"
    let numberFromIP = ipAddressToNum ipAddressStr
    putStrLn $ "IP Address: " ++ ipAddressStr
    putStrLn $ "Number representation: " ++ show numberFromIP
    let number = 3232235521
    let ipAddress = numToIpAddress number
    putStrLn $ "Number: " ++ show number
    putStrLn $ "IP Address representation: " ++ ipAddress


