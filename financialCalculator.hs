-- Financial Calculator in Haskell

import Text.Printf (printf)
-- 'Text.Printf' for formatting the output.
import Data.Maybe (fromMaybe)
-- 'Data.Maybe' is imported in case you want to handle optional values in the future.

-- Function to calculate Simple Interest
-- Takes three parameters (principal, rate, and time) and calculates simple interest.
simpleInterest :: Float -> Float -> Float -> Float
simpleInterest principal rate time = principal * (rate / 100) * time

-- Function to calculate Compound Interest
-- Takes four parameters (principal, rate, number of compounding periods per year, and time) and calculates the future value.
compoundInterest :: Float -> Float -> Float -> Float -> Float
compoundInterest principal rate n time =
    principal * (1 + (rate / (n * 100))) ** (n * time)  -- Use ** for Floating

-- Function to display menu options and perform actions
main :: IO ()
main = do
    putStrLn "Welcome to the Basic Financial Calculator!"
    calculatorLoop

calculatorLoop :: IO ()
-- 'calculatorLoop' displays options repeatedly until the user chooses to exit.
calculatorLoop = do
    putStrLn "\nSelect an option:"
    putStrLn "1. Calculate Simple Interest"
    putStrLn "2. Calculate Compound Interest"
    putStrLn "3. Exit"
    
    choice <- getLine
    
    case choice of
        "1" -> do
            calculateSimpleInterest
            calculatorLoop
        "2" -> do
            calculateCompoundInterest
            calculatorLoop
        "3" -> putStrLn "Exiting the calculator. Goodbye!"
        _   -> do
            putStrLn "Invalid choice! Please try again."
            calculatorLoop

-- Calculate Simple Interest
calculateSimpleInterest :: IO ()
calculateSimpleInterest = do
    putStrLn "\nEnter Principal Amount (P):"
    principalInput <- getLine
    let principal = read principalInput :: Float

    putStrLn "Enter Rate of Interest (R):"
    rateInput <- getLine
    let rate = read rateInput :: Float

    putStrLn "Enter Time (T) in years:"
    timeInput <- getLine
    let time = read timeInput :: Float

    let interest = simpleInterest principal rate time
    printf "Simple Interest: %.2f\n" interest

-- Calculate Compound Interest
calculateCompoundInterest :: IO ()
calculateCompoundInterest = do
    putStrLn "\nEnter Principal Amount (P):"
    principalInput <- getLine
    let principal = read principalInput :: Float

    putStrLn "Enter Annual Rate of Interest (R):"
    rateInput <- getLine
    let rate = read rateInput :: Float

    putStrLn "Enter Compounding Frequency (n, e.g., annually = 1, semi-annually = 2):"
    frequencyInput <- getLine
    let frequency = read frequencyInput :: Float

    putStrLn "Enter Time (T) in years:"
    timeInput <- getLine
    let time = read timeInput :: Float

    let futureValue = compoundInterest principal rate frequency time
    printf "Future Value with Compound Interest: %.2f\n" futureValue