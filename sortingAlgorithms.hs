import Text.Read (readMaybe)
import System.Random (randomRs, newStdGen)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- Function definitions for sorting algorithms

-- Bubble Sort
bubbleSort :: [Int] -> ([Int], Int, Int)
bubbleSort lst = go lst (length lst) 0 0
  where
    go :: [Int] -> Int -> Int -> Int -> ([Int], Int, Int)
    go xs n comps swaps
      | n <= 1 = (xs, comps, swaps)
      | otherwise =
          let (newList, newComps, newSwaps) = onePass xs comps swaps
          in go newList (n - 1) newComps newSwaps

    onePass :: [Int] -> Int -> Int -> ([Int], Int, Int)
    onePass [] comps swaps = ([], comps, swaps)
    onePass [x] comps swaps = ([x], comps, swaps)
    onePass (x:y:xs) comps swaps
      | x > y =
          let (sortedTail, comps', swaps') = onePass (x:xs) (comps + 1) (swaps + 1)
          in (y : sortedTail, comps', swaps')  -- Correctly returning a tuple
      | otherwise =
          let (sortedTail, comps', swaps') = onePass (y:xs) (comps + 1) swaps
          in (x : sortedTail, comps', swaps')  -- Correctly returning a tuple

-- Selection Sort
selectionSort :: [Int] -> ([Int], Int, Int)
selectionSort [] = ([], 0, 0)
selectionSort lst = selectionSortHelper lst 0 0
  where
    selectionSortHelper :: [Int] -> Int -> Int -> ([Int], Int, Int)
    selectionSortHelper [] comps swaps = ([], comps, swaps)
    selectionSortHelper xs comps swaps =
      let minElem = minimum xs
          (index, _) = minimumByIndex xs
      in if index < length xs
         then
           let (before, after) = splitAt index xs
           in case after of
                [] -> error "Unexpected pattern: after splitAt index, but list is empty."
                (_:rest) ->
                  let newList = before ++ rest
                      (sortedTail, restComps, restSwaps) = selectionSortHelper newList (comps + length xs - 1) (swaps + 1)
                  in (minElem : sortedTail, restComps + 1, restSwaps + 1)
         else error "Index out of bounds in selectionSortHelper."

    minimumByIndex :: [Int] -> (Int, Int)
    minimumByIndex lst = go lst 0 (head lst) 0
      where
        go :: [Int] -> Int -> Int -> Int -> (Int, Int)
        go [] _ currentMin currentIdx = (currentIdx, currentMin)
        go (x:xs) idx currentMin currentIdx
          | x < currentMin = go xs (idx + 1) x idx
          | otherwise      = go xs (idx + 1) currentMin currentIdx

-- Insertion Sort
insertionSort :: [Int] -> ([Int], Int, Int)
insertionSort [] = ([], 0, 0)
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert :: Int -> ([Int], Int, Int) -> ([Int], Int, Int)
    insert y (sorted, comps, swaps) = insertHelper y sorted comps swaps

    insertHelper :: Int -> [Int] -> Int -> Int -> ([Int], Int, Int)
    insertHelper y [] comps swaps = ([y], comps, swaps)
    insertHelper y (z:zs) comps swaps
      | y <= z    = (y : z : zs, comps + 1, swaps)
      | otherwise =
          let (sortedTail, comps', swaps') = insertHelper y zs (comps + 1) swaps
          in (z : sortedTail, comps', swaps')

-- Merge Sort
mergeSort :: [Int] -> ([Int], Int, Int)
mergeSort [] = ([], 0, 0)
mergeSort [x] = ([x], 0, 0)
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

    merge :: ([Int], Int, Int) -> ([Int], Int, Int) -> ([Int], Int, Int)
    merge (sortedLeft, compsLeft, swapsLeft) (sortedRight, compsRight, swapsRight) =
      let (mergedList, compsMerged, swapsMerged) = mergeHelper sortedLeft sortedRight (compsLeft + compsRight) (swapsLeft + swapsRight)
      in (mergedList, compsMerged, swapsMerged)

    mergeHelper :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
    mergeHelper [] ys comps swaps = (ys, comps, swaps)
    mergeHelper xs [] comps swaps = (xs, comps, swaps)
    mergeHelper (x:xs) (y:ys) comps swaps
      | x <= y =
          let (sortedTail, comps', swaps') = mergeHelper xs (y:ys) (comps + 1) swaps
          in (x : sortedTail, comps', swaps')
      | otherwise =
          let (sortedTail, comps', swaps') = mergeHelper (x:xs) ys (comps + 1) (swaps + 1)
          in (y : sortedTail, comps', swaps')

-- Quick Sort
quickSort :: [Int] -> ([Int], Int, Int)
quickSort [] = ([], 0, 0)
quickSort (p:xs) = (sortedLeft ++ [p] ++ sortedRight, compsLeft + compsRight + length xs, swapsLeft + swapsRight)
  where
    leftPartition = [x | x <- xs, x <= p]
    rightPartition = [x | x <- xs, x > p]
    (sortedLeft, compsLeft, swapsLeft) = quickSort leftPartition
    (sortedRight, compsRight, swapsRight) = quickSort rightPartition

-- Function to print sorted result with statistics
printSortedResult :: ([Int], Int, Int) -> IO ()
printSortedResult (sortedList, comparisons, swaps) = do
    putStrLn "Sorted list:"
    print sortedList
    putStrLn $ "Number of comparisons: " ++ show comparisons
    putStrLn $ "Number of swaps: " ++ show swaps

-- Enhanced function to read a list of integers from user input
readIntList :: IO [Int]
readIntList = do
    putStrLn "Enter the list of integers separated by spaces:"
    input <- getLine
    let numbers = words input
    case mapM readMaybe numbers of
        Just validNumbers -> return validNumbers
        Nothing -> do
            putStrLn "Invalid input! Please enter only integers."
            readIntList  -- Retry on invalid input

-- Enhanced function to read number of random integers
readPositiveInt :: IO Int
readPositiveInt = do
    putStrLn "Enter the number of random integers to generate:"
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just n | n > 0 -> return n  -- Return valid positive integer
        _ -> do
            putStrLn "Please enter a valid positive integer."
            readPositiveInt  -- Retry on invalid input

-- Function to generate a list of random integers
generateRandomList :: Int -> IO [Int]
generateRandomList n = do
    gen <- newStdGen
    let randomList = take n (randomRs (1, 100) gen)  -- Random integers between 1 and 100
    return randomList

-- Main interactive program
main :: IO ()
main = do
    putStrLn "Sort Algorithms Demonstration"

    let loop = do
            putStrLn "\nChoose a sorting algorithm:"
            putStrLn "1. Bubble Sort"
            putStrLn "2. Selection Sort"
            putStrLn "3. Insertion Sort"
            putStrLn "4. Merge Sort"
            putStrLn "5. Quick Sort"
            putStrLn "6. Generate Random List and Bubble Sort"
            putStrLn "7. Exit"

            algorithmChoice <- getLine

            case algorithmChoice of
                "1" -> do
                    putStrLn "You chose Bubble Sort."
                    numbers <- readIntList
                    start <- getCurrentTime
                    let (sorted, comps, swaps) = bubbleSort numbers
                    end <- getCurrentTime
                    printSortedResult (sorted, comps, swaps)
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

                "2" -> do
                    putStrLn "You chose Selection Sort."
                    numbers <- readIntList
                    start <- getCurrentTime
                    let (sorted, comps, swaps) = selectionSort numbers
                    end <- getCurrentTime
                    printSortedResult (sorted, comps, swaps)
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

                "3" -> do
                    putStrLn "You chose Insertion Sort."
                    numbers <- readIntList
                    start <- getCurrentTime
                    let (sorted, comps, swaps) = insertionSort numbers
                    end <- getCurrentTime
                    printSortedResult (sorted, comps, swaps)
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

                "4" -> do
                    putStrLn "You chose Merge Sort."
                    numbers <- readIntList
                    start <- getCurrentTime
                    let (sorted, comps, swaps) = mergeSort numbers
                    end <- getCurrentTime
                    printSortedResult (sorted, comps, swaps)
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

                "5" -> do
                    putStrLn "You chose Quick Sort."
                    numbers <- readIntList
                    start <- getCurrentTime
                    let (sorted, comps, swaps) = quickSort numbers
                    end <- getCurrentTime
                    printSortedResult (sorted, comps, swaps)
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

                "6" -> do
                    n <- readPositiveInt  -- Call the function with error checking
                    randomList <- generateRandomList n
                    putStrLn $ "Generated list: " ++ show randomList
                    putStrLn "Press Enter to sort using Bubble Sort:"
                    _ <- getLine
                    start <- getCurrentTime
                    let (sorted, comps, swaps) = bubbleSort randomList
                    end <- getCurrentTime
                    printSortedResult (sorted, comps, swaps)
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)

                "7" -> do
                    putStrLn "Exiting program. Goodbye!"
                    return ()

                _ -> do
                    putStrLn "Invalid option! Please choose a valid sorting algorithm."

            loop  -- Continue the loop after completing the action

    loop  -- Start the interactive loop
