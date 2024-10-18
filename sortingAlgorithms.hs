import Text.Read (readMaybe)  -- To read and parse user input
import System.Random (randomRs, newStdGen)  -- For generating random numbers
import Data.Time.Clock (diffUTCTime, getCurrentTime)  -- To measure time taken for sorting
import Data.List (intercalate)  -- For easily joining strings
import System.Exit (exitSuccess, exitFailure)  -- For terminating program

-- Define a data type for sorting algorithms and their complexity info
data SortingAlgorithm = SortingAlgorithm
    { name            :: String          -- The name of the sorting algorithm
    , sort            :: [Int] -> ([Int], Int, Int)  -- Function to sort a list of integers
    , timeComplexity  :: String          -- Time complexity of the algorithm
    , spaceComplexity :: String          -- Space complexity of the algorithm
    , description     :: String          -- Brief description of how the algorithm works
    }

-- Function definitions for sorting algorithms

-- Bubble Sort implementation
bubbleSort :: [Int] -> ([Int], Int, Int)
bubbleSort lst = go lst (length lst) 0 0  -- Start the recursive process
  where
    -- Helper function for recursive bubble sort
    go xs n comps swaps
      | n <= 1 = (xs, comps, swaps)  -- If the list has 1 or 0 elements, it's already sorted
      | otherwise =
          let (newList, newComps, newSwaps) = onePass xs comps swaps  -- Process one pass
          in go newList (n - 1) newComps newSwaps  -- Recur for the next pass

    -- Helper function to handle one pass of bubble sort
    onePass [] comps swaps = ([], comps, swaps)  -- Base case for empty list
    onePass [x] comps swaps = ([x], comps, swaps)  -- Base case for single element
    onePass (x:y:xs) comps swaps
      | x > y =  -- If the first element is greater, swap them
          let (sortedTail, comps', swaps') = onePass (x:xs) (comps + 1) (swaps + 1)
          in (y : sortedTail, comps', swaps')  -- Return the swapped result
      | otherwise =  -- If no swap is needed
          let (sortedTail, comps', swaps') = onePass (y:xs) (comps + 1) swaps
          in (x : sortedTail, comps', swaps')  -- Return the result

-- Selection Sort implementation
selectionSort :: [Int] -> ([Int], Int, Int)
selectionSort [] = ([], 0, 0)  -- Base case for empty list
selectionSort lst = selectionSortHelper lst 0 0  -- Start selection sort process
  where
    selectionSortHelper [] comps swaps = ([], comps, swaps)  -- Base case for recursion
    selectionSortHelper xs comps swaps =
      let minElem = minimum xs  -- Find the minimum element in the list
          (index, _) = minimumByIndex xs  -- Get the index of the minimum element
      in if index < length xs  -- Check if index is valid
         then
           let (before, after) = splitAt index xs  -- Split the list at the min element
           in case after of
                [] -> error "Unexpected pattern: after splitAt index, but list is empty."
                (_:rest) ->  -- Carry on with the remaining list
                  let newList = before ++ rest  -- Build the new list without the min element
                      (sortedTail, restComps, restSwaps) = selectionSortHelper newList (comps + length xs - 1) (swaps + 1)
                  in (minElem : sortedTail, restComps + 1, restSwaps + 1)  -- Return the sorted result
         else error "Index out of bounds in selectionSortHelper."

    -- Helper function to find the index of the minimum element
    minimumByIndex :: [Int] -> (Int, Int)
    minimumByIndex lst = go lst 0 (head lst) 0  -- Start searching for the minimum
      where
        go [] _ currentMin currentIdx = (currentIdx, currentMin)  -- Base case for recursion
        go (x:xs) idx currentMin currentIdx
          | x < currentMin = go xs (idx + 1) x idx  -- Update if a new minimum is found
          | otherwise      = go xs (idx + 1) currentMin currentIdx  -- Continue searching

-- Insertion Sort implementation
insertionSort :: [Int] -> ([Int], Int, Int)
insertionSort [] = ([], 0, 0)  -- Base case for empty list
insertionSort (x:xs) = insert x (insertionSort xs)  -- Insert the first element into the sorted list
  where
    -- Helper function to insert an element into the right position in the sorted list
    insert y (sorted, comps, swaps) = insertHelper y sorted comps swaps

    insertHelper y [] comps swaps = ([y], comps, swaps)  -- If the list is empty, just place y
    insertHelper y (z:zs) comps swaps
      | y <= z    = (y : z : zs, comps + 1, swaps)  -- If y is less than z, place y before z
      | otherwise =
          let (sortedTail, comps', swaps') = insertHelper y zs (comps + 1) swaps  -- Recur to find the spot
          in (z : sortedTail, comps', swaps')  -- Return the result

-- Merge Sort implementation
mergeSort :: [Int] -> ([Int], Int, Int)
mergeSort [] = ([], 0, 0)  -- Base case for empty list
mergeSort [x] = ([x], 0, 0)  -- Base case for one-element list
mergeSort xs = merge (mergeSort left) (mergeSort right)  -- Merge sorted halves
  where
    (left, right) = splitAt (length xs `div` 2) xs  -- Split the list into two halves

    -- Merge two sorted lists
    merge :: ([Int], Int, Int) -> ([Int], Int, Int) -> ([Int], Int, Int)
    merge (sortedLeft, compsLeft, swapsLeft) (sortedRight, compsRight, swapsRight) =
      let (mergedList, compsMerged, swapsMerged) = mergeHelper sortedLeft sortedRight (compsLeft + compsRight) (swapsLeft + swapsRight)
      in (mergedList, compsMerged, swapsMerged)

    -- Helper function for merging two sorted lists
    mergeHelper :: [Int] -> [Int] -> Int -> Int -> ([Int], Int, Int)
    mergeHelper [] ys comps swaps = (ys, comps, swaps)  -- If one list is empty, return the other
    mergeHelper xs [] comps swaps = (xs, comps, swaps)  -- If the other list is empty, return this
    mergeHelper (x:xs) (y:ys) comps swaps
      | x <= y =
          let (sortedTail, comps', swaps') = mergeHelper xs (y:ys) (comps + 1) swaps  -- Continue merging
          in (x : sortedTail, comps', swaps')
      | otherwise =
          let (sortedTail, comps', swaps') = mergeHelper (x:xs) ys (comps + 1) (swaps + 1)  -- Otherwise merge with y first
          in (y : sortedTail, comps', swaps')

-- Quick Sort implementation
quickSort :: [Int] -> ([Int], Int, Int)
quickSort [] = ([], 0, 0)  -- Base case for empty list
quickSort (p:xs) = (sortedLeft ++ [p] ++ sortedRight, compsLeft + compsRight + length xs, swapsLeft + swapsRight)
  where
    leftPartition = [x | x <- xs, x <= p]  -- Elements less than or equal to pivot
    rightPartition = [x | x <- xs, x > p]  -- Elements greater than pivot
    (sortedLeft, compsLeft, swapsLeft) = quickSort leftPartition  -- Sort left partition
    (sortedRight, compsRight, swapsRight) = quickSort rightPartition  -- Sort right partition

-- List of sorting algorithms with their complexity data
sortingAlgorithms :: [SortingAlgorithm]
sortingAlgorithms =
    [ SortingAlgorithm
        { name = "Bubble Sort"
        , sort = bubbleSort
        , timeComplexity = "O(n^2) average and worst case"  -- Complexity notation
        , spaceComplexity = "O(1)"  -- In-place sorting
        , description = "Bubble Sort is a simple comparison algorithm. It repeatedly steps through the list, compares adjacent elements and swaps them if they are in the wrong order. The pass through the list is repeated until the list is sorted."
        }
    , SortingAlgorithm
        { name = "Selection Sort"
        , sort = selectionSort
        , timeComplexity = "O(n^2) average and worst case"
        , spaceComplexity = "O(1)"
        , description = "Selection Sort is a simple comparison algorithm. It divides the input list into two parts: a sorted and an unsorted part. It repeatedly selects the smallest or largest element from the unsorted part and moves it to the end of the sorted part."
        }
    , SortingAlgorithm
        { name = "Insertion Sort"
        , sort = insertionSort
        , timeComplexity = "O(n^2) average and worst case"
        , spaceComplexity = "O(1)"
        , description = "Insertion Sort is a simple and intuitive comparison-based algorithm. It builds the final sorted array one item at a time. It is much less efficient on large lists than more advanced algorithms such as quicksort, heapsort, or merge sort."
        }
    , SortingAlgorithm
        { name = "Merge Sort"
        , sort = mergeSort
        , timeComplexity = "O(n log n) average and worst case"
        , spaceComplexity = "O(n)"
        , description = "Merge Sort is an efficient, stable, and comparison-based sorting algorithm. It divides the input array into two halves, recursively sorts them, and then merges the sorted halves."
        }
    , SortingAlgorithm
        { name = "Quick Sort"
        , sort = quickSort
        , timeComplexity = "O(n log n) average case; O(n^2) worst case"
        , spaceComplexity = "O(log n) average case; O(n) worst case"
        , description = "Quick Sort is an efficient, in-place sorting algorithm. It selects a 'pivot' element and partitions the other elements into two sub-arrays according to whether they are less than or greater than the pivot."
        }
    ]

-- Function to print sorted result with statistics
printSortedResult :: ([Int], Int, Int) -> IO ()
printSortedResult (sortedList, comparisons, swaps) = do
    putStrLn "Sorted list:"  -- Print the sorted list
    print sortedList
    putStrLn $ "Number of comparisons: " ++ show comparisons  -- Print the number of comparisons
    putStrLn $ "Number of swaps: " ++ show swaps  -- Print the number of swaps

-- Enhanced function to read a list of integers from user input
readIntList :: IO [Int]
readIntList = do
    putStrLn "Enter the list of integers separated by spaces:"  -- Prompt the user for input
    input <- getLine  -- Read the input
    let numbers = words input  -- Split input by spaces to get individual numbers
    -- Parse the input into integers and validate
    case mapM readMaybe numbers of
        Just validNumbers -> return validNumbers  -- If valid, return the list of integers
        Nothing -> do
            putStrLn "Invalid input! Please enter only integers."
            readIntList  -- Retry on invalid input

-- Enhanced function to read a positive integer from user input
readPositiveInt :: IO Int
readPositiveInt = do
    putStrLn "Enter the number of random integers to generate:"  -- Prompt for the number of random integers
    input <- getLine  -- Read the input
    case readMaybe input :: Maybe Int of
        Just n | n > 0 -> return n  -- Return the positive integer if valid
        _ -> do
            putStrLn "Please enter a valid positive integer."
            readPositiveInt  -- Retry on invalid input

-- Function to generate a list of random integers
generateRandomList :: Int -> IO [Int]
generateRandomList n = do
    gen <- newStdGen  -- Create a new random number generator
    let randomList = take n (randomRs (1, 100) gen)  -- Generate a list of n random integers between 1 and 100
    return randomList

-- Main interactive program
main :: IO ()
main = do
    putStrLn "Sort Algorithms Demonstration"  -- Introduction message

    let loop = do
            putStrLn "\nChoose a sorting algorithm:"  -- Prompt user for algorithm choice
            -- Display available sorting algorithms with proper numbering
            mapM_ (\(i, algorithm) -> putStrLn (show i ++ ". " ++ name algorithm)) $ zip [1..] sortingAlgorithms
            putStrLn "6. Generate Random List and Bubble Sort"  -- Option to generate random list
            putStrLn "7. Exit"  -- Exit option

            algorithmChoice <- getLine  -- Read user's choice

            -- Handle algorithm selection
            case algorithmChoice of
                "1" -> runSorting (sortingAlgorithms !! 0)  -- Bubble Sort
                "2" -> runSorting (sortingAlgorithms !! 1)  -- Selection Sort
                "3" -> runSorting (sortingAlgorithms !! 2)  -- Insertion Sort
                "4" -> runSorting (sortingAlgorithms !! 3)  -- Merge Sort
                "5" -> runSorting (sortingAlgorithms !! 4)  -- Quick Sort
                "6" -> do
                    n <- readPositiveInt  -- Get the number of random integers
                    randomList <- generateRandomList n  -- Generate the list
                    putStrLn $ "Generated list: " ++ show randomList  -- Display the generated list
                    putStrLn "Press Enter to sort using Bubble Sort:"  -- Wait for user confirmation
                    _ <- getLine
                    start <- getCurrentTime  -- Start timer
                    let (sorted, comps, swaps) = bubbleSort randomList  -- Sort the random list using Bubble Sort
                    end <- getCurrentTime  -- End timer
                    printSortedResult (sorted, comps, swaps)  -- Print the result
                    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)  -- Print the time taken

                "7" -> do
                    putStrLn "Thank you for using the Sort Algorithms Demonstration program. Goodbye!"  -- Exit message
                    exitSuccess  -- Terminate the program

                _ -> do
                    putStrLn "Invalid option! Please choose a valid sorting algorithm."  -- Handle invalid input

            loop  -- Continue the loop after completing the action

    loop  -- Start the interactive loop

-- Helper function to run the selected sorting algorithm
runSorting :: SortingAlgorithm -> IO ()
runSorting algorithm = do
    putStrLn $ "You chose " ++ name algorithm ++ "."  -- Display algorithm choice
    putStrLn $ "Description: " ++ description algorithm  -- Show algorithm description
    putStrLn $ "Time Complexity: " ++ timeComplexity algorithm  -- Show time complexity
    putStrLn $ "Space Complexity: " ++ spaceComplexity algorithm  -- Show space complexity
    numbers <- readIntList  -- Read the input list of integers
    start <- getCurrentTime  -- Start timer
    let (sorted, comps, swaps) = sort algorithm $ numbers  -- Sort the list
    end <- getCurrentTime  -- End timer
    printSortedResult (sorted, comps, swaps)  -- Print sorted result
    putStrLn $ "Time taken: " ++ show (diffUTCTime end start)  -- Print time taken