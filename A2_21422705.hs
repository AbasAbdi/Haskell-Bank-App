-- data types
type ID = Int
type Fnumber = Int -- Receiver
type Tnumber = Int -- Sender
type Rate = Float
data TransacType = Deposit | Withdrawal | Transfer | Receipt | Invalid deriving (Show,Eq) -- deriving show so type can be easily displayed and eq so that we can check for equality
data Currency = Pound | Dollar | Euro | Error deriving (Show,Eq) -- deriving show so type can be easily displayed and eq so that we can check for equality
type Reason = String
type Value = Float
type HiddenValue = Float
type Transaction = (ID, TransacType, Currency, Rate, Fnumber, Tnumber, Reason, Value, HiddenValue)
type TransacList = [Transaction]

-- Initialising an empty list
myList :: TransacList
myList = []

-- "ui": part of main input processing; prompts user for a command, then hands command and Transaction list to handle function.
-- input: TransacList (data to operate on)
ui :: TransacList -> IO ()
ui ts = do
 putStrLn "What would you like to do? [quit, add, cancel, list, search, filter, balance]"
 cmd <- getLine
 handle cmd ts

-- Entry point into the application
main = ui myList

-- "handle": part of main input processing; uses pattern matching to handle input commands
-- input: String (command), TransacList (data to operate on)
handle :: String -> TransacList -> IO ()
handle "quit" ts = do
 putStrLn "Goodbye!"
handle "list" ts = do
 displayList ts
 putStrLn "=============================================================="
 ui ts
handle "cancel" ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts) = do
 putStrLn "Please enter most recent transaction ID: "
 getId <- getLine
 let newID = read getId :: Int
 if ((show index) == (show newID)) then do 
  if ((show transactype) == (show Deposit)) then do
   ui (((length ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts)) + 1, Withdrawal, currency, rate, number, tnumber, "Correction", value, -hiddenvalue) : ((index), transactype, currency, rate, number, tnumber, "Reversed", value, hiddenvalue) : ts )
  else if ((show transactype) == (show Withdrawal)) then do 
   ui (((length ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts)) + 1, Deposit, currency, rate, number, tnumber, "Correction", value, abs(hiddenvalue)): ((index), transactype, currency, rate, number, tnumber, "Reversed", value, hiddenvalue) : ts)
  else if ((show transactype) == (show Transfer)) then do 
   ui (((length ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts)) + 1, Receipt, currency, rate, number, tnumber, "Correction", value, abs(hiddenvalue)): ((index), transactype, currency, rate, number, tnumber, "Reversed", value, hiddenvalue): ts)
  else if ((show transactype) == (show Receipt)) then do 
   ui (((length ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts)) + 1, Transfer, currency, rate, number, tnumber, "Correction", value, -hiddenvalue):   ((index), transactype, currency, rate, number, tnumber, "Reversed", value, hiddenvalue): ts)
  else ui ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts)
 else ui ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts)
handle "search" ts = do
 putStrLn "Type S for Sender | R for Receiver | B for Both ? "
 choice <- getLine
 if (choice == "r" ||choice == "R" || choice =="Receiver" || choice =="receiver") then do
  putStrLn "Please enter the account number:"
  account <- getLine
  let newaccount = read account :: Int
  displaySearch ts newaccount 'r'
  putStrLn "=============================================================="
  ui ts
 else if (choice == "s" ||choice == "S" || choice =="Sender" || choice =="sender") then do
  putStrLn "Please enter the account number:"
  account <- getLine
  let newaccount = read account :: Int
  displaySearch ts newaccount 's'
  putStrLn "=============================================================="
  ui ts
 else if (choice == "b" ||choice == "B" || choice =="Both" || choice =="both") then do
  putStrLn "Please enter the account number:"
  account <- getLine
  let newaccount = read account :: Int
  displaySearch ts newaccount 'b'
  putStrLn "=============================================================="
  ui ts
 else handle "search" ts
handle "filter" ts = do
 putStrLn "Type W for Withdrawal | D for Deposit | T for Transfer | R for Receipt or leave blank "
 transac <- getLine
 let newTransac = stringToTransacType transac
 putStrLn "Please select a currency Pound | Dollar | Euro or leave blank"
 curren <- getLine
 let newCurrency = stringToCurrency curren
 putStrLn "Please enter a value or leave blank "
 interimValue1 <- getLine
 if ((show interimValue1) /= (show "")) then do
  let interimValue2 = read interimValue1 :: Float
  displayFilter ts newTransac newCurrency interimValue2
 else displayFilter ts newTransac newCurrency 0.00
 putStrLn "=============================================================="
 ui ts
handle "Main" ts = do
 ui ts
handle "add" ts = do
 putStrLn "Type W for Withdrawal | D for Deposit | T for Transfer | R for Receipt | M for Main?"
 interimTransacType <- getLine
 let newTransacType = stringToTransacType interimTransacType
 if (interimTransacType == "m" ||interimTransacType == "M" || interimTransacType =="main" || interimTransacType =="Main") then 
  handle "Main" ts
 else if ((show newTransacType) == (show Invalid)) then 
  handle "add" ts
 else putStrLn "Please select a currency Pound | Dollar | Euro | M for Main?"
 interimCurrency <- getLine
 let newCurrency = stringToCurrency interimCurrency
 if (interimCurrency == "m" ||interimCurrency == "M" || interimCurrency =="main" || interimCurrency =="Main") then 
  handle "Main" ts
 else if ((show newCurrency) == (show Error)) then 
  handle "add" ts
 else putStrLn "Please enter a value (or M for Main): "
 interimValue1 <- getLine
 if (interimValue1 == "m" ||interimValue1 == "M" || interimValue1 =="main" || interimValue1 =="Main") then 
  handle "Main" ts
 else if (interimValue1 == "") then
  handle "add" ts
 else do
 let interimValue2 = read interimValue1 :: Float
 putStrLn "Please enter an exchange rate or 0 to use default (or M for Main): "
 rate <- getLine
 if (rate == "m" || rate == "M" || rate == "main" || rate == "Main") then 
  handle "Main" ts
 else if (rate == "") then 
  handle "add" ts
 else do
 let newRate = read rate :: Float
 let newValue = assignValue newTransacType newCurrency newRate interimValue2
 putStrLn "Please enter a reason for this transaction or leave blank (or M for Main):"
 reas <- getLine
 if (reas == "m" || reas == "M" || reas == "main" || reas == "Main") then 
  handle "Main" ts
 else if (reas == "Reversed") then do
  handle "add" ts
 else if (more newTransacType) then do
  putStrLn "Please enter the receivers account number (or M for Main): "
  strnum <- getLine
  if (strnum == "m" || strnum == "M" || strnum == "main" || strnum == "Main") then 
   handle "Main" ts
  else if (strnum == "") then 
   handle "add" ts
  else do
  let tNum = read strnum :: Int
  ui (((length ts) + 1, newTransacType, newCurrency, newRate, 0, tNum, reas, interimValue2, newValue): ts)
 else if (more2 newTransacType) then do
  putStrLn "Please enter the payees account number (or M for Main): "
  newNum <- getLine
  if (newNum == "m" || newNum == "M" || newNum == "main" || newNum == "Main") then 
   handle "Main" ts
  else if (newNum == "") then 
   handle "add" ts
  else do
  let newNumber = read newNum :: Int
  ui (((length ts) + 1, newTransacType, newCurrency, newRate, newNumber, 0, reas, interimValue2, newValue): ts)
 else ui (((length ts) + 1, newTransacType, newCurrency, newRate, 0, 0, reas, interimValue2, newValue): ts)
handle "balance" ts = do
 putStrLn ("Total Balance: £"++show (calcbalance ts))
 putStrLn ("Average Transaction: £"++show (calcaverage ts))
 ui ts
handle x ts = do
 putStrLn (x ++ " is not a valid input.")
 ui ts
 
-- "assignValue" takes the TransacType, Currency, and two floats to determin the correct value 
-- input: TransacType, Currency, Float, and Float
-- output: Float
assignValue :: TransacType -> Currency -> Float -> Float -> Float
assignValue Deposit Pound r x = x
assignValue Deposit Dollar r x = if (r > 0) then do x/r else x / 1.42
assignValue Deposit Euro r x = if (r > 0) then do x/r else x / 1.16
assignValue Withdrawal Pound r x = (-x)
assignValue Withdrawal Dollar r x = if (r > 0) then do (-x)/r else  (-x) / 1.42
assignValue Withdrawal Euro r x = if (r > 0) then do (-x)/r else  (-x) / 1.16
assignValue Transfer Pound r x =  (-x)
assignValue Transfer Dollar r x = if (r > 0) then do (-x)/r else  (-x) / 1.42
assignValue Transfer Euro r x = if (r > 0) then do (-x)/r else  (-x) / 1.16
assignValue Receipt Pound r x = x
assignValue Receipt Dollar r x = if (r > 0) then do x/r else  x / 1.42
assignValue Receipt Euro r x = if (r > 0) then do x/r else  x / 1.16

-- "more" takes TransacType and outputs a boolean 
-- input: TransacType
-- output: Bool
more :: TransacType ->Bool
more Transfer = True
more _ = False

-- "more2" takes TransacType and outputs a boolean 
-- input: TransacType
-- output: Bool
more2 :: TransacType ->Bool
more2 Receipt = True
more2 _ = False



-- "displayList" prints Transaction list on the screen
-- input: TransacList 
displayList :: TransacList -> IO ()
displayList [] = do
 putStrLn "List of Transactions:"
 putStrLn "=============================================================="
displayList ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts) = do
 displayList ts
 if tnumber == 0 && number == 0 && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency) ++ " " ++ (show reason))
 else if tnumber == 0 && number == 0 && reason == "" then do
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency))
 else if number == 0 && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++ " " ++ (show currency) ++ " " ++ (show reason)++ " Receiver: " ++ (show tnumber))
 else if number == 0 && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++ " " ++ (show currency) ++ " Receiver: " ++ (show tnumber))
 else if number /= 0 && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++  " " ++ (show currency)++ " " ++ (show reason)++ " Sender: " ++ (show number))
 else
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++  " " ++ (show currency)++ " Sender: " ++ (show number))

-- "displaySearch" takes a TransacList and an int containing the account number before printing Transaction list on the screen
-- input: TransacList, Int and Char
displaySearch :: TransacList -> Int -> Char -> IO ()
displaySearch [] _ _ = do
 putStrLn "List of Transactions:"
 putStrLn "=============================================================="
displaySearch ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts) x y = do
 displaySearch ts x y
 if (y== 's' && tnumber == x) && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency) ++ " " ++ (show reason))
 else if (y== 's' && tnumber == x) && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency))
 else if (y== 'r' && number == x) && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency) ++ " " ++ (show reason))
 else if (y== 'r' && number == x) && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency))
 else if (y== 'b' && (tnumber == x || number == x)) && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency) ++ " " ++ (show reason))
 else if (y== 'b' && (tnumber == x || number == x))  && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency))
 else
  putStr ""
  

-- "displayFilter" takes a TransacList, TransacType, Currency and a float before printing Transaction list on the screen
-- input: TransacList, TransacType, Currency and Float
displayFilter :: TransacList -> TransacType -> Currency -> Float -> IO ()
displayFilter [] _ _ _ = do
 putStrLn "List of Transactions:"
 putStrLn "=============================================================="
displayFilter ((index, transactype, currency, rate, number, tnumber, reason, value, hiddenvalue):ts) x z y = do
 displayFilter ts x z y
 if (((show transactype) == (show x) && (show currency) == (show z) && (show value) == (show y)) || ((show transactype) == (show x) && (show currency) == (show z) && (show y) == (show 0.00)) ||      ((show transactype) == (show x) && (show z) == (show Error) && (show value) == (show y)) || ((show transactype) == (show x) && (show z) == (show Error) &&  (show y) == (show 0.00))|| ((show x) == (show Invalid) && (show currency) == (show z) && (show value) == (show y)) || ((show x) == (show Invalid) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show x) == (show Invalid) && (show z) == (show Error) && (show value) == (show y))) && tnumber == 0 && number == 0 && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency) ++ " " ++ (show reason))
 else if (((show transactype) == (show x) && (show currency) == (show z) && (show value) == (show y)) || ((show transactype) == (show x) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show transactype) == (show x) && (show z) == (show Error) && (show value) == (show y)) || ((show transactype) == (show x) && (show z) == (show Error) &&  (show y) == (show 0.00))|| ((show x) == (show Invalid) && (show currency) == (show z) && (show value) == (show y)) || ((show x) == (show Invalid) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show x) == (show Invalid) && (show z) == (show Error) && (show value) == (show y))) && tnumber == 0 && number == 0 && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++  " " ++ (show value) ++ " " ++ (show currency))
 else if (((show transactype) == (show x) && (show currency) == (show z) && (show value) == (show y)) || ((show transactype) == (show x) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show transactype) == (show x) && (show z) == (show Error) && (show value) == (show y)) || ((show transactype) == (show x) && (show z) == (show Error) &&  (show y) == (show 0.00))|| ((show x) == (show Invalid) && (show currency) == (show z) && (show value) == (show y)) || ((show x) == (show Invalid) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show x) == (show Invalid) && (show z) == (show Error) && (show value) == (show y))) && number == 0 && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++ " " ++ (show currency) ++ " " ++ (show reason)++ " Receiver: " ++ (show tnumber))
 else if (((show transactype) == (show x) && (show currency) == (show z) && (show value) == (show y)) || ((show transactype) == (show x) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show transactype) == (show x) && (show z) == (show Error) && (show value) == (show y)) || ((show transactype) == (show x) && (show z) == (show Error) &&  (show y) == (show 0.00))|| ((show x) == (show Invalid) && (show currency) == (show z) && (show value) == (show y)) || ((show x) == (show Invalid) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show x) == (show Invalid) && (show z) == (show Error) && (show value) == (show y))) && number == 0 && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++ " " ++ (show currency) ++ " Receiver: " ++ (show tnumber))
 else if (((show transactype) == (show x) && (show currency) == (show z) && (show value) == (show y)) || ((show transactype) == (show x) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show transactype) == (show x) && (show z) == (show Error) && (show value) == (show y)) || ((show transactype) == (show x) && (show z) == (show Error) &&  (show y) == (show 0.00))|| ((show x) == (show Invalid) && (show currency) == (show z) && (show value) == (show y)) || ((show x) == (show Invalid) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show x) == (show Invalid) && (show z) == (show Error) && (show value) == (show y))) && number /= 0 && reason /= "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++  " " ++ (show currency)++ " " ++ (show reason)++ " Sender: " ++ (show number))
 else if (((show transactype) == (show x) && (show currency) == (show z) && (show value) == (show y)) || ((show transactype) == (show x) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show transactype) == (show x) && (show z) == (show Error) && (show value) == (show y)) || ((show transactype) == (show x) && (show z) == (show Error) &&  (show y) == (show 0.00))|| ((show x) == (show Invalid) && (show currency) == (show z) && (show value) == (show y)) || ((show x) == (show Invalid) && (show currency) == (show z) && (show y) == (show 0.00)) || ((show x) == (show Invalid) && (show z) == (show Error) && (show value) == (show y))) && number /= 0 && reason == "" then 
  putStrLn ("Transaction "++ (show index) ++ ":    " ++ (show transactype) ++ " " ++ (show value)++  " " ++ (show currency)++ " Sender: " ++ (show number))
 else
  putStr ""

-- "stringToTransacList" converts a string into a TransacType value
-- input: String
-- output: TransacType
stringToTransacType :: String -> TransacType
stringToTransacType "deposit" = Deposit
stringToTransacType "d" = Deposit
stringToTransacType "Deposit" = Deposit
stringToTransacType "D" = Deposit
stringToTransacType "withdrawal" = Withdrawal
stringToTransacType "w" = Withdrawal
stringToTransacType "Withdrawal" = Withdrawal
stringToTransacType "W" = Withdrawal
stringToTransacType "transfer" = Transfer
stringToTransacType "t" = Transfer
stringToTransacType "Transfer" = Transfer
stringToTransacType "T" = Transfer
stringToTransacType "receipt" = Receipt
stringToTransacType "r" = Receipt
stringToTransacType "Receipt" = Receipt
stringToTransacType "R" = Receipt
stringToTransacType _ = Invalid

-- "stringToCurrency" converts a string into a Currency value
-- input: String
-- output: Currency
stringToCurrency :: String -> Currency
stringToCurrency "pound" = Pound
stringToCurrency "p" = Pound
stringToCurrency "Pound" = Pound
stringToCurrency "P" = Pound
stringToCurrency "dollar" = Dollar
stringToCurrency "d" = Dollar
stringToCurrency "Dollar" = Dollar
stringToCurrency "D" = Dollar
stringToCurrency "euro" = Euro
stringToCurrency "e" = Euro
stringToCurrency "Euro" = Euro
stringToCurrency "E" = Euro
stringToCurrency _ = Error


-- "calcbalance" balances up the hidden values in a Transaction list
-- input: TransacList
-- output: Float (balance of hidden values in list of Transaction)
calcbalance :: TransacList -> Float
calcbalance [] = 0.00
calcbalance ((_, _, _, _, _, _, _, _, v):ts) =  v + calcbalance ts 

-- "calcaverage" calculates the average of the sum of values in a Transaction list
-- input: TransacList
-- output: Float (average of values in list of Transaction)
calcaverage :: TransacList -> Float
calcaverage [] = 0.00
calcaverage list = calcbalance list / numValues
  where
    numValues = fromIntegral (length list) :: Float -- parse the int to float for length of list