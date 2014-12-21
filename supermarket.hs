--Sean Stasny
--CSci_450
--Supermarket.hs
------------------------------------------------------------------
-- Extended exercise: supermarket billing
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Types of names, prices (pence) and bar-codes.

type Name    = String
type Price   = Int
type BarCode = Int

-- The database linking names prices and bar codes.

type Database = [ (BarCode,Name,Price) ]

-- The example database we use is

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

-- The lists of bar codes, and of Name,Price pairs.

type TillType = [BarCode]
type BillType = [(Name,Price)]

-- The length of a line in the bill.

lineLength :: Int
lineLength = 30

--------------------------------------------------------------------------------------------------------
formatCents :: Price -> String

formatCents n =
        let dollars = n `div` 100
            cents   = n `mod` 100
            in show dollars ++ "." ++ (fixCents . show) cents
--------------------------------------------------------------------------------------------------------
fixCents :: String -> String
fixCents xs
    | length xs <2 = '0':xs
    | otherwise    = xs
--------------------------------------------------------------------------------------------------------
formatLine :: (Name,Price) -> String

formatLine (xs,y) =
        let amount       = formatCents y
            amountLen    = length amount
            nameLen      = length xs
            amountOfDots = lineLength - (amountLen + nameLen)
            dot          =  "."
            printDots    = concat $ replicate amountOfDots dot
            newLine      = "\n"
            in  id(xs ++ printDots ++ amount++ newLine)
--------------------------------------------------------------------------------------------------------
formatLine' :: (Name,Price) -> String

formatLine' (xs,y) =
        let amount       = formatCents y
            amountLen    = length amount
            nameLen      = length xs
            amountOfDots = (lineLength+1) - (amountLen + nameLen)
            dot          =  "."
            printDots    = concat $ replicate amountOfDots dot
            in  id(xs ++ printDots ++ amount)
--------------------------------------------------------------------------------------------------------
formatLines :: [(Name, Price)] -> String

formatLines [] = []
formatLines ((xs,y):xss) = 
        let first = formatLine((xs,y))
            rest  = formatLines(xss)
            in first ++ rest
--------------------------------------------------------------------------------------------------------
makeTotal :: BillType -> Price

makeTotal [] = 0
makeTotal ((xs,y):xss) = 
        let getNum = y + makeTotal(xss)
            in id getNum
--------------------------------------------------------------------------------------------------------
formatTotal :: Price -> String
    
formatTotal n = formatLine' ("\nTotal", n)
---------------------------------------------------------------------------------------------------------
formatBill :: BillType -> String

formatBill [] = []
formatBill ((xs,y):xss) = 
        let formatLn  = formatLines ((xs,y):xss)
            getTotal = makeTotal ((xs,y):xss)
            total = formatTotal getTotal
            in formatLn ++ total
---------------------------------------------------------------------------------------------------------
look :: Database -> BarCode -> (Name, Price)

look dataBase barCode
 | null hit = ("Unknown Item", 0)
 | otherwise = head hit
      where hit = [(n, p) | (b, n, p) <-dataBase, b == barCode] 
---------------------------------------------------------------------------------------------------------
lookup' :: BarCode -> (Name, Price)

lookup' x = look codeIndex x
---------------------------------------------------------------------------------------------------------
makeBill :: TillType -> BillType

makeBill tillType = [lookup' item | item <-tillType]
---------------------------------------------------------------------------------------------------------          
