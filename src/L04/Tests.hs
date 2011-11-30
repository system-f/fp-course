module L04.Tests where

import L01.Optional
import L02.List
import L04.Fluffy
import L04.Misty

fluffyTests :: IO ()
fluffyTests =
  let showNil = show (Nil :: List Int)
      empty = Empty :: Optional Int      
      results =
        [
        -- instance Fluffy List
        ("ListFluffy1"
       , show (furry (+1) Nil)
       , showNil) ,
       -- instance Fluffy List
        ("ListFluffy2"
       , show (furry (+1) (1 :| 2 :| 3 :| Nil))
       , show (2 :| 3 :| 4 :| Nil)),
        -- instance Fluffy Optional
        ("OptionalFluffy1"
       , show (furry (+1) Empty)
       , show empty) ,
       -- instance Fluffy Optional
        ("OptionalFluffy2"
       , show (furry (+1) (Full 1))
       , show (Full 2)),
       -- instance Misty List
        ("ListMisty.banana"
       , show (banana (\n -> n :| n :| Nil) (1 :| 2 :| 3 :| Nil))
       , show (1 :| 1 :| 2 :| 2 :| 3 :| 3 :| Nil)),
       -- instance Misty List
        ("ListMisty.unicorn"
       , show (unicorn 'x' :: List Char)
       , show ('x' :| Nil)),       
       -- instance Misty Optional
        ("OptionalMisty.banana"
       , show (banana (\n -> Full (n + n)) (Full 7))
       , show (Full 14)),
       -- instance Misty Optional
        ("OptionalMisty.unicorn"
       , show (unicorn 'x' :: Optional Char)
       , show (Full 'x')),
       -- jellybean (List)
        ("jellybean1"
        , show (jellybean ((1 :| 2 :| 3 :| Nil) :| (1 :| 2 :| Nil) :| Nil))
        , show (1 :| 2 :| 3 :| 1:| 2 :| Nil)),       
       -- jellybean (Optional)
        ("jellybean2"
        , show (jellybean (Full (Full 7)))
        , show (Full 7)),       
       -- jellybean (Optional)
        ("jellybean3"
        , show (jellybean (Full empty))
        , show empty),       
       -- sausage (List)
        ("sausage1"        
        , show (sausage [1 :| 2 :| 3 :| Nil, 1 :| 2 :| Nil])
        , show ([1, 1] :| [1, 2] :| [2, 1] :| [2, 2] :| [3, 1] :| [3, 2] :| Nil)),      
       -- sausage (Optional)
        ("sausage2"        
        , show (sausage [Full 7, Full 8])
        , show (Full [7, 8])),       
       -- sausage (Optional)
        ("sausage3"        
        , show (sausage [Full 7, Empty])
        , show empty),       
       -- moppy (List)
        ("moppy1"        
        , show (moppy id [1 :| 2 :| 3 :| Nil, 1 :| 2 :| Nil])
        , show ([1, 1] :| [1, 2] :| [2, 1] :| [2, 2] :| [3, 1] :| [3, 2] :| Nil)),      
       -- moppy (Optional)
        ("moppy2"        
        , show (moppy id [Full 7, Full 8])
        , show (Full [7, 8])),       
       -- moppy (Optional)
        ("moppy3"        
        , show (moppy id [Full 7, Empty])
        , show empty)       
        ]
      check (n, a, b) = do print ("=== " ++ n ++ " ===")
                           print (if a == b then "PASS" else "FAIL Expected: " ++ b ++ " Actual: " ++ a)

  in mapM_ check results


