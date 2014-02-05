

--
--
--
--


validateAge :: Int -> Maybe Int
validateAge n =
  if (n > 200)
  then 
    Nothing
  else
    Just n


validateName :: String -> Maybe String
validateName n = 
  if null n
  then 
    Nothing
  else
    Just n


data Email = Email String String deriving (Eq, Show)

validateEmail :: String -> Maybe Email
validateEmail email = 
  if null email
  then
    Nothing
  else
    Just (Email "fred" email)

data Person = Person String Int Email deriving (Eq, Show)

validatePerson :: String -> Int -> String -> Maybe Person
validatePerson name age email = 
  do name'  <- validateName name
     age'   <- validateAge age
     email' <- validateEmail email
     validatePerson' name' age' email'

validatePerson' :: String -> Int -> Email -> Maybe Person
validatePerson' n a e =
  Just (Person n a e)


liftM3 :: 
  Monad f => 
   (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftM3 g x y z = 
  do x' <- x
     y' <- y
     z' <- z
     return (g x' y' z')

{--
  validateName name >>= (\name' ->
  validateAge age >>= (\age' ->
  validateEmail email >>= (\email' ->
    return (Person name' age' email'))))
--}


data Hole = Hole


