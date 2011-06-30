module L01.Validation where

type Err = String

--  class Validation<A> {
--    Validation(String error) {} // Error
--    Validation(A value) {} // Value
--  }
data Validation a = Error Err | Value a
  deriving (Eq, Show)

isError :: Validation a -> Bool
isError (Error _) = True
isError (Value _) = False

isValue :: Validation a -> Bool
isValue = not . isError

mapValidation :: (a -> b) -> Validation a -> Validation b
mapValidation _ (Error s) = Error s
mapValidation f (Value a) = Value (f a)

bindValidation :: Validation a -> (a -> Validation b) -> Validation b
bindValidation (Error s) _ = Error s
bindValidation (Value a) f = f a

valueOr :: Validation a -> a -> a
valueOr (Error _) a = a
valueOr (Value a) _ = a

errorOr :: Validation a -> Err -> Err
errorOr (Error e) _ = e
errorOr (Value _) a = a

valueValidation :: a -> Validation a
valueValidation = Value

