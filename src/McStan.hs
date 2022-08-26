-- TODO Rename into proper module structure
-- TODO Use this GHC2021 extension or whatever its called
-- TODO Use Werror with dev flag
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module McStan where

-- TODO It is silly to produce Stan code just to let it be compiled again. Rather, I should FFI into the Stan library!

-- base
import Data.List.NonEmpty

-- text
import Data.Text
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

-- transformers
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (partitionEithers)
import Control.Arrow (second)

data StanType
  -- TODO bounds
  = Array (NonEmpty Int)
  | Integer_
  | Real

data Data a where
  ArrayData :: Name -> Data ('Array shape)
  RealData :: Name -> Double -> Data 'Real

-- TODO Num,
-- TODO Data, transformed data

-- TODO no such thing as data themselves in Stan
data TransformedData a where
  SimplyData :: Data a -> TransformedData a
  (:+:) :: TransformedData a -> TransformedData a -> TransformedData a

data SomeData where
  SomeData :: Data a -> SomeData

data Distribution a where
  Normal :: StanValue 'Real -> StanValue 'Real -> Distribution 'Real

data ParameterDeclaration a where
  -- TODO Can I get away without these singletons, and instead type level tags using @?
  RealParameter :: Name -> ParameterDeclaration 'Real

data Name = Name
  { name :: Text
  , comment :: Maybe Text
  }

toText :: Name -> Text
toText Name { name, comment } = name <> maybe "" (" // " <>) comment

data TransformedParameter a where
  SimplyParameter :: ParameterDeclaration a -> TransformedParameter a
  -- TODO Put in separate file so no name clash
  (:+::) :: TransformedParameter a -> TransformedParameter a -> TransformedParameter a

instance Num (TransformedParameter a) where
  (+) = (:+::)
  -- TODO Further operators

-- TODO Naming
makeName :: TransformedParameter a -> Text
makeName (SimplyParameter (RealParameter Name { name })) = name
-- TODO Trouble: If two TPs have the same formula, they'll get the same name. I still need an accumulation for an increasing index
makeName (parameter1 :+:: parameter2) = "___PLUS_" <> makeName parameter1 <> "_" <> makeName parameter2

-- TODO Naming
-- TODO this is the same kind of recursion like the name? Maybe this can be jointly refactored?
synthesizeExpression :: TransformedParameter a -> Text
synthesizeExpression (SimplyParameter (RealParameter Name { name })) = name
synthesizeExpression (parameter1 :+:: parameter2) = "(" <> synthesizeExpression parameter1 <> ") + (" <> synthesizeExpression parameter2 <> ")"

data SomeParameter where
  SomeParameterDeclaration :: ParameterDeclaration a -> SomeParameter
  SomeTransformedParameter :: TransformedParameter a -> SomeParameter

onSomeParameter :: (forall a . ParameterDeclaration a -> b) -> (forall a . TransformedParameter a -> c) -> SomeParameter -> Either b c
onSomeParameter onParameterDeclaration _onTransformedParameter (SomeParameterDeclaration parameterDeclaration) = Left $ onParameterDeclaration parameterDeclaration
onSomeParameter _onParameterDeclaration onTransformedParameter (SomeTransformedParameter transformedParameter) = Right $ onTransformedParameter transformedParameter

onSomeParameters :: (forall a . ParameterDeclaration a -> b) -> (forall a . TransformedParameter a -> c) -> [SomeParameter] -> ([b], [c])
onSomeParameters onParameterDeclaration onTransformedParameter = partitionEithers . fmap (onSomeParameter onParameterDeclaration onTransformedParameter)

-- TODO Number literals (fromInteger, fractionals?)
data StanValue a
  = ValueData (Data a)
  | ValueParameter (ParameterDeclaration a)
  | ValueTransformedParameter (TransformedParameter a)

-- TODO Can throw an error when used literals in binding position?
getValueName :: StanValue a -> Text
getValueName (ValueData (RealData name _value)) = toText name
getValueName _ = error "not implemented"

instance Num (StanValue a) where
  ValueParameter parameterDeclaration + value = ValueTransformedParameter (SimplyParameter parameterDeclaration) + value
  value + ValueParameter parameterDeclaration = value + ValueTransformedParameter (SimplyParameter parameterDeclaration)
  ValueTransformedParameter transformedParameter1 + ValueTransformedParameter transformedParameter2 = ValueTransformedParameter $ transformedParameter1 + transformedParameter2
  -- TODO Missing cases
  -- TODO Further operators

data LikelihoodFactor where
  LikelihoodFactor :: StanValue a -> Distribution a -> LikelihoodFactor

data StanModel = StanModel
  { data_ :: [SomeData]
  , parameters :: [SomeParameter]
  , likelihoodFactors :: [LikelihoodFactor]
  }

instance Semigroup StanModel where
  model1 <> model2 = StanModel
    { data_ = data_ model1 <> data_ model2
    , parameters = parameters model1 <> parameters model2
    , likelihoodFactors = likelihoodFactors model1 <> likelihoodFactors model2
    }

instance Monoid StanModel where
  mempty = StanModel
    { data_ = []
    , parameters = []
    , likelihoodFactors = []
    }

data StanDeclarationError
  = DoubleDataDeclaration
  deriving Show

-- Naming: How will we name the monad where we execute the backend then?
newtype StanT m a = StanT
  { getStanT :: WriterT StanModel (ExceptT StanDeclarationError m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- TODO Don't like naming
realP :: Monad m => Text -> StanT m (StanValue 'Real)
realP name = StanT $ do
  let parameterDeclaration_ = RealParameter $ Name name Nothing
  tell mempty { parameters = [SomeParameterDeclaration parameterDeclaration_] }
  return $ ValueParameter parameterDeclaration_

realD :: Monad m => Text -> Double -> StanT m (StanValue 'Real)
realD name value = StanT $ do
  let data_ = RealData Name { name, comment = Nothing } value
  tell mempty { data_ = [SomeData data_]}
  return $ ValueData data_

(<~) :: Monad m => StanValue a -> Distribution a -> StanT m ()
value <~ distribution = StanT $ tell mempty { likelihoodFactors = [LikelihoodFactor value distribution]}

-- TODO Could also use an Accum and put that in there
validate :: StanModel -> Maybe StanDeclarationError
-- TODO Errors: Double names,
validate = const Nothing

compileModel :: StanModel -> Text
compileModel StanModel { data_, parameters, likelihoodFactors } = Text.unlines $
  [ compileDatas data_
  , compileParameters parameters
  , compileLikelihoodFactors likelihoodFactors
  ]

compileSomeData :: SomeData -> Text
compileSomeData (SomeData (RealData name _value)) = "real " <> toText name <> ";"
compileSomeData _ = error "compileSomeData: Not implemented yet"

compileDatas :: [SomeData] -> Text
compileDatas datas = Text.unlines $
  [ "data {" ]
  ++ (compileSomeData <$> datas)
  ++ [ "}" ]

compileParameterDeclaration :: ParameterDeclaration a -> Text
compileParameterDeclaration (RealParameter name) = "real " <> toText name <> ";"

data CompiledTransformedParameter = CompiledTransformedParameter
  { declaration :: Text
  , definition :: Text
  }

compileTransformedParameter :: TransformedParameter a -> Maybe CompiledTransformedParameter
compileTransformedParameter (SimplyParameter parameterDeclaration) = Nothing
compileTransformedParameter transformedParameter@(_transformedParameter1 :+:: _transformedParameter2) = Just CompiledTransformedParameter
  -- TODO How do I get the type information in here? Type classes? (But then I don't need the singletons anymore) Or singletons as well?
  -- TODO Duplication with compileParameterDeclaration
  { declaration = "real " <> makeName transformedParameter <> ";"
  , definition = synthesizeExpression transformedParameter
  }

-- TODO Still need the case where a tp is added to the model when calling a distribution on it

compileParameters :: [SomeParameter] -> Text
compileParameters parameters = let
    (parameterDeclarations, transformedParameters) = second catMaybes $ onSomeParameters compileParameterDeclaration compileTransformedParameter parameters
    transformedDeclarations = declaration <$> transformedParameters
    transformedDefinitions = definition <$> transformedParameters
  in Text.unlines $
    [ "parameters {" ]
    -- TODO indentation
    ++ parameterDeclarations
    ++
    [ "}"
    , ""
    , "transformed parameters {"
    ]
    ++ transformedDeclarations
    ++ transformedDefinitions
    ++ ["}"]

-- TODO I'm losing comments here. Maybe they should be dropped from Name?
compileValue :: StanValue a -> Text
compileValue (ValueData (RealData name _value)) = toText name
compileValue (ValueParameter (RealParameter name)) = toText name
compileValue _ = error "compileValue: Not implemented"

compileDistribution :: Distribution a -> Text
compileDistribution (Normal mu sigma) = "normal(" <> compileValue mu <> ", " <> compileValue sigma <> ")"

compileLikelihoodFactor :: LikelihoodFactor -> Text
compileLikelihoodFactor (LikelihoodFactor value distribution) = getValueName value <> " ~ " <> compileDistribution distribution <> ";"

compileLikelihoodFactors :: [LikelihoodFactor] -> Text
compileLikelihoodFactors likelihoodFactors = Text.unlines $
  [ "model {"]
  ++ (compileLikelihoodFactor <$> likelihoodFactors)
  ++ [ "}" ]

runStanT :: Functor m => StanT m a -> m (Either StanDeclarationError (a, Text))
runStanT = fmap (fmap (fmap compileModel)) . runExceptT . runWriterT . getStanT

printStan :: MonadIO m => StanT m () -> m ()
printStan stan = do
  result <- runStanT stan
  liftIO $ case result of
    Left e -> print e
    Right ((), code) -> TextIO.putStrLn code
