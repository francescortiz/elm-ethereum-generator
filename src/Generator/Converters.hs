{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Converters where

import           Data.List.Index (indexed)
import           Data.Monoid     ((<>))
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as Text
import           Types
import           Utils           (paramAlphabet, sanitizeName, textLowerFirst)


-- TODO: Support fixed sized array types, e.g. address[3]
-- TODO: Support all bytes sizes, e.g. bytes32 or bytes5

-- | Convert Solidity type in ABI to elm-ethereum type
getElmType :: Text -> Text
getElmType "address"   = "Address"
getElmType "bool"      = "Bool"
getElmType "bytes"     = "String"
getElmType "bytes4"     = "String"
getElmType "bytes32"     = "String"
getElmType "string"    = "String"
getElmType tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "(List BigInt)"
                | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "(List Bool)"
                | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "(List Address)"
                | Text.isPrefixOf "bytes4" tipe && Text.isSuffixOf "[]" tipe = "(List String)"
                | Text.isPrefixOf "bytes32" tipe && Text.isSuffixOf "[]" tipe = "(List String)"
                | Text.isPrefixOf "uint" tipe = "BigInt"
                | Text.isPrefixOf "int256" tipe = "BigInt"
                | Text.isPrefixOf "string" tipe = "String"
                | otherwise = tipe <> "-ERROR!"


-- | Get elm decoder for solidity type
getDecoder :: Text -> Text
getDecoder "address"  = "Evm.address"
getDecoder "bool"     = "Evm.bool"
getDecoder "bytes"    = "Evm.dynamicBytes"
getDecoder "bytes4"   = "(Evm.staticBytes 4)"
getDecoder "bytes32"   = "(Evm.staticBytes 32)"
getDecoder "string"   = "Evm.string"
getDecoder tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "(Evm.dynamicArray Evm.uint)"
                | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "(Evm.dynamicArray Evm.bool)"
                | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "(Evm.dynamicArray Evm.address)"
                | Text.isPrefixOf "bytes4" tipe && Text.isSuffixOf "[]" tipe = "(Evm.dynamicArray (Evm.staticBytes 4))"
                | Text.isPrefixOf "bytes32" tipe && Text.isSuffixOf "[]" tipe = "(Evm.dynamicArray (Evm.staticBytes 32))"
                | Text.isPrefixOf "uint" tipe = "Evm.uint"
                | Text.isPrefixOf "int256" tipe = "Evm.int"
                | Text.isPrefixOf "string" tipe = "Evm.string"
                | otherwise = tipe <> "-ERROR!"


-- | Get elm enocder for solidity type
getEncodingType :: Text -> Text
getEncodingType "address"  = "AddressE"
getEncodingType "bool"     = "BoolE"
getEncodingType "bytes"    = "DBytesE"
getEncodingType "bytes4"   = "StringE"
getEncodingType "bytes32"   = "StringE"
getEncodingType "string"   = "StringE"
getEncodingType tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "(ListE UintE)"
                     | Text.isPrefixOf "int256" tipe && Text.isSuffixOf "[]" tipe = "(ListE IntE)"
                     | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "(ListE BoolE)"
                     | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "(ListE AddressE)"
                     | Text.isPrefixOf "bytes4" tipe && Text.isSuffixOf "[]" tipe = "(ListE DBytesE)"
                     | Text.isPrefixOf "bytes32" tipe && Text.isSuffixOf "[]" tipe = "(ListE DBytesE)"
                     | Text.isPrefixOf "uint" tipe = "UintE"
                     | Text.isPrefixOf "int256" tipe = "IntE"
                     | otherwise = tipe <> "-ERROR!"


-- | orders(address,bytes32) == [ AddressE a, BytesE b ]
callDataEncodings :: Arg -> Text
callDataEncodings Arg { nameAsInput, encoding } =
    encoding <> " " <>  nameAsInput


-- |    "transfer(address,uint256)"
methodSignature :: Declaration -> Text
methodSignature DFunction { funName, funInputs } =
    "\"" <> funName <> "("
    <> Text.intercalate "," (funArgType <$> funInputs)
    <> ")\""
methodSignature DEvent { eveName, eveInputs } =
    "\"" <> eveName <> "("
    <> Text.intercalate "," (eveArgType <$> eveInputs)
    <> ")\""
methodSignature _ = ""


-- |   "name : String"
outputRecord :: Arg -> Text
outputRecord Arg { nameAsOutput, elmType } =
    nameAsOutput <> " : " <> elmType


eventDecoderName :: Text -> Text
eventDecoderName t =
    textLowerFirst t <> "Decoder"


-- | The below functions/class helps normalize data for unnamed inputs/outputs
data Arg = Arg  { elmType      :: Text
                , nameAsInput  :: Text
                , nameAsOutput :: Text
                , web3Field    :: Text
                , decoder      :: Text
                , encoding     :: Text
                , isIndexed    :: Bool
                } deriving (Show, Eq, Ord)


class NormalizedArgs a where
    normalize  :: a -> [Arg]


instance NormalizedArgs [FunctionArg] where
    normalize args = map (rename . funcTuple) (indexed args)


instance NormalizedArgs [EventArg] where
    normalize args = map (rename . eventTuple) (indexed args)


funcTuple :: (Int, FunctionArg) -> (Int, (Text, Text, Bool))
funcTuple (i, FunctionArg { funArgName, funArgType }) = (i, (funArgName, funArgType, False))


eventTuple :: (Int, EventArg) -> (Int, (Text, Text, Bool))
eventTuple (i, EventArg { eveArgName, eveArgType, eveArgIndexed }) = (i, (eveArgName, eveArgType, eveArgIndexed))


rename :: (Int, (Text, Text, Bool)) -> Arg
rename (index, (argName, argType, isIndexed)) = case argName of
    "" ->  Arg type' nInput nOutput indexT decoder encoder isIndexed
    _  ->  Arg type' (sanitizeName argName) (sanitizeName argName) argName decoder encoder isIndexed
    where
        indexT  = Text.pack (show index)
        type'   = getElmType argType
        nInput  = alphabetInt index
        nOutput = "v" <> indexT
        decoder = getDecoder argType
        encoder = getEncodingType argType


alphabetInt :: Int -> Text
alphabetInt i =
    Text.singleton $ paramAlphabet !! i
