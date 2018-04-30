{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator (generate) where

import qualified Data.List            as List
import           Data.Monoid          ((<>))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as Text
import           Generator.Converters (Arg (..))
import qualified Generator.Converters as C
import qualified Generator.ElmLang    as EL
import qualified Generator.Templates  as T
import           Types
import qualified Utils                as U



generate :: (ContractABI, FilePath) -> Text
generate (ContractABI declarations, moduleName) =
    Text.intercalate "\n" (name <> imports <> methodsAndEvents)
    where
        name = T.moduleName $ U.getFileName moduleName
        imports = T.imports
        methodsAndEvents = concat (declarationToElm <$> List.sort declarations)



declarationToElm :: Declaration -> [Text]
declarationToElm func = concatMap (<> ["\n"]) $ filter (not . null)
    [ decComment func <> decBody func
    , decDecoder func
    , decTypeAlias func
    ]



{-

    Comment Generation

-}
decComment :: Declaration -> [Text]
decComment func@DFunction{} = EL.docComment $ C.methodSignature func <> " function"
decComment event@DEvent{}   = EL.docComment $ C.methodSignature event <> " event"
decComment _ = []



{-

    Type Alias Generation

-}
decTypeAlias :: Declaration -> [Text]
decTypeAlias DEvent { eveName, eveInputs } = EL.typeAlias (U.textUpperFirst eveName) (C.normalize eveInputs)
decTypeAlias DFunction { funName, funOutputs } =
    case funOutputs of
        [] -> []
        [_] -> []
        _   -> EL.typeAlias (U.textUpperFirst funName) (C.normalize funOutputs)
decTypeAlias _ = []



{-

    Body Generation

-}
decBody :: Declaration -> [Text]
decBody func@DFunction { funName, funOutputs, funInputs } = sig <> declaration <>  body

    where
        normalizedInputs = C.normalize funInputs

        inputParamNames =
            case funInputs of
                [] -> " contractAddress"
                _  -> " contractAddress " <> Text.intercalate " " (nameAsInput <$> normalizedInputs)

        paramRecord :: Text -> [Text]
        paramRecord = T.callBuilder
            (C.methodSignature func)
            (EL.wrapArray $ Text.intercalate ", " (C.callDataEncodings <$> normalizedInputs))

        sig = funcTypeSig func

        declaration = [ funName <> inputParamNames <> " =" ]

        body =
            U.indent 1 <$>
                case C.normalize funOutputs of
                    []  -> paramRecord "Decode.succeed ()"
                    [x] -> paramRecord $ "toElmDecoder " <> decoder x
                    _   -> paramRecord $ funName <> "Decoder"


decBody event@DEvent { eveName, eveInputs } = sig <> declaration <> body
    where
        indexedTopics = filter (\arg -> isIndexed arg) (C.normalize eveInputs)

        typeSigHelper x = "Maybe " <> elmType x <> " ->"

        sig =
            case indexedTopics of
                [] -> [ U.textLowerFirst eveName <> "Event : Address -> LogFilter" ]
                _  -> [ U.textLowerFirst eveName <> "Event : Address -> " <> (Text.unwords $ typeSigHelper <$> indexedTopics) <> " LogFilter" ]

        declaration =
            case indexedTopics of
                [] -> [ U.textLowerFirst eveName <> "Event contractAddress = " ]
                _  -> [ U.textLowerFirst eveName <> "Event contractAddress " <> (Text.intercalate " " (nameAsInput <$> indexedTopics)) <> " = " ]

        body = U.indent 1 <$> (T.logFilterBuilder $ topicsBuilder (C.methodSignature event) indexedTopics)

decBody _ = []



{-

    Decoder Generation

-}
decDecoder :: Declaration -> [Text]
{-  Function Call Decoder

    someCallDecoder : Decoder SomeCall
    someCallDecoder =
        evmDecode SomeEvent
            |> andMap address
            |> andMap uint
            |> toElmDecoder
-}
decDecoder DFunction { funName, funOutputs } =
    let
        sig = [ funName <> "Decoder : Decoder " <>  U.textUpperFirst funName ]

        declaration = [ funName <> "Decoder =" ]

        decoderPipline = toPipeLineText <$> (C.normalize funOutputs)

        toPipeLineText Arg { decoder } =
            "|> andMap " <> decoder

        body = [ U.indent 1 ("evmDecode " <> U.textUpperFirst funName) ]
                <> ( U.indent 2 <$> decoderPipline )
                <> [ U.indent 2 "|> toElmDecoder" ]

    in
        case funOutputs of
            []  -> []
            [_] -> []
            _   ->  sig <> declaration <> body

{-  Event Decoder

    someEventDecoder : Decoder SomeEvent
    someEventDecoder =
        decode SomeEvent
            |> custom (topic 1 uint)
            |> custom (data 0 address)
-}
decDecoder DEvent { eveName, eveInputs } =
    let
        sig = [ U.textLowerFirst eveName <> "Decoder : Decoder " <> U.textUpperFirst eveName ]

        declaration = [ U.textLowerFirst eveName <> "Decoder = " ]

        body =
            map (U.indent 1)
            ([ "decode " <> U.textUpperFirst eveName ] <> eventPipelineBuilder (1,0) (C.normalize eveInputs) [])

        toPipeline tipe n arg = U.indent 1 $
            "|> custom ("
                <> tipe <> " "
                <> (Text.pack $ show n)
                <> " "
                <> decoder arg
                <> ")"

        eventPipelineBuilder :: (Int, Int) -> [Arg] -> [Text] -> [Text]
        eventPipelineBuilder (topicIndex, dataIndex) args accum =
            case args of
                [] ->
                    reverse accum
                (x:xs) ->
                    case isIndexed x of
                        True ->
                            eventPipelineBuilder (topicIndex + 1, dataIndex) xs (toPipeline "topic" topicIndex x : accum)
                        False ->
                            eventPipelineBuilder (topicIndex, dataIndex + 1) xs (toPipeline "data" dataIndex x : accum)
    in
        case eveInputs of
            [] -> []
            _  -> sig <> declaration <> body

decDecoder _ = []



{-

    HELPERS

-}

-- | Generate Elm type signatures for solidity declaration (funcs, events, constructor)
funcTypeSig :: Declaration -> [Text]
funcTypeSig DFunction { funName, funInputs, funOutputs } = [ typeSig ]
    where
        typeSig = funName <> " : Address -> " <> Text.intercalate " -> " (inputs <> outputs)

        inputs = elmType <$> C.normalize funInputs

        outputs = ["Call " <> o]
            where
                o = case C.normalize funOutputs of
                    []  -> "()"
                    [x] -> elmType x
                    _   -> U.textUpperFirst funName


-- | Generates the "topics" field within a Logfilter
-- | Comprised of a list of Maybe Strings
topicsBuilder :: Text -> [Arg] -> Text
topicsBuilder sig args =
    let
        defaultTopic =
            "Just <| keccak256 " <> sig

        multiTopic xs =
            defaultTopic : (makeTopic <$> xs)

        makeTopic arg =
            "Maybe.map (Evm.encode << " <> encoding arg <> ") " <> nameAsInput arg
    in
        case args of
            [] -> EL.wrapArray defaultTopic
            xs -> EL.multiLineArray $ multiTopic xs
