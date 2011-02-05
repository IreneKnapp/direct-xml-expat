{-# LANGUAGE GADTs #-}
module Text.XML.Expat.SAX (
                           module Data.XML.Types,
                           Parser,
                           newParser,
                           Callback,
                           setCallback,
                           clearCallback,
                           parsedBeginDocument,
                           parsedEndDocument,
                           parsedBeginElement,
                           parsedEndElement,
                           parsedCharacters,
                           parsedComment,
                           parsedInstruction,
                           parsedDoctype,
                           parseBytes,
                           parseComplete
                          )
  where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.IORef
import Data.XML.Types


data Parser = Parser {
    parserErrorHandler :: String -> IO (),
    parserFilename :: Maybe String,
    parserInputBuffer :: IORef ByteString,
    parserBeginDocumentCallback
      :: IORef (Maybe (IO Bool)),
    parserEndDocumentCallback
      :: IORef (Maybe (IO Bool)),
    parserBeginElementCallback
      :: IORef (Maybe (Name -> [Attribute] -> IO Bool)),
    parserEndElementCallback
      :: IORef (Maybe (Name -> IO Bool)),
    parserCharactersCallback
      :: IORef (Maybe (String -> IO Bool)),
    parserCommentCallback
      :: IORef (Maybe (String -> IO Bool)),
    parserInstructionCallback
      :: IORef (Maybe (Instruction -> IO Bool)),
    parserDoctypeCallback
      :: IORef (Maybe (Doctype -> IO Bool))
  }


data Callback a where
  CallbackBeginDocument :: Callback (IO Bool)
  CallbackEndDocument :: Callback (IO Bool)
  CallbackBeginElement :: Callback (Name -> [Attribute] -> IO Bool)
  CallbackEndElement :: Callback (Name -> IO Bool)
  CallbackCharacters :: Callback (String -> IO Bool)
  CallbackComment :: Callback (String -> IO Bool)
  CallbackInstruction :: Callback (Instruction -> IO Bool)
  CallbackDoctype :: Callback (Doctype -> IO Bool)


newParser :: (String -> IO ())
          -> Maybe String
          -> IO Parser
newParser errorHandler maybeFilename = do
  inputBufferIORef <- newIORef BS.empty
  beginDocumentCallbackIORef <- newIORef Nothing
  endDocumentCallbackIORef <- newIORef Nothing
  beginElementCallbackIORef <- newIORef Nothing
  endElementCallbackIORef <- newIORef Nothing
  charactersCallbackIORef <- newIORef Nothing
  commentCallbackIORef <- newIORef Nothing
  instructionCallbackIORef <- newIORef Nothing
  doctypeCallbackIORef <- newIORef Nothing
  return Parser {
             parserErrorHandler = errorHandler,
             parserFilename = maybeFilename,
             parserInputBuffer = inputBufferIORef,
             parserBeginDocumentCallback = beginDocumentCallbackIORef,
             parserEndDocumentCallback = endDocumentCallbackIORef,
             parserBeginElementCallback = beginElementCallbackIORef,
             parserEndElementCallback = endElementCallbackIORef,
             parserCharactersCallback = charactersCallbackIORef,
             parserCommentCallback = commentCallbackIORef,
             parserInstructionCallback = instructionCallbackIORef,
             parserDoctypeCallback = doctypeCallbackIORef
           }


setCallback :: Parser -> Callback a -> a -> IO ()
setCallback parser which callback = do
  case which of
    CallbackBeginDocument -> do
      writeIORef (parserBeginDocumentCallback parser) $ Just callback
    CallbackEndDocument -> do
      writeIORef (parserEndDocumentCallback parser) $ Just callback
    CallbackBeginElement -> do
      writeIORef (parserBeginElementCallback parser) $ Just callback
    CallbackEndElement -> do
      writeIORef (parserEndElementCallback parser) $ Just callback
    CallbackCharacters -> do
      writeIORef (parserCharactersCallback parser) $ Just callback
    CallbackComment -> do
      writeIORef (parserCommentCallback parser) $ Just callback
    CallbackInstruction -> do
      writeIORef (parserInstructionCallback parser) $ Just callback
    CallbackDoctype -> do
      writeIORef (parserDoctypeCallback parser) $ Just callback


clearCallback :: Parser -> Callback a -> IO ()
clearCallback parser which = do
  case which of
    CallbackBeginDocument -> do
      writeIORef (parserBeginDocumentCallback parser) Nothing
    CallbackEndDocument -> do
      writeIORef (parserEndDocumentCallback parser) Nothing
    CallbackBeginElement -> do
      writeIORef (parserBeginElementCallback parser) Nothing
    CallbackEndElement -> do
      writeIORef (parserEndElementCallback parser) Nothing
    CallbackCharacters -> do
      writeIORef (parserCharactersCallback parser) Nothing
    CallbackComment -> do
      writeIORef (parserCommentCallback parser) Nothing
    CallbackInstruction -> do
      writeIORef (parserInstructionCallback parser) Nothing
    CallbackDoctype -> do
      writeIORef (parserDoctypeCallback parser) Nothing


parsedBeginDocument :: Callback (IO Bool)
parsedBeginDocument = CallbackBeginDocument


parsedEndDocument :: Callback (IO Bool)
parsedEndDocument = CallbackEndDocument


parsedBeginElement :: Callback (Name -> [Attribute] -> IO Bool)
parsedBeginElement = CallbackBeginElement


parsedEndElement :: Callback (Name -> IO Bool)
parsedEndElement = CallbackEndElement


parsedCharacters :: Callback (String -> IO Bool)
parsedCharacters = CallbackCharacters


parsedComment :: Callback (String -> IO Bool)
parsedComment = CallbackComment


parsedInstruction :: Callback (Instruction -> IO Bool)
parsedInstruction = CallbackInstruction


parsedDoctype :: Callback (Doctype -> IO Bool)
parsedDoctype = CallbackDoctype


isNameStartChar :: Char -> Bool
isNameStartChar c =
  let codepoint = ord c
      inRange (a, b) = a <= codepoint && codepoint <= b
  in isLetter c || c == '_' || any inRange nameStartCharRanges


isNameChar :: Char -> Bool
isNameChar c =
  let codepoint = ord c
      inRange (a, b) = a <= codepoint && codepoint <= b
  in isLetter c
     || isDigit c
     || c == '-'
     || c == '.'
     || c == (chr 0xB7)
     || any inRange nameCharRanges


nameStartCharRanges :: [(Int, Int)]
nameStartCharRanges =
  [(0xC0, 0xD6), (0xD8, 0xF6), (0xF8, 0x2FF), (0x370, 0x37D), (0x37F, 0x1FFF),
   (0x200C, 0x200D), (0x2070, 0x218F), (0x2C00, 0x2FEF), (0x3001, 0xD7FF),
   (0xF900, 0xFDCF), (0xFDF0, 0xFFFD), (0x10000, 0xEFFFF)]


nameCharRanges :: [(Int, Int)]
nameCharRanges =
  nameStartCharRanges
  ++ [(0x0300, 0x036F), (0x203F, 0x2040)]


parseBytes :: Parser -> ByteString -> IO ()
parseBytes parser newBytes = do
  undefined


parseComplete :: Parser -> IO ()
parseComplete parser = do
  preexistingBytes <- readIORef $ parserInputBuffer parser
  if not $ BS.null preexistingBytes
    then parserErrorHandler parser $ "Trailing garbage at end of XML."
    else return ()
