{-# LANGUAGE GADTs, ForeignFunctionInterface #-}
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
import Foreign
import Foreign.C
import Foreign.C.String


newtype XML_Parser = XML_Parser (Ptr ())

type StartElementHandler = Ptr () -> CString -> Ptr CString -> IO ()


foreign import ccall "XML_ParserCreate"
  xml_ParserCreate :: CString -> IO XML_Parser

foreign import ccall "XML_ParserFree"
  xml_ParserFree :: XML_Parser -> IO ()

foreign import ccall "XML_Parse"
  xml_Parse :: XML_Parser -> CString -> CInt -> CInt -> IO CInt

foreign import ccall "XML_SetStartElementHandler"
  xml_SetStartElementHandler
    :: XML_Parser -> FunPtr StartElementHandler -> IO ()

foreign import ccall "wrapper"
  mkStartElementHandler
    :: StartElementHandler -> IO (FunPtr StartElementHandler)

foreign import ccall "XML_GetErrorCode"
  xml_GetErrorCode :: XML_Parser -> IO CInt

foreign import ccall "XML_ErrorString"
  xml_ErrorString :: CInt -> IO CString

foreign import ccall "XML_GetCurrentLineNumber"
  xml_GetCurrentLineNumber :: XML_Parser -> IO CULong

foreign import ccall "XML_GetCurrentColumnNumber"
  xml_GetCurrentColumnNumber :: XML_Parser -> IO CULong


data Parser = Parser {
    parserForeignParser :: XML_Parser,
    parserErrorHandler :: String -> IO (),
    parserHasBegunDocument :: IORef Bool,
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
          -> IO Parser
newParser errorHandler = do
  foreignParser <- withCString "UTF-8" (\encoding -> xml_ParserCreate encoding)
  hasBegunDocumentIORef <- newIORef False
  beginDocumentCallbackIORef <- newIORef Nothing
  endDocumentCallbackIORef <- newIORef Nothing
  beginElementCallbackIORef <- newIORef Nothing
  endElementCallbackIORef <- newIORef Nothing
  charactersCallbackIORef <- newIORef Nothing
  commentCallbackIORef <- newIORef Nothing
  instructionCallbackIORef <- newIORef Nothing
  doctypeCallbackIORef <- newIORef Nothing
  
  parser
    <- return Parser {
         parserForeignParser = foreignParser,
         parserErrorHandler = errorHandler,
         parserHasBegunDocument = hasBegunDocumentIORef,
         parserBeginDocumentCallback = beginDocumentCallbackIORef,
         parserEndDocumentCallback = endDocumentCallbackIORef,
         parserBeginElementCallback = beginElementCallbackIORef,
         parserEndElementCallback = endElementCallbackIORef,
         parserCharactersCallback = charactersCallbackIORef,
         parserCommentCallback = commentCallbackIORef,
         parserInstructionCallback = instructionCallbackIORef,
         parserDoctypeCallback = doctypeCallbackIORef
       }
  
  startElementHandler <- mkStartElementHandler $ handleStartElement parser
  xml_SetStartElementHandler foreignParser startElementHandler
  
  return parser


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


handleStartElement :: Parser -> Ptr () -> CString -> Ptr CString -> IO ()
handleStartElement parser _ elementName attributeArray = do
  putStrLn $ "Got start element, huzzah."


parseBytes :: Parser -> ByteString -> IO ()
parseBytes parser newBytes = do
  hasBegunDocument <- readIORef $ parserHasBegunDocument parser
  keepGoing
    <- if not hasBegunDocument
         then do
           maybeBeginDocumentCallback
             <- readIORef $ parserBeginDocumentCallback parser
           case maybeBeginDocumentCallback of
             Nothing -> return True
             Just beginDocumentCallback -> beginDocumentCallback
         else return True
  if keepGoing
    then do
      let bufferLength = BS.length newBytes
      statusInt <- allocaBytes bufferLength
                               (\buffer -> do
                                  let loop i = do
                                        if i == bufferLength
                                          then return ()
                                          else do
                                            let byte = BS.index newBytes i
                                            pokeElemOff buffer i byte
                                            loop $ i + 1
                                  loop 0
                                  xml_Parse (parserForeignParser parser)
                                            (castPtr buffer)
                                            (fromIntegral bufferLength)
                                            0)
      handleStatusInt parser statusInt
    else return ()


parseComplete :: Parser -> IO ()
parseComplete parser = do
  statusInt <- alloca (\emptyData -> xml_Parse (parserForeignParser parser)
                                               emptyData
                                               0
                                               1)
  handleStatusInt parser statusInt
  maybeEndDocumentCallback
    <- readIORef $ parserEndDocumentCallback parser
  keepGoing <- case maybeEndDocumentCallback of
                 Nothing -> return True
                 Just endDocumentCallback -> endDocumentCallback
  if keepGoing
    then xml_ParserFree (parserForeignParser parser)
    else return ()


handleStatusInt :: Parser -> CInt -> IO ()
handleStatusInt parser 0 = do
  lineNumber <- xml_GetCurrentLineNumber (parserForeignParser parser)
  columnNumber <- xml_GetCurrentColumnNumber (parserForeignParser parser)
  errorString <- xml_GetErrorCode (parserForeignParser parser)
                 >>= xml_ErrorString
                  >>= peekCString
  parserErrorHandler parser $ "XML parse error on line " ++ (show lineNumber) ++
                              ", column " ++ (show columnNumber) ++
                              ": " ++ errorString
handleStatusInt parser 1 = return ()
