{- This module was generated from data in the Kate syntax highlighting file go.xml, version 1.00,
   by  The Go Authors -}

module Text.Highlighting.Kate.Syntax.Go ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Go"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.go"

-- | Highlight source code using this syntax definition.
highlight :: String -> Either String [SourceLine]
highlight input =
  case runParser parseSource startingState "source" input of
    Left err     -> Left $ show err
    Right result -> Right result

-- | Parse an expression using appropriate local context.
parseExpression :: GenParser Char SyntaxState LabeledSource
parseExpression = do
  st <- getState
  let oldLang = synStLanguage st
  setState $ st { synStLanguage = "Go" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Go",["Normal"])], synStLanguage = "Go", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "String" -> (popContext >> return ())
    "Multiline String" -> return ()
    "Comment 1" -> (popContext >> return ())
    "Comment 2" -> return ()
    _ -> return ()
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents, synStCharsParsedInLine = 0 }

withAttribute attr txt = do
  if null txt
     then fail "Parser matched no text"
     else return ()
  let style = fromMaybe "" $ lookup attr styles
  st <- getState
  let oldCharsParsed = synStCharsParsedInLine st
  updateState $ \st -> st { synStCharsParsedInLine = oldCharsParsed + length txt } 
  return (nub [style, attr], txt)

styles = [("Normal Text","Normal"),("Keyword","Keyword"),("Builtin Function","DataType"),("Data Type","DataType"),("Decimal","DecVal"),("Octal","BaseN"),("Hex","BaseN"),("Float","Float"),("Char","Char"),("String","String"),("String Char","Char"),("Comment","Comment"),("Symbol","Normal"),("Error","Error")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("String","String"),("Multiline String","String"),("Comment 1","Comment"),("Comment 2","Comment")]

parseRules "Normal" = 
  do (attr, result) <- (((pDetectSpaces >>= withAttribute "Normal Text"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\'\"" ["break","case","const","continue","default","defer","else","fallthrough","false","for","func","go","goto","if","import","interface","iota","nil","package","range","return","select","struct","switch","true","type","var"] >>= withAttribute "Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\'\"" ["bool","byte","chan","float","float32","float64","int","int8","int16","int32","int64","map","string","uint","uintptr","uint8","uint16","uint32","uint64"] >>= withAttribute "Data Type"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\'\"" ["cap","close","closed","convert","copy","len","make","new","panic","panicln","print","println"] >>= withAttribute "Builtin Function"))
                        <|>
                        ((pDetectIdentifier >>= withAttribute "Normal Text"))
                        <|>
                        ((pDetectChar False '{' >>= withAttribute "Symbol"))
                        <|>
                        ((pDetectChar False '}' >>= withAttribute "Symbol"))
                        <|>
                        ((pHlCOct >>= withAttribute "Octal"))
                        <|>
                        ((pHlCHex >>= withAttribute "Hex"))
                        <|>
                        ((pHlCChar >>= withAttribute "Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "Multiline String") >>~ pushContext "Multiline String")
                        <|>
                        ((pDetect2Chars False '/' '/' >>= withAttribute "Comment") >>~ pushContext "Comment 1")
                        <|>
                        ((pDetect2Chars False '/' '*' >>= withAttribute "Comment") >>~ pushContext "Comment 2")
                        <|>
                        ((pAnyChar ":!%&()+,-/.*<=>?[]|~^;" >>= withAttribute "Symbol")))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String"))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Multiline String" = 
  do (attr, result) <- (((pLineContinue >>= withAttribute "String"))
                        <|>
                        ((pHlCStringChar >>= withAttribute "String Char"))
                        <|>
                        ((pDetectChar False '`' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Comment 1" = 
  do (attr, result) <- ((pLineContinue >>= withAttribute "Comment"))
     return (attr, result)

parseRules "Comment 2" = 
  do (attr, result) <- ((pDetect2Chars False '*' '/' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
