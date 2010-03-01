{- This module was generated from data in the Kate syntax highlighting file slate.xml, version 0.3,
   by  José Pablo Ezequiel 'Pupeno' Fernández (pupeno@pupeno.com) -}

module Text.Highlighting.Kate.Syntax.Slate ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Slate"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.slate"

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
  setState $ st { synStLanguage = "Slate" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Slate",["Normal"])], synStLanguage = "Slate", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Comment" -> return ()
    "String" -> return ()
    "Symbol" -> (popContext >> return ())
    "BlockHeader" -> return ()
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

styles = [("Normal","Normal"),("BlockHeader","Keyword"),("KeywordMessage","Keyword"),("BinaryMessage","Keyword"),("SpecialChar","Keyword"),("Type","DataType"),("Symbol","Keyword"),("String","String"),("Character","String"),("EscapedString","String"),("Comment","Comment"),("Decimal","Float"),("Float","Float")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal"),("Comment","Comment"),("String","String"),("Symbol","Symbol"),("BlockHeader","")]

parseRules "Normal" = 
  do (attr, result) <- (((pDetectChar False '"' >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ pushContext "String")
                        <|>
                        ((pDetectChar False '#' >>= withAttribute "Symbol") >>~ pushContext "Symbol")
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z_\\x7f-\\xff][a-zA-Z0-9_\\x7f-\\xff]*: ") >>= withAttribute "KeywordMessage"))
                        <|>
                        ((pRegExpr (compileRegex "[\\#\\$\\%\\*\\-\\+\\=\\~\\/\\\\\\?<>\\,\\;]+ ") >>= withAttribute "BinaryMessage"))
                        <|>
                        ((pRegExpr (compileRegex "\\$. ") >>= withAttribute "Character"))
                        <|>
                        ((pDetectChar False '|' >>= withAttribute "SpecialChar") >>~ pushContext "BlockHeader")
                        <|>
                        ((pDetectChar False '@' >>= withAttribute "SpecialChar"))
                        <|>
                        ((pDetectChar False '&' >>= withAttribute "SpecialChar"))
                        <|>
                        ((pFloat >>= withAttribute "Float"))
                        <|>
                        ((pInt >>= withAttribute "Decimal")))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- ((pDetectChar False '"' >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "String" = 
  do (attr, result) <- (((pRegExpr (compileRegex "\\\\[enrtb0afvs'\\\\]") >>= withAttribute "EscapedString"))
                        <|>
                        ((pDetectChar False '\'' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Symbol" = 
  do (attr, result) <- ((pRegExpr (compileRegex "[ \\(\\)\\[\\]\\{\\}\\@\\.\\|\\!]") >>= withAttribute "Normal") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "BlockHeader" = 
  do (attr, result) <- (((pDetectChar False '|' >>= withAttribute "SpecialChar") >>~ (popContext >> return ()))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z_\\x7f-\\xff][a-zA-Z0-9_\\x7f-\\xff]*") >>= withAttribute "BlockHeader")))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
