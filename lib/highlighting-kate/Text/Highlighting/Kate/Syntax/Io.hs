{- This module was generated from data in the Kate syntax highlighting file io.xml, version 1.0,
   by  Vincent Adam Burns -}

module Text.Highlighting.Kate.Syntax.Io ( highlight, parseExpression, syntaxName, syntaxExtensions ) where
import Text.Highlighting.Kate.Definitions
import Text.Highlighting.Kate.Common
import Text.ParserCombinators.Parsec
import Data.List (nub)
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

-- | Full name of language.
syntaxName :: String
syntaxName = "Io"

-- | Filename extensions for this language.
syntaxExtensions :: String
syntaxExtensions = "*.io"

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
  setState $ st { synStLanguage = "Io" }
  context <- currentContext <|> (pushContext "Normal" >> currentContext)
  result <- parseRules context
  updateState $ \st -> st { synStLanguage = oldLang }
  return result

parseSource = do 
  lineContents <- lookAhead wholeLine
  updateState $ \st -> st { synStCurrentLine = lineContents }
  result <- manyTill parseSourceLine eof
  return $ map normalizeHighlighting result

startingState = SyntaxState {synStContexts = fromList [("Io",["Normal"])], synStLanguage = "Io", synStCurrentLine = "", synStCharsParsedInLine = 0, synStCaseSensitive = True, synStKeywordCaseSensitive = True, synStCaptures = []}

parseSourceLine = manyTill parseExpressionInternal pEndLine

pEndLine = do
  newline <|> (eof >> return '\n')
  context <- currentContext
  case context of
    "Normal" -> return ()
    "Comment" -> return ()
    "Tripple string" -> return ()
    "Single string" -> return ()
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

styles = [("Normal Text","Normal"),("Operator","Operator"),("Flow Control Keyword","Flow"),("Builtin Function","DataType"),("Special Variable","Others"),("Float","Float"),("Int","DecVal"),("Hex","Others"),("Comment","Comment"),("String","String")]

parseExpressionInternal = do
  context <- currentContext
  parseRules context <|> (pDefault >>= withAttribute (fromMaybe "" $ lookup context defaultAttributes))

defaultAttributes = [("Normal","Normal Text"),("Comment","Comment"),("Tripple string","String"),("Single string","String")]

parseRules "Normal" = 
  do (attr, result) <- (((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["`","~","$","%","{","}","[","]","++","--","*","/","%","^","+","-",">>","<<",">","<","<=",">=","==","!=","&","^","..","|","&&","||","!=","+=","-=","*=","/=","<<=",">>=","&=","|=","%=","=",":=","<-","<->","->",".","?","@","@@"] >>= withAttribute "Operator"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["and","break","else","elseif","exit","for","foreach","if","ifFalse","ifNil","ifTrue","or","pass","raise","return","then","try","wait","while","yield"] >>= withAttribute "Flow Control Keyword"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["activate","activeCoroCount","asString","block","catch","clone","collectGarbage","compileString","continue","do","doFile","doMessage","doString","forward","getSlot","getEnvironmentVariable","hasSlot","isActive","isNil","isResumable","list","message","method","parent","pause","perform","performWithArgList","print","proto","raiseResumable","removeSlot","resend","resume","schedulerSleepSeconds","self","sender","setSchedulerSleepSeconds","setSlot","shallowCopy","slotNames","super","system","thisBlock","thisContext","thisMessage","type","uniqueId","updateSlot","write"] >>= withAttribute "Builtin Function"))
                        <|>
                        ((pKeyword " \n\t.():!+,-<=>%&*/;?[]^{|}~\\" ["Array","AudioDevice","AudioMixer","Block","Box","Buffer","CFunction","CGI","Color","Curses","DBM","DNSResolver","DOConnection","DOProxy","DOServer","Date","Directory","Duration","DynLib","Error","Exception","FFT","File","Fnmatch","Font","Future","GL","GLE","GLScissor","GLU","GLUCylinder","GLUQuadric","GLUSphere","GLUT","Host","Image","Importer","LinkList","List","Lobby","Locals","MD5","MP3Decoder","MP3Encoder","Map","Message","Movie","NULL","nil","true","false","Notification","Number","Object","OpenGL","Point","Protos","Regex","SGMLTag","SQLite","Server","ShowMessage","SleepyCat","SleepyCatCursor","Socket","SocketManager","Sound","Soup","Store","String","Tree","UDPSender","UPDReceiver","URL","User","Warning","WeakLink"] >>= withAttribute "Special Variable"))
                        <|>
                        ((pRegExpr (compileRegex "[a-zA-Z_][a-zA-Z_0-9]+") >>= withAttribute "Normal"))
                        <|>
                        ((pRegExpr (compileRegex "([0-9]+\\.[0-9]*|\\.[0-9]+)([eE][0-9]+)?") >>= withAttribute "Float"))
                        <|>
                        ((pRegExpr (compileRegex "([1-9][0-9]*([eE][0-9]+)?|0)") >>= withAttribute "Int"))
                        <|>
                        ((pRegExpr (compileRegex "0[Xx][0-9a-fA-F]+") >>= withAttribute "Hex"))
                        <|>
                        ((pRegExpr (compileRegex "#.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "//.*$") >>= withAttribute "Comment"))
                        <|>
                        ((pRegExpr (compileRegex "/\\*.*$") >>= withAttribute "Comment") >>~ pushContext "Comment")
                        <|>
                        ((pString False "\"\"\"" >>= withAttribute "String") >>~ pushContext "Tripple string")
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ pushContext "Single string"))
     return (attr, result)

parseRules "Comment" = 
  do (attr, result) <- ((pRegExpr (compileRegex "\\*/") >>= withAttribute "Comment") >>~ (popContext >> return ()))
     return (attr, result)

parseRules "Tripple string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "String"))
                        <|>
                        ((pRegExpr (compileRegex "\"\"\"") >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules "Single string" = 
  do (attr, result) <- (((pHlCStringChar >>= withAttribute "String"))
                        <|>
                        ((pDetectChar False '"' >>= withAttribute "String") >>~ (popContext >> return ())))
     return (attr, result)

parseRules x = fail $ "Unknown context" ++ x
