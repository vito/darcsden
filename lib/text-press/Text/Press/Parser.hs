
module Text.Press.Parser where
import Data.Char (isSpace)
import Data.Either (Either(..))
import Data.Map (fromList, Map, lookup, insert)
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (intercalate)
import Prelude hiding (lookup)

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec.Error
import Text.Parsec.Char (string, anyChar, space, alphaNum, letter, oneOf)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Combinator (manyTill, many1, notFollowedBy, choice, eof, lookAhead, optional, sepBy1, sepEndBy)
import Text.Parsec.Pos (SourcePos, sourceName)
import Text.Parsec.Prim ((<|>), try, Parsec, getPosition, getState)
import qualified Text.Parsec.Prim as Parsec.Prim

import Text.Press.Types
import Text.Press.Render

skipMany p = scan
    where scan = (p >> scan) <|> return ()

intermediateParser = manyTill intermediate eof

intermediate = choice [try parseTag, try parseHtmlVar, try parseVar, someText]

someText = withPos $ fmap PText someText'
    where someText' = (choice [check $ string "{", check $ string "{{", check $ string "{%", check eof]) <|> succ
          check p = (lookAhead $ try p) >> return []
          succ = do
            c <- anyChar
            xs <- someText'
            return $ c : xs

between left right = string left >> manyTill anyChar (string right)

withPos action = do
    p <- getPosition
    result <- action
    return (result, p)

parseTag = withPos $ do
    string "{%"
    skipMany space
    name <- identifier
    skipMany space
    rest <- manyTill anyChar (string "%}")
    return $ PTag name rest
    where

identifier = do
    l <- choice [letter, oneOf "_"]
    s <- Parsec.Prim.many (choice [alphaNum, oneOf "_"])
    return (l:s)

parseHtmlVar = withPos $ fmap PHtmlVar $ between "{{{" "}}}"

parseVar = withPos $ fmap PVar $ between "{{" "}}"

parseFile :: Parser -> String -> IO (Either Parsec.ParseError Template)
parseFile parser filename = do
    eitherTokens <- parseFromFile intermediateParser filename
    return $ case eitherTokens of
                Left err -> Left err
                Right tokens -> Parsec.Prim.runParser tokensToTemplate (parser, newTemplate) filename tokens

parseString :: Parser -> String -> Either Parsec.ParseError Template
parseString parser string = do
    case Parsec.Prim.runParser intermediateParser () "" string of
        Left err -> Left err
        Right tokens -> Parsec.Prim.runParser tokensToTemplate (parser, newTemplate) "" tokens


tokensToTemplate :: Parsec [(Token, SourcePos)] ParserState Template
tokensToTemplate = do
    nodes <- fmap catMaybes $ Parsec.Prim.many pNode
    (p, t) <- getState
    return $ t {tmplNodes=nodes}

pNode = choice [pHtmlVar, pVar, pTag, pText]

pHtmlVar = do
    (PHtmlVar x, pos) <- htmlVar
    return $ Just $ HtmlVar $ strip x

pVar = do
    (PVar x, pos) <- var
    return $ Just $ Var $ strip x

pText = do
    (PText x, pos) <- text
    return $ Just $ Text x

pTag = do
    ((PTag name rest), pos) <- tag
    (parser, _) <- getState
    case lookup name (parserTagTypes parser) of
        Nothing -> fail ("unknown tag: " ++ show name)
        Just (TagType t) -> t name rest

token' x = Parsec.Prim.token (show . fst) (snd) x
htmlVar = token' $ toMaybe $ isHtmlVar . fst
var = token' $ toMaybe $ isVar . fst
text = token' $ toMaybe $ isText . fst
tag = token' $ toMaybe $ isTag . fst
tagNamed name = token' $ toMaybe $ (isTagNamed name) . fst
tagNamedOneOf name = token' $ toMaybe $ (isTagNamedOneOf name) . fst

toMaybe f tokpos = if (f tokpos) then Just tokpos else Nothing

isHtmlVar (PHtmlVar _) = True
isHtmlVar otherwise = False

isVar (PVar _) = True
isVar otherwise = False

isTag (PTag _ _) = True
isTag otherwise = False

isTagNamed aname tag = isTagNamedOneOf [aname] tag

isTagNamedOneOf names (PTag name _) = name `elem` names
isTagNamedOneOf names otherwise = False

isText (PText _) = True
isText otherwise = False

strip = f . f
    where f = reverse . dropWhile isSpace

handleParsecError e = error (show e)

failWithParseError :: (Parsec.Prim.Stream s m t) => Parsec.Error.ParseError -> Parsec.Prim.ParsecT s u m a
failWithParseError parseError = Parsec.Prim.ParsecT $
    \s -> return $ Parsec.Prim.Empty $ return $ Parsec.Prim.Error parseError

runSubParser parser state input = do
    name <- fmap sourceName getPosition
    case Parsec.Prim.runParser parser state name input of
        Left parseError -> failWithParseError parseError
        Right tokens -> return tokens

spaces = many1 space

runParseTagExpressions input = runSubParser parseTagExpressions () input
    where parseTagExpressions = do
              optional spaces
              exprs <- (choice [try pStr, try pVar]) `sepEndBy` spaces
              return exprs
          pStr = fmap ExprStr $ between "\"" "\""
          pVar = fmap (ExprVar . intercalate ".") $ (identifier `sepBy1` string ".")


