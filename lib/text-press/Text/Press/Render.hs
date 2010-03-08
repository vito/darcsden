module Text.Press.Render where

import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.Error.Class (throwError)

import Data.Map (Map, lookup, fromList, insert)
import Data.Maybe (listToMaybe, catMaybes)
import Prelude hiding (lookup)
import Data.List hiding (lookup)

import Text.JSON.Types
import Text.JSON

import Text.XHtml.Strict (stringToHtmlString)

import Text.Press.Types

emit s = tell [s]

data EscapedJS = E JSValue
               deriving (Eq, Show)

instance Render Node where
    render (Text s) = emit s
    render (HtmlVar var) = do
        context <- getRenderState
        case lookupVar var context of
            Nothing -> emit ""
            Just jsval -> render jsval
    render (Var var) = do
        context <- getRenderState
        case lookupVar var context of
            Nothing -> emit ""
            Just jsval -> render (E jsval)
    render (Tag _ f) = render f

instance Render TagFunc where
    render (TagFunc f) = f

instance Render JSValue where
    render JSNull = emit ""
    render (JSString x) = emit $ fromJSString x
    render other = emit $ (showJSValue other) ""

instance Render EscapedJS where
    render (E JSNull) = emit ""
    render (E (JSString x)) = emit $ stringToHtmlString $ fromJSString x
    render (E other) = emit $ stringToHtmlString $ (showJSValue other) ""

lookupVarM name = do
    st <- getRenderState
    return $ lookupVar name st

lookupVar name (RenderState {renderStateValues = vals}) =
    listToMaybe . catMaybes $ map (getf name) vals

split :: String -> String -> [String]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ "" = Nothing
          sp1 t s = case find (t `isSuffixOf`) (inits s) of
                      Nothing -> Just (s, "")
                      Just p -> Just (take ((length p) - (length t)) p,
                                      drop (length p) s)

getf name a = getf' names (Just a)
    where
        names = split "." name
        getf' [] y = y
        getf' x Nothing = Nothing
        getf' (x : xs) obj@(Just (JSObject a)) = getf' xs $ get_field a x
        getf' x y = Nothing

-- Show a block
showBlock :: String -> RenderT_
showBlock blockName = do
    templates <- templateStack
    let maybeNodes = lookupFirst blockName $ map tmplBlocks $ templates
    case maybeNodes of
        Just nodes -> mapM_ render nodes
        Nothing -> tell [""]

lookupFirst :: Ord k => k -> [Map k a] -> Maybe a
lookupFirst name maps = listToMaybe . catMaybes $ map (lookup name) maps

getTemplate = fmap renderStateTemplate getRenderState

templateStack = getTemplate >>= templateStack'
    where
        templateStack' t@(Template {tmplExtends=Nothing}) = return [t]
        templateStack' t@(Template {tmplExtends=Just name}) = do
            cache <- fmap (parserTemplateCache . renderStateParser) get
            case lookup name cache of
                Just template -> do
                    templates <- templateStack' template
                    return $ t : (template : templates)
                Nothing -> throwError $ PressError $ "expecting a template in the cache named: " ++ (show name)

doRender = do
    bodyNodes <- fmap (tmplNodes . last) templateStack
    mapM render bodyNodes

coerceJSToBool :: JSValue -> Bool
coerceJSToBool JSNull = False
coerceJSToBool (JSBool bool) = bool
coerceJSToBool (JSRational sign r) = (not sign) && (r > 0)
coerceJSToBool (JSString x) = length (fromJSString x) > 0
coerceJSToBool (JSArray vals) = length vals > 0
coerceJSToBool (JSObject obj) = length (fromJSObject obj) > 0
