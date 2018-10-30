{-# LANGUAGE OverloadedStrings #-}

module ParseXML where 

import Control.Monad.State
import Text.XML.Writer (element, document, elementA, ToXML(..), content, pprint)
import Text.XML as P
import Text.XML (Node(..), Element(..))
import Data.Text 

data ParseState = ParseResult [Element] | ParseError 
type Parse a = State ParseState a

elementContent :: Text -> Parse Text 
elementContent name = do 
	res <- get 
	case res of 
		(ParseResult (x:xs)) -> do 
			if (nameLocalName . elementName) x == name then do 
				case elementNodes x of 
					((NodeContent txt):rest) -> do 
						put $ ParseResult xs 
						return txt 
					_ -> do 
						put ParseError 
						return ""
			else do 
				put ParseError 
				return ""
		_ -> do 
			put ParseError 
			return ""

maybeElementContent :: Text -> Parse (Maybe Text)
maybeElementContent name = do 
	res <- get 
	case res of 
		(ParseResult (x:xs)) -> do 
			if (nameLocalName . elementName) x == name then do 
				case elementNodes x of 
					((NodeContent txt):rest) -> do 
						put $ ParseResult xs 
						return $ Just txt
					_ -> do 
						put ParseError 
						return Nothing
			else do 
				put $ ParseResult $ (x:xs)
				return Nothing
		_ -> do 
			put ParseError 
			return Nothing

multipleElementContent :: Text -> Parse [Text]
multipleElementContent name = do 
	res <- maybeElementContent name 
	case res of 
		Nothing -> return [] 
		(Just t) -> do 
			rest <- multipleElementContent name 
			return $ t:rest

class FromXML a where
	parse :: Parse a


getElements :: [Node] -> [Element]
getElements [] = [] 
getElements ((NodeElement e):rest) = e : (getElements rest)

parseXML :: (FromXML a) => P.Document -> Maybe a 
parseXML doc = let 
	p = parse 
	nodes = elementNodes $ documentRoot $ doc 
	elements = getElements nodes
	in case runState p (ParseResult elements) of 
		(res, ParseError) -> Nothing 
		(res, _) -> Just res
	

-- parseA :: Parse A 
-- parseA = do 
-- 	_aa <- elementContent "a"
-- 	_ab <- elementContent "b"
-- 	return $ A {
-- 		aa = _aa,
-- 		ab = _ab
-- 		}

-- parseIt :: Maybe A
-- parseIt = case P.parseText def text of 
-- 	Left err -> Nothing
-- 	Right doc -> parseXML parseA doc 

-- text = "<root><a>1</a><b>2</b></root>"
