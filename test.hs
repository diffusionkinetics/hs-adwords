{-# LANGUAGE OverloadedStrings #-}

module Test where 

import Text.XML.Writer (element, document, elementA, ToXML(..), content, pprint)
import Text.XML as P
import Data.Text 

data MyElement = ElementA Int | ElementB String 


instance ToXML MyElement where
	toXML elem = do 
		element "A" $ content "something"
		element "B" $ do 
			element "C" $ content "AAAA"
			elementA "AttributedElement" [("a", "b")] $ content "text"


doc = document "root" $ toXML $ ElementA 2

text = "<a><b a='2'>something</b></a>"

parseIt :: P.Document 
parseIt = case P.parseText def text of 
	Left err -> P.parseText_ def "<haha>lol</haha>"
	Right doc -> doc
