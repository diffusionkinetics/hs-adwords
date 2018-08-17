from src import *
from src.generators.haskell import * 
w = WSDLParser("adwords.xml")
import lxml.etree as ET 


code = HaskellCodeBuilder()
code.type("SomeType", [("A", {"x": "Int", "y": "[Char]"})])
code.function("f", {"n": "Int"}, "Int", "n * ff (n-1)") 
#print(code.get_code())

generator = HaskellCodeGenerator()
#print(generator.parse_services(w))
print(generator.parse_schemas(w))
