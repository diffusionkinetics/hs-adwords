from src import *
w = WSDLParser("adwords.xml")
import lxml.etree as ET 

print(w.get_schemas())

code = HaskellCodeBuilder()
code.type("SomeType", [("A", {"x": "Int", "y": "[Char]"})])
code.function("f", {"n": "Int"}, "Int", "n * ff (n-1)") 
print(code.get_code())
