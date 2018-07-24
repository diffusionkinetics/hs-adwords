from src import *
w = WSDLParser("adwords.xml")
import lxml.etree as ET 

print(w.get_schemas())
