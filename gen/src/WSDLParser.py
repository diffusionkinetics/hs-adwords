
from .AST import * 
from zeep import * 

from zeep.xsd.types import * 
from zeep.xsd.elements.indicators import *

import lxml.etree as ET


class WSDLParser(object):

    def __init__(self, path):
        self.data = self.parse(path)
        self.root = ET.parse(path).getroot()

    def get_schemas(self):
        return [e for e in self.root.iter() if e.tag.endswith("schema")]

    def get_types(self):
        schemas = self.get_schemas() 
        res = [] 
        for schema in schemas:
            for e in schema.iter():
                if e.tag.endswith("simpleType") or e.tag.endswith("complexType"):
                    res.append(e)
        return res

    def parse(self, path):
        return Client(path).wsdl

    def get_messages(self):
        l = [m for m in self.data.messages.values()]
        res = [] 
        for message in l:
            parts = [ p for p in message.parts.values()]
            fields = [] 
            for part in parts:
                if part.element == None:
                    fields.append((part.name, Type(part.type.name)))
                else:
                    fields.append((part.element.name, Type(part.element.name)))
            res.append(Message(name=message.name.localname, fields=fields))
        return res

    def get_elements(self):
        res = [] 
        for elem in list(self.data.types.elements)[1:]:
            if elem.type.__class__.__base__ == ComplexType:
                res.append(ComplexElement(elem.name, self.__parse_elems(elem.type.elements_nested)))
            elif elem.type.__class__.__base__ == Sequence:
                    pass
            else:
                res.append(Element(elem.name, str(elem.type.name)))
        return res

    def __parse_elems(self, elems):
        res = [] 
        for name, typ in elems:
            if typ.__class__ == ComplexType:
                res.append(ComplexElement(name, self.__parse_elems(typ)))
            elif typ.__class__ == Sequence:
                    res.append(SequenceElement(name=name, type=self.__parse_elems(typ.elements_nested)))
            else:
                res.append(Element(name, type = str(typ.name)))
        return res
