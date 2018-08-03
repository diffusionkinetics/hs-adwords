
from .AST import * 
from zeep import * 

import lxml.etree as ET

from .schema import * 

class Parser:
    def get_schemas(self):
        raise NotImplementedError

    def get_services(self):
        raise NotImplementedError

    def get_messages(self):
        raise NotImplementedError

    def __add__(self, other):
        return ParserUnion([self, other])

class WSDLParser(Parser):

    def __init__(self, path):
        self.data = self.parse(path)
        self.root = ET.parse(path).getroot()

    def get_schemas(self):
        schemas = [e for e in self.root.iter() if clear_tag(e) == "schema"]
        return [Schema(s) for s in schemas]

    def get_services(self):
        return list(self.data.services.values())

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

class ParserUnion(Parser):
    def __init__(self, items):
        self.items = items

    def get_schemas(self):
        return sum([item.get_schemas() for item in self.items], [])

    def get_services(self):
        return sum([item.get_services() for item in self.items], [])

    def get_messages(self):
        return sum([item.get_services() for item in self.items], [])
