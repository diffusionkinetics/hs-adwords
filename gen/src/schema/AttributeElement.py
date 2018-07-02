
from ..utils import * 
from .SimpleTypeelement import * 

class AttributeElement:
    def __init__(self, elem):
        self.default = elem.attrib.get("default", "")
        self.fixed = elem.attrib("fixed", "")
        self.form = elem.attrib.get("form", "")
        self.id = elem.attrib("id", "")
        self.name = elem.attrib.get("name", "")
        self.ref = elem.attrib.get("ref", "")
        self.type = elem.attrib.get("type", "")
        self.use = elem.attrib.get("use", "")
        self.annotations = [AnnotationElement(e) for e in elem.getchildren() if clear_tag(e) == "annotation"]
        self.simple_types = [SimpleTypeElement(e) for e in elem.getchildren() if clear_tag(e) == "simpleType"]
