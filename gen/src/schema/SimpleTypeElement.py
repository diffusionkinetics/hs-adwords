
from .AnnotationElement import * 
from .UnionElement import * 
from .ListElement import * 
from .RestrictionElement import * 

class SimpleType:
    def __init__(self, elem):
        self.name = elem.attrib.get("name", "")
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.restrictions = get_elements(elem, RestrictionElement, "restriction")
        self.unions = get_elements(elem, UnionElement, "union")
        self.lists = get_elements(elem, ListElement, "list")
