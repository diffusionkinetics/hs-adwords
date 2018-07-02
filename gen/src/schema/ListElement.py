
from ..utils import * 
from .SimpleTypeElement import * 
from .AnnotationElement import * 

class ListElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.simple_types = get_elements(elem, SimpleType, "simpleType")
        self.item_type = elem.attrib.get("itemType", "")
