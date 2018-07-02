
from ..Utils import * 
from .AnnotationElement import * 
from .SimpleType import * 

class UnionElement:
    def __init__(self, elem):
        self.member_types = elem.attrib.get("memberTypes", "")
        self.simple_types = get_elements(elem, SimpleTypeElement, "simpleType")
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
