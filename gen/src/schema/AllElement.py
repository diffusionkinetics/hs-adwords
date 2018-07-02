
from .AnnotationElement import * 
from .Element import * 
from ..utils import * 


class AllElement:
    
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.max_occurs = elem.attrib.get("maxOccurs", 1)
        self.min_occurs = elem.attrib.get("minOccurs", 1)
        self.annotations = [AnnotationElement(a) for a in elem.getchildren() if clear_tag(a) == "annotation"]
        self.elements = [Element(a) for a in elem.getchildren() if clear_tag(a) == "element"]
