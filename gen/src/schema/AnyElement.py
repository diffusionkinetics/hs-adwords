
from ..utils import * 
from .AnnotationElement import * 

class AnyElement:
    def __init__(self, elem):
        self.min_occurs = elem.attrib.get("minOccurs", 1)
        self.maxOccurs = elem.attrib.get("maxOccurs", 1)
        self.annotations = [AnnotationElement(e) for e in elem.getchildren() if clear_tag(e) == "annotation"]
