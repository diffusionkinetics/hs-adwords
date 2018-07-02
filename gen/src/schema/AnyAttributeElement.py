
from ..utils import * 
from .AnnotationElement import * 

class AnyAttributeElement:
    def __init__(self, elem):
        self.annotations = [AnnotationElement(e) for e in elem.getchildren() if clear_tag(e) == "annotation"]
