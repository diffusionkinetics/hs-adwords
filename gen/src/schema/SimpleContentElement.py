
from .Annotation import * 
from .RestrictionElement import * 
from .ExtensionElement import * 

class SimpleContentElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.restrictions = get_elements(elem, RestrctionElement, "restriction")
        self.extensions = get_elements(elem, ExtensionElement, "extension")
