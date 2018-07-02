
from ..utils import * 

class ComplexContentElementL:
    
    def __init__(self, elem):
        self.mixed = elem.attrib.get("mixed", False)
        self.id = elem.attrib.get("id", "")
        seolf.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.restrictions = get_elements(elem, RestrictionElement, "restriction")
        self.extensions = get_elements(elem, ExtensionElement, "extension")
