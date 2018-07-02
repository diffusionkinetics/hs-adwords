
from ..utils import * 
from .AnnotationElement import * 
from .Element import * 
from .GroupElement import * 
from .SequenceElement import * 
from .AnyElement import * 


class ChoiceElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.elements = get_elements(elem, Element, "element")
         self.groups = get_elements(elem, GroupElement, "group")
         self.choices = get_elements(elem, "choice", ChoiceElement)
         self.sequences = get_elements(elem, SequenceElement, "sequence")
         self.anys = get_elements(elem, AnyElement, "any")
         self.max_occurs = elem.attrib.get("maxOccurs", "unbounded")
         self.min_occurs = elem.attrib.get("minOccurs", 0)
