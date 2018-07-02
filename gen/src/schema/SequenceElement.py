
from ..utils import * 
from .AnnotationElement import * 
from .Element import * 
from .ChoiceElement import * 
from .GroupElement import * 
from .SequenceElement import * 
from .AnyElement import * 

class SequenceElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnotationElement, "annotation")
        self.element = get_elements(elem, Element, "element")
        self.groups = gget_elements(elem, GroupElement, "group")
        self.choices = get_elements(elem, ChoiceElement, "choice")
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.anys = get_elements(elem, AnyElement, "any")
