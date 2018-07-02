
from ..utils import * 

from .AnnotationElement import * 
from .ChoiceElement import * 
from .SequenceElement import *
from .AllElement import * 

class GroupElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.choices = get_elements(elem, ChoiceElement, "choice")
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.alls = get_elements(elem, AllElement, "all")
        self.name = elem.attrib.get("name", "")
        self.id = elem.attrib.get("id", "")
