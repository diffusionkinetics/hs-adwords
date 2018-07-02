
from ..utils import * 
from .AnnotationElement import *
from .SelectorElement import * 
from .FieldElement import *

class UniqueElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.selectors = get_elements(elem, SelectorElement, "selector")
        self.fields = get_elements(elem, FieldElement, "field")
