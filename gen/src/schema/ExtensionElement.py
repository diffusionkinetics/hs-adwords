
from ..utils import * 
from .AnnotationElement import *
from .GroupElement import *
from .AllElement import * 
from .ChoiceElement import * 
from .SequenceElement import * 
from .AttributeElement import * 
from .AttributeGroupElement import * 
from .AnyAttributeElement import * 

class ExtensionElement:
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.groups = get_elements(elem, GroupElement, "group")
        self.alls = get_elements(elem, AllElement, "all")
        self.choices = get_elements(elem, ChoiceElement, "choice")
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.attributes = get_elements(elem, AttributeElement, "attribute")
        self.attribute_groups = get_elements(elem, AttributeGroupElement, "attributeGroup")
        self.any_attributes = get_elements(elem, AnyAttributeElement, "anyAttribute")
        self.base = elem.attrib["base"]
        self.id = elem.attrib.get("id", "")
