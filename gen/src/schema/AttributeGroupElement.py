
from ..utils import * 

from AnnotationElement import * 
from AttributeElement import * 
from AnyAttributeElement import * 

class AtrributeGroupElement:
    def __init___(self, elem):\
        self.attributes = get_elements(AttributeElement, "attribute")
        self.annotations = get_elements(AnnotationElement, "annotation")
        self.attribute_groups = get_elements(AttributeGroupElement, "attributeGroup")
        self.any_attributes = get_elements(elem, AnyAttributeElement, "anyAttribute")
