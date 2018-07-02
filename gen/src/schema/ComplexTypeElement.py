
from ..utils import * 

class ComplexTypeElement:
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.name = elem.attrib.get("name", "")
        self.abstract = elem.attrib.get("abstract", False)
        self.mixed = elem.attrib.get("mixed", False)
        self.block = elem.attrib.get("block", "")
        self.final = elem.attrib.get("final", "")

        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.simple_contents = get_elements(elem, SimpleContentElement, "simpleContent")
        self.complex_contents = get_elements(elem, ComplexContentElement, "complexContent")
        self.groups = get_elements(elem, GroupElement, "group")
        self.alls = get_elements(elem, AllElement, "all")
        self.choices = get_elements(elem, ChoiceElement, "choice")
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.attributes = get_elements(elem, AttributesElement, "attribute")
        self.attribute_groups = get_elements(elem, AttributeGroupElement, "attributeGroup")
        self.any_attributes = get_elements(elem, AnyAttributeElement, "anyAttribute")
