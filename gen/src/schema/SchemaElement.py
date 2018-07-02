
from ..utils import * 
from .IncludeElement import * 
from .ImportElement import * 
from .RedefineElement import * 
from .AnnotationElement import * 
from .SimpleTypeElement import * 
from .ComplexTypeElement import * 
from .GroupElement import *
from .AttributeGroupElement import * 
from .Element import * 
from .AttributeElement import * 
from .NotationElement import *

class SchemaElement:
    def __init__(self, elem):
        self.attribute_form_default = elem.attrib.get("attributeFormDefault", "unqualified")
        self.element_form_default = elem.attrib.get("elementFormDefault", "unqualified")
        self.block_default = elem.attrib.get("blockDefault")
        self.final_default = elem.attrib.get("finalDefault")
        self.target_namespace = elem.attrib.get("targetNamespace", "")

        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.includes = get_elements(elem, IncludeElement, "include")
        self.imports = get_elements(elem, ImportElement, "import")
        self.redefines = get_elements(elem, RedefineElement, "redefine")
        self.simple_types = get_elements(elem, SimpleTypeElement, "simpleType")
        self.complex_types = get_elements(elem, ComplexTypeElement, "complexType")
        self.groups = get_elements(elem, GroupElement, "group")
        self.attribute_groups = get_elements(elem, AttributeGroupElement, "attributeGroup")
        self.elements = get_elements(elem, Element, "element")
        self.attributes = get_elements(elem, AttributeElement, "attribute")
        self.notations = get_element(elem, NotationElement, "notation")

