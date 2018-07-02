
from ..utils import * 
from .AnnotationElement import * 
from .SimpleTypeElement import *
from .ComplexTypeElement import * 
from .UniqueElement import *
from .KeyElement import * 
from .KeyRefElement import * 

class Element:
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.name = elem.attrib.get("name", "")
        self.ref = elem.attrib.get("ref", "")
        self.type = elem.attrib.get("type", "")
        self.substitution_group = elem.attrib.get("substitutionGroup", "")
        self.default = elem.attrib.get("default", "")
        self.fixed = elem.attrib.get("fixed", "")
        self.form = elem.attrib.get("form", "")
        self.max_occurs = elem.attrib.get("maxOccurs", "")
        self.min_occurs = elem.attrib.get("minOccurs", "")
        self.nillable = elem.attrib.get("nillable", "")
        self.abstract = elem.attrib.get("abstract", "")
        self.block = elem.attrib.get("block", "")
        self.final = elem.attrib.get("final", "")

        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.simple_types = get_elements(elem, SimpleTypeElement, "simpleType")
        self.complex_types = get_elements(elem, ComplexTypeElement, "complexType")
        self.uniques = get_elements(elem, UniqueElement, "unique")
        self.keys = get_elements(elem, KeyElement, "key")
        self.key_refs = get_elements(elem, KeyRefElement, "keyref")
