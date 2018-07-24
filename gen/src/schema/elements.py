
from ..utils import * 


class SchemaElement:
    def haskell_code(self):
        raise NotImplementedError


class AllElement(SchemaElement):
    
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.max_occurs = elem.attrib.get("maxOccurs", 1)
        self.min_occurs = elem.attrib.get("minOccurs", 1)
        self.annotations = [AnnotationElement(a) for a in elem.getchildren() if clear_tag(a) == "annotation"]
        self.elements = [Element(a) for a in elem.getchildren() if clear_tag(a) == "element"]
    def haskell_code(self):
        return ""


class AnnotationElement(SchemaElement):
    
    def __init__(self, elem):
        self.documentations = [DocumentationElement(e) for e in elem.getchildren() if clear_tag(e) == "documentation"]
        self.appinfos = [AppInfoElement(e) for e in elem.getchildren() if clear_tag(e) == "appinfoX"]
    def haskell_code(self):
        return ""


class AnyAttributeElement(SchemaElement):
    def __init__(self, elem):
        self.annotations = [AnnotationElement(e) for e in elem.getchildren() if clear_tag(e) == "annotation"]
    def haskell_code(self):
        return ""


class AnyElement(SchemaElement):
    def __init__(self, elem):
        self.min_occurs = elem.attrib.get("minOccurs", 1)
        self.maxOccurs = elem.attrib.get("maxOccurs", 1)
        self.annotations = [AnnotationElement(e) for e in elem.getchildren() if clear_tag(e) == "annotation"]
    def haskell_code(self):
        return ""


class AppInfoElement(SchemaElement):
    
    def __init__(self, elem):
        self.text = elem.text
    def haskell_code(self):
        return ""


class AttributeElement(SchemaElement):
    def __init__(self, elem):
        self.default = elem.attrib.get("default", "")
        self.fixed = elem.attrib("fixed", "")
        self.form = elem.attrib.get("form", "")
        self.id = elem.attrib("id", "")
        self.name = elem.attrib.get("name", "")
        self.ref = elem.attrib.get("ref", "")
        self.type = elem.attrib.get("type", "")
        self.use = elem.attrib.get("use", "")
        self.annotations = [AnnotationElement(e) for e in elem.getchildren() if clear_tag(e) == "annotation"]
        self.simple_types = [SimpleTypeElement(e) for e in elem.getchildren() if clear_tag(e) == "simpleType"]
    def haskell_code(self):
        return ""

class AttributeGroupElement(SchemaElement):
    def __init___(self, elem):
        self.attributes = get_elements(AttributeElement, "attribute")
        self.annotations = get_elements(AnnotationElement, "annotation")
        self.attribute_groups = get_elements(AttributeGroupElement, "attributeGroup")
        self.any_attributes = get_elements(elem, AnyAttributeElement, "anyAttribute")
    def haskell_code(self):
        return ""


class ChoiceElement(SchemaElement):
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.elements = get_elements(elem, Element, "element")
        self.groups = get_elements(elem, GroupElement, "group")
        self.choices = get_elements(elem, "choice", ChoiceElement)
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.anys = get_elements(elem, AnyElement, "any")
        self.max_occurs = elem.attrib.get("maxOccurs", "unbounded")
        self.min_occurs = elem.attrib.get("minOccurs", 0)
    def haskell_code(self):
        return ""


class ComplexContentElement(SchemaElement):
    
    def __init__(self, elem):
        self.mixed = elem.attrib.get("mixed", False)
        self.id = elem.attrib.get("id", "")
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.restrictions = get_elements(elem, RestrictionElement, "restriction")
        self.extensions = get_elements(elem, ExtensionElement, "extension")

    def haskell_code(self):
        return ""

class ComplexTypeElement(SchemaElement):
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
        self.attributes = get_elements(elem, AttributeElement, "attribute")
        self.attribute_groups = get_elements(elem, AttributeGroupElement, "attributeGroup")
        self.any_attributes = get_elements(elem, AnyAttributeElement, "anyAttribute")
    def haskell_code(self):
        return ""

class DocumentationElement(SchemaElement):
    def __init__(self, elem):
        self.text = elem.text

    def haskell_code(self):
        return ""

class Element(SchemaElement):
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
    def haskell_code(self):
        return ""


class ExtensionElement(SchemaElement):
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
    def haskell_code(self):
        return ""


class Field(SchemaElement):
    def __init__(self, elem):
        pass
    def haskell_code(self):
        return ""


class GroupElement(SchemaElement):
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.choices = get_elements(elem, ChoiceElement, "choice")
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.alls = get_elements(elem, AllElement, "all")
        self.name = elem.attrib.get("name", "")
        self.id = elem.attrib.get("id", "")
    def haskell_code(self):
        return ""


class ImportElement(SchemaElement):
    def __init__(self, elem):
        self.schema_location = elem.attrib.get("schemaLocation", "")
        self.namespace = elem.attrib.get("namespace", "")
    def haskell_code(self):
        return ""



class IncludeElement(SchemaElement):
    def __init__(self, elem):
        self.schema_location = elem.attrib.get("schemaLocation", "")
    def haskell_code(self):
        return ""



class KeyElement(SchemaElement):
    def __init__(self, elem):
        self.name = elem.attrib.get("name", "")
    def haskell_code(self):
        return ""


class KeyRefElement(SchemaElement):
    def __init__(self, elem):
        self.name = elem.attrib.get("name", "")
        self.refer = elem.attrib.get("refer", "")

    def haskell_code(self):
        return ""


class ListElement(SchemaElement):
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.simple_types = get_elements(elem, SimpleType, "simpleType")
        self.item_type = elem.attrib.get("itemType", "")
    def haskell_code(self):
        return ""

class NotationElement(SchemaElement):
    def __init__(self, elem):
        pass
    def haskell_code(self):
        return ""

class RedefineElement(SchemaElement):
    def __init__(self, elem):
        pass
    def haskell_code(self):
        return ""


class RestrictionElement(SchemaElement):
    def __init__(self, elem):
        self.base = elem.attrib["base"]
    def haskell_code(self):
        return ""

class Schema(SchemaElement):
    def __init__(self, elem):
        self.annotations = get_elements(elem, AnnotationElement, "annotation")
        self.includes = get_elements(elem, IncludeElement, "include")
        self.imports = get_elements(elem, ImportElement, "import")
        self.redefines = get_elements(elem, RedefineElement, "redefine")
        self.simple_types = get_elements(elem, SimpleTypeElement, "simpleType")
        self.complex_types = get_elements(elem, ComplexTypeElement, "complexType")
        self.groups = get_elements(elem, GroupElement, "group")
        self.attribute_groups = get_elements(elem, AttributeGroupElement, "attributeGroupp")
        self.elements = get_elements(elem, Element, "element")
        self.attributes = get_elements(elem, AttributeElement, "attribute")
        
        self.id = elem.attrib.get("id", "")

    def haskell_code(self):
        return ""

class SelectorElement(SchemaElement):
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
    def haskell_code(self):
        return ""

class SequenceElement(SchemaElement):
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.max_occurs = elem.attrib.get("maxOccurs", 1)
        self.min_occurs = elem.attrib.get("minOccurs", 1)
        self.elements = get_elements(elem, Element, "element")
        self.groups = get_elements(elem, GroupElement, "group")
        self.choices = get_elements(elem, ChoiceElement, "choice")
        self.sequences = get_elements(elem, SequenceElement, "sequence")
        self.anys = get_elements(elem, AnyElement, "any")
    def haskell_code(self):
        return ""

class SimpleContentElement(SchemaElement):
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.restrictions = get_elements(elem, RestrictionElement, "restriction")
        self.extensions = get_elements(elem, ExtensionElement, "extension")
    def haskell_code(self):
        return ""

class SimpleTypeElement(SchemaElement):
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.name = elem.attrib.get("name", "")
        self.restrictions = get_elements(elem, RestrictionElement, "restriction")
        self.lists = get_elements(elem, ListElement, "list")
        self.unions = get_elements(elem, UnionElement, "union")
    def haskell_code(self):
        return ""

class UnionElement(SchemaElement):
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.member_types = elem.attrib.get("memberTypes", "")
        self.simple_types = get_elements(elem, SimpleTypeElement, "simpleType")
    def haskell_code(self):
        return ""

class UniqueElement(SchemaElement):
    def __init__(self, elem):
        self.id = elem.attrib.get("id", "")
        self.name = elem.attrib.get("name", "")
        self.selectors = get_elements(elem, SelectorElement, "selector")
        self.fields = get_elements(elem, FieldElement, "field")
    def haskell_code(self):
        return ""
