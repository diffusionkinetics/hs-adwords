
from ..utils import * 

class ImportElement:
    def __init__(self, elem):
        self.schema_location = elem.attrib.get("schemaLocation", "")
        self.namespace = elem.attrib.get("namespace", "")
