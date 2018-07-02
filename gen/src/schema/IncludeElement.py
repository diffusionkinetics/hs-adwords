

from ..utils import * 

class IncludeElement:
    def __init__(self, elem):
        self.schema_location = elem.attrib.get("schemaLocation", "")
