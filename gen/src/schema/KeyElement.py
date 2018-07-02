

from ..utils import * 

class KeyElement:
    def __init__(self, elem):
        self.name = elem.attrib.get("name", "")
