
from ..utils import * 

class KeyRefElement:
    def __init__(self, elem):
        self.name = elem.attrib.get("name", "")
        self.refer = elem.attrib.get("refer", "")

