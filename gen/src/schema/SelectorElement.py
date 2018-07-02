
from .SelectorElement import * 
from ..utils import * 

class SelectorElement:
    def __init__(self, elem):
        self.xpath = elem.attrib.get("xpath", "")
