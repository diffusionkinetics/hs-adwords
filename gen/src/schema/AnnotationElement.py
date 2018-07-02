
from ..utils import * 
from .DocumentationElement import * 
from .AppInfoElement import * 

class AnnotationElement:
    
    def __init__(self, elem):
        self.documentations = [DocumentationElement(e) for e in elem.getchildren() if clear_tag(e) == "documentation"]
        self.appinfos = [AppInfoElement(e) for e in elem.getchildren() if clear_tag(e) == "appinfoX"]
