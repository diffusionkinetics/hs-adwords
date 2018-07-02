

from collections import * 

Message = namedtuple("Message", ["name", "fields"])

Endpoint = namedtuple("Endpoint", ["uri", "input_message", "output_message"])

Element = namedtuple("Element", ["name", "type"])

Type = namedtuple("Type", "name")

ComplexElement = namedtuple("ComplexElement", ["name", "elements"])

SequenceElement = namedtuple("SequenceElement", ["name", "type"])

Service = namedtuple("Service", ["endpoints"])

