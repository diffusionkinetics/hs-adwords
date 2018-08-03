
from ..CodeGenerator import * 
from .HaskellCodeBuilder import * 

class HaskellCodeGenerator(CodeGenerator):
        
    def parse_services(self, parser):
        code = HaskellCodeBuilder()
        code.type("Service", [(service.name, {}) for service in parser.get_services()])
        code.function("serviceUrl", {
            "service": "Service"
        }, "Text")
        for service in parser.get_services():
            code("serviceUrl %s = %s" % (service.name, list(service.ports.values())[0].binding_options["address"]))
        return str(code)

    def parse_schemas(self, parser):
        code = HaskellCodeBuilder()
        for schema in parser.get_schemas():
            for ct in schema.complex_types:
                fields = {}
                for sequence in ct.sequences:
                    for e in sequence.elements:
                        fields[e.name] = e.type
                code.type(ct.name, [(ct.name, fields)])
        return str(code)
