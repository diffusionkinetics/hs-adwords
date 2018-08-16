
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

    def map_type(self, t):
        if not ":" in t:
            t = ":" + t
        return {
            "string": "String",
            "int": "Int",
            "long": "Int"
        }.get(t[t.find(":")+1:], t[t.find(":")+1:])

    def complex_type(self, code, ct):
        fields = {}
        for sequence in ct.sequences:
            for e in sequence.elements:
                fields[e.name] = self.map_type(e.type)
        code.type(ct.name, [(ct.name, fields)])
        code("instance ToXML %s where" % ct.name)
        code("  toXML e = element \"%s\" %s" % (ct.name, "do" if fields != {} else ""))
        for k,v in fields.items():
            code("    element \"%s\" $ content e.%s" % (k,k))
        code("instance FromXML %s where" % ct.name)
        code("  parseIt :: P.Document ")
        code("  parseIt = case P.parseText def text of ")
        code("    Left err -> P.parseText_ def \"<haha>lol</haha>\"")
        code("    Right doc -> doc")

    def parse_schemas(self, parser):
        code = HaskellCodeBuilder()
        code("class FromXML a where")
        code("  parseIt :: Text -> a")
        for schema in parser.get_schemas():
            for ct in schema.complex_types:
                self.complex_type(code, ct)
        return str(code)
