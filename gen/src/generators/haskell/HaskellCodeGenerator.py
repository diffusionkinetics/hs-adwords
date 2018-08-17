
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
            code("serviceUrl %s = \"%s\"" % (service.name, list(service.ports.values())[0].binding_options["address"]))
        return str(code)

    def map_type(self, t):
        if not ":" in t:
            t = ":" + t
        return {
            "string": "String",
            "int": "Int",
            "long": "Int"
        }.get(t[t.find(":")+1:], t[t.find(":")+1:])

    def complex_type(self, parser, code, ct):
        fields = {}
        for sequence in ct.sequences:
            for e in sequence.elements:
                fields[e.name] = self.map_type(e.type if e.type != "" else e.simple_types[0].name)
        if len(ct.complex_contents) > 0:
            for extension in ct.complex_contents[0].extensions:
                base = [c for schema in parser.get_schemas() for c in schema.complex_types if c.name == extension.base[extension.base.find(":")+1:]][0]
                for sequence in base.sequences:
                    for e in sequence.elements:
                        fields[e.name] = self.map_type(e.type if e.type != "" else e.simple_types[0].name)
        code.type(ct.name, [(ct.name, fields)])
        code("instance ToXML %s where" % ct.name)
        code("  toXML e = element \"%s\" %s" % (ct.name, "do" if fields != {} else ""))
        for k,v in fields.items():
            code("    element \"%s\" $ content e.%s" % (k,k))
        code("instance FromXML %s where" % ct.name)
        code("  parseIt :: Text -> %s" % ct.name)
        code("  parseIt text = case P.parseText def text of ")
        code("    Left err -> Left ParseError")
        code("    Right doc -> Right (parse_%s doc)" % ct.name.lower())
        fn = "parse_%s" % ct.name.lower()
        code("%s :: P.Document -> %s" % (fn, ct.name))
        code("%s (P.Document _ (P.Element _ _ %s) _) = %s %s" % (fn, ":".join(["(NodeContent %s)" % f for f in fields.keys()]) + ":xs" if fields != {} else "[]", 
            ct.name, 
            " ".join(fields.keys())
        ))


    def parse_schemas(self, parser):
        code = HaskellCodeBuilder()
        code("data ParseError = ParseError")
        code("class FromXML a where")
        code("  parseIt :: Text -> Eiither ParseError a")
        for schema in parser.get_schemas():
            for ct in schema.complex_types:
                self.complex_type(parser, code, ct)
        return str(code)
