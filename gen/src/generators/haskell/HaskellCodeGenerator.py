
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
        t = t.replace(".", "")
        if not ":" in t:
            t = ":" + t
        return {
            "string": "String",
            "int": "Int",
            "long": "Int"
        }.get(t[t.find(":")+1:], t[t.find(":")+1:])

    def map_field_name(self, name):
        return name[0].lower() + name[1:].replace(".", "")

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
        fields = {self.map_field_name(k): v for k,v in fields.items()}
        code.type(ct.name, [(ct.name, fields)])
        code("instance ToXML %s where" % ct.name)
        code("  toXML e = element \"%s\" %s" % (ct.name, "$ do" if fields != {} else ""))
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

    def simple_type(self, parser, code, st):
        if len(st.restrictions) > 0:
            restriction = st.restrictions[0]
            code("type %s = %s" % (st.name, self.map_type(restriction.base)))
        elif len(st.lists) > 0:
            l = st.lists[0]
            if l.item_type != "":
                code("type %s = [%s]" % (st.name, self.map_type(l.item_type)))
            else:
                self.simple_type(parser, code, l.simple_types[0])
                code("type %s = [%s]" % (st.name, self.map_type(l.simple_types[0].name)))
        else: # union
            union = st.unions[0]
            if union.member_types != "":
                code("data %s = %s" % (st.name, " | ".join(["%sAs%s %s" (st.name, t, t) for t in union.member_types.split(" ")])))
            else:
                names = [s.name for s in union.simple_types]
                for s in union.simple_types:
                    self.simple_type(parser, code, s)
                code("data %s = %s" % (st.name, " | ".join(["%sAs%s %s" (st.name, t, t) for t in names])))


    def parse_schemas(self, parser):
        code = HaskellCodeBuilder()
        code("module Schema where")

        code("import Text.XML.Writer (element, document, elementA, ToXML(..), content, pprint)")
        code("import Text.XML as P")
        code("import Data.Text ")
        code("data ParseError = ParseError")
        code("class FromXML a where")
        code("  parseIt :: Text -> Eiither ParseError a")
        for schema in parser.get_schemas():
            for ct in schema.complex_types:
                self.complex_type(parser, code, ct)
            for st in schema.simple_types:
                self.simple_type(parser, code, st)
        return str(code)
