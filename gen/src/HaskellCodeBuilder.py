
class HaskellCodeBuilder:

    def __init__(self):
        self.code = "" 

    def constructor(self, name, fields):
        code = ""
        code += "%s { " % name 
        code += "\n\t, ".join(["%s :: %s" % (field, typename) for field,typename in fields.items()])
        code += " }"
        return code

    def type(self, name, constructors):
        """
        name: name of a type 
        constructors: list of tuples (constructor_name, fields) where fields is dictionary of mapping field name to typename 
        """
        self.code += "data %s = " % name 
        self.code += " | ".join([self.constructor(cname, cfields) for cname, cfields in constructors])
        self.code += " deriving (Show, Generic)"
