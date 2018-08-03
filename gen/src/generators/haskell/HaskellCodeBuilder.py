
class HaskellCodeBuilder:

    def __init__(self):
        self.code = "" 

    def constructor(self, name, fields):
        code = ""
        code += "%s " % name 
        if fields:
            code += "{ "
        code += "\n\t, ".join(["%s :: %s" % (field, typename) for field,typename in fields.items()])
        if fields:
            code += " }"
        return code

    def type(self, name, constructors):
        """
        name: name of a type 
        constructors: list of tuples (constructor_name, fields) where fields is dictionary of mapping field name to typename 
        """
        self.code += "\n\ndata %s = " % name 
        self.code += " | ".join([self.constructor(cname, cfields) for cname, cfields in constructors])
        self.code += " deriving (Show, Generic)"

    def function(self, name, args, result, expression=None):
        """
        namme: name of a function
        args: dictionary mapping argument name to its type
        result: string representing typename
        """
        self.code += "\n%s :: %s\n" % (name, " -> ".join([t for n,t in args.items()] + [result]))
        if expression is not None:
            self.code += "%s %s = %s\n\n" % (name, " ".join([n for n,t in args.items()]), expression)

    def get_code(self):
        return self.code

    def __str__(self):
        return self.code

    def __call__(self, code):
        self.code += "\n%s\n" % code
