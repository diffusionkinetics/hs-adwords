
import re


def clear_tag(elem):
        try:
            rbr = elem.tag.rfind('}') + 1
            tag_clear = elem.tag[rbr:]
            return tag_clear
        except AttributeError:
            return ""

def get_node(tree, node):
    for elem in tree.iter():
        tag_clear = clear_tag(elem)
        if tag_clear == node:
            return elem
    return None

def get_nodes(tree, node):
    res = []
    for elem in tree.iter():
        tag_clear = clear_tag(elem)
        if tag_clear == node:
            res.append(elem)
    return res

def to_clear_text(e, replacements = {}, catch_attr={}, ignore_tail = True):
    """
    replacements: dictionary of replacements where key is tagname and value is tuple which contains text before and after contents of tag
    """
    text = ""
    tail = ""
    if e.text != None:
        text = e.text
    if e.tail != None and not ignore_tail:
        tail = e.tail.strip()
    return put_replacement_start(e, replacements) + text + "".join(to_clear_text(c, replacements, catch_attr, False) for c in e.getchildren()) + put_replacement_end(e, replacements) + catch_attributes(e, catch_attr) + tail

def get_elements(elem, class_name, tag):
    return [class_name(e) for e in elem.getchildren() if clear_tag(e) == tag]
