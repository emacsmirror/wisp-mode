#!/usr/bin/env python3

class Line:
    @property
    def base(self):
        if self.sublevels:
            return self.sublevels[0]
        return self.indent

    @property
    def levels(self):
        return self.sublevels + [self.indent]
    
    def __init__(self, line):
        self.continues = line.lstrip().startswith(". ")
        if self.continues:
            self.content = line.lstrip()[2:].lstrip()
        else:
            self.content = line.lstrip()
        # the indentation of the code
        self.indent = len(line) - len(line.lstrip())
        # due to : vertical space optimization, the line can have multiple sublevels
        # TODO make this work.
        self.sublevels = []
        while self.content.startswith(": ") and self.content[2:].lstrip():
            self.sublevels.append(self.indent)
            self.indent += len(self.content) - len(self.content[2:].lstrip())
            self.content = self.content[2:].lstrip()
        if self.content.strip() == ":":
            self.content = ""

def wisp2lisp(code):
    """Turn wisp code to lisp code."""
    # first get rid of linebreaks in strings
    instring = False
    nostringbreaks = []
    for char in code:
        if char == '"':
            instring = not instring
        if instring and char == "\n":
            nostringbreaks.append("\\n")
        else:
            nostringbreaks.append(char)
    code = "".join(nostringbreaks)
    
    # now read the indentation
    lines = []
    for line in code.splitlines():
        lines.append(Line(line))

    # finally emit matching lisp code
    # write into the lisp lines with a delay of 1 line
    lisplines = []
    levels = []
    prev = lines[0]
    if not prev.continues:
        prev.content = "(" + prev.content
    for line in lines[1:]:
        # continuing lines do not get a leading bracket.
        if not line.continues:
            line.content = "(" + line.content
        # rising indent: sibling function or variable
        if line.base > prev.indent:
            levels.extend(line.levels)
            lisplines.append(prev.indent * " " + prev.content)
        # same indent: neighbour function of variable: close the previour lines bracket
        elif line.base == prev.indent:
            lisplines.append(prev.indent * " " + prev.content + ")")
            for l in line.levels:
                if l > levels[-1]:
                    levels.append(l)
        # lower indent: parent funtion or variable. Find the number of brackets to close
        elif prev.indent > line.base:
            bracketstoclose = len([level for level in levels if level >= line.indent])
            levels = levels[:-bracketstoclose + 1]
            for l in line.levels:
                if not levels or l > levels[-1]:
                    levels.append(l)
            if prev.continues:
                bracketstoclose -= 1
            lisplines.append(prev.indent * " " + prev.content + ")" * bracketstoclose)
        
        prev = line
    
    lisplines.append(prev.indent * " " + prev.content + ")" * (len(levels)))
    
    return "\n".join(lisplines)
            


if __name__ == "__main__":
    print()
    with open("example.w") as f:
        wisp = f.read()
    print(wisp2lisp(wisp))
        
