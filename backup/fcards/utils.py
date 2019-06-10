from typing  import List as L, Tuple as T

'''Misc helpful things'''

def flatten(lol:L[list])->list:
    return [item for sublist in lol for item in sublist]
################################################################################

class Tree(object):
    '''
    Parse a nested bullet structure where nesting is determined by whitespace, e.g.
    - A
       - A1
       - A2
            - A2i
    - B
    '''
    def __init__(self, value : str, children : L['Tree']) -> None:
        self.value = value; self.children = children
    def __str__(self)->str:
        return self.print(0)
    def __len__(self)->int: return 1 + sum(map(len,self.children))

    def showflat(self,_discard : bool = True)->str:
        '''Discard root and show flattened information (for Anki cards)'''
        if _discard: curr = ''
        else:
            if self.value and self.value[0]=='-': curr = '\n'+self.value[1:]
            else: curr = '\n'+self.value
        return curr + ''.join([c.showflat(_discard=False) for c in self.children])

    def print(self, indent : int) -> str:
        '''Visualize as tree'''
        rest = ''.join([c.print(indent+1) for c in self.children])
        return '\n' + '\t'*indent + self.value + rest

    @classmethod
    def from_str(cls, lines:L[str]) -> 'Tree':
        '''Takes the "content" of an orgmode node (list of strings) and makes a Tree'''
        pairs = [(cls.level(x),x) for x in filter(lambda x: not x.isspace(),lines)]
        try:
            root  = Tree(value = 'root', children = cls.parse_children(pairs))
        except ValueError as e:
            print(e)
            for k,v in pairs: print(k,v)
            import pdb;pdb.set_trace();assert False
        return root

    @classmethod
    def parse_children(cls, pairs : L[T[int,str]]) -> L['Tree']:
        '''Recursively parse a list of (indent-level, <content>) pairs'''
        if not pairs: return [] # Base case: no more children
        next_val   = pairs[0][1].strip() # our first element is definitely a child.
        childlevel = pairs[0][0]         # All children have this indentation level
        children   = [] # The list that we will return
        next_pairs = [] # type: L[T[int,str]] ## the lines that are descendents of the child
        for i,x in pairs[1:]:
            if   i < childlevel:  raise ValueError('Indentation level incorrectly parsed: ',x)
            elif i > childlevel:  next_pairs.append((i,x)) # something that belongs to next child at some depth
            else:
                # We've returned back to the child indentation level, so everything we've seen up to now gets added
                children.append(Tree(value   = next_val, children = cls.parse_children(next_pairs)))
                next_val, next_pairs = x.strip(), [] # reset these variables

        # Add the last tree
        children.append(Tree(value=next_val,children = cls.parse_children(next_pairs)))
        return children

    @staticmethod
    def level(astr : str) -> int:
        '''Get indentation level assuming tab spacing = 8'''
        ws = astr[:len(astr) - len(astr.lstrip())]
        return ws.count(' ')+8*ws.count('\t')
