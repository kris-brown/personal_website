 # External
from typing  import Any, List as L, Tuple as T, Optional as O, Callable as C
from abc     import ABCMeta,abstractmethod
from os.path import join
from os      import environ
from re      import match, sub
# Internal
from utils import flatten, Tree
from connect import Card
from PyOrgMode import OrgDataStructure,OrgNode # type: ignore
Element = Any
################################################################################
################################################################################

def heading(x:Element)->str: return x.heading.strip()

class CardType(object, metaclass = ABCMeta):
    def __init__(self,chap:str,sect:str,page:str=None)->None:
        self.chap = chap
        self.sect = sect
        self.page = page or ''

    @staticmethod
    def parseall(root:OrgNode)->L['CardType']:

        assert hasattr(root,'heading'), root
        head    = heading(root)
        phead   = heading(root.parent)
        gphead  = heading(root.parent.parent)
        ggphead = heading(root.parent.parent.parent)
        goodtags = lambda x: ('nocard' not in x.tags) and ('todo' not in x.tags) # type: C[[Element],bool]
        i = lambda xs: [x for x in xs.content if goodtags(xs)] # type: C[[Element],L[Element]]
        try:
            if   head == 'Definitions':
                return [Def.parse(x,chap=ggphead,sect=gphead,part=phead) for x in i(root) if goodtags(x)]
            elif head == 'Propositions':
                return [Prop.parse(x,chap=ggphead,sect=gphead,part=phead) for x in i(root) if goodtags(x)]
            elif head  == 'Examples':
                return [Example.parse(x,chap=ggphead,sect=gphead,part=phead) for x in i(root) if goodtags(x)]
            elif phead == 'Exercises':
                return [Exercise.parse(root,chap=ggphead,sect=gphead)] if goodtags(root) else []
            elif head == 'Notes': return []
            else:
                import pdb;pdb.set_trace()
                raise NotImplementedError('New kind of section? ',root.parent.heading,root.heading,root.content)
        except Exception as e:
            print('\n'.join([str(e),ggphead,gphead,phead,head]));assert False
    @property
    @abstractmethod
    def ID(self)->str: raise NotImplementedError
    @property
    @abstractmethod
    def tag(self)->str: raise NotImplementedError
    @property
    @abstractmethod
    def front(self)->str: raise NotImplementedError
    @property
    @abstractmethod
    def back(self)->str: raise NotImplementedError
    @property
    @abstractmethod
    def part(self)->str: raise NotImplementedError

    def cloze(self)->bool:
        if '##' in (self.front + self.back):
            assert all([x.count('##')%2==0 for x in [self.front,self.back]])
            return True
        return False

    def add(self)->None:
        '''Add to Anki'''
        fields = dict(
            Title    = self.ID,
            Chapter  = self.chap,
            Section  = self.sect,
            Part     = self.part,
            Page     = str(self.page))

        if self.cloze():
            text = self.format(self.front,cloze=True) + '\n<hr>\n' + self.format(self.back,cloze=True)
            fields['Text'] = text
            modelName='Cloze'
        else:
            fields['Front'] = self.format(self.front)
            fields['Back'] = self.format(self.back)
            modelName='Basic'

        card = Card(fields=fields,tags=[self.tag])
        card.add(modelName=modelName)

    @staticmethod
    def format(s : str, cloze : bool = False)->str:
        '''Take org mode's markdown and make Anki's pseudo-html'''
        # Replace $...$ with [$$]...[/$$]
        s1 = sub(r'\$(.+?)\$',r'[$$]\1[/$$]',s)
        s2 = sub(r'\/([\w|()!-=><]+)\/',r'<i>\1</i>',s1)
        # Replace linebreaks, and more?
        rdict = {'\n' : '<br />',}
        for a,b in rdict.items(): s2 = s2.replace(a,b)
        if cloze:
            s3 = sub(r'##(.+?)##',r'{{c1::\1}}',s2)
            return s3
        else:
            return s2

    @staticmethod
    def keytitle(s:Element)->T[str,O[str]]:
        key  = s.heading.replace('<','').replace('>','').strip()
        pagestr  = list(filter(str.isdigit,s.tags))
        page     = pagestr[0] if pagestr else None
        return key,page

class Example(CardType):
    def __init__(self, x : Tree, chap : str, sect : str, part : str, key : str) -> None:
        self.x = x; self._part = part; self.key = key
        super().__init__(chap,sect)
        if not self.cloze():
            print(self.key) ;import pdb;pdb.set_trace()
        assert self.cloze()
        assert self.key[0]=='X'

    @property
    def ID(self)->str: return self.key
    @property
    def tag(self)->str: return 'Example'
    @property
    def front(self)->str: return self.x.showflat()
    @property
    def back(self)->str: return ''
    @property
    def part(self)->str: return self._part
    @staticmethod
    def parse(root : OrgNode, chap: str,sect:str,part:str) -> 'CardType':
        key  = root.heading.replace('<','').replace('>','').strip()
        return Example(x=Tree.from_str(root.content),
                        chap=chap,sect=sect,part=part,key=key)

class Exercise(CardType):
    def __init__(self,q:Tree,a:Tree,chap:str,sect:str,key:str,page:str=None) -> None:
        self.q = q; self.a = a; self.key = key
        super().__init__(chap,sect,page)
        assert self.key[0]=='E'
    @property
    def ID(self)->str: return self.key
    @property
    def tag(self)->str: return 'Ex'
    @property
    def front(self)->str: return self.q.showflat()
    @property
    def back(self)->str: return self.a.showflat()
    @property
    def part(self)->str: return 'Exercises'

    @classmethod
    def parse(cls,root : OrgNode, chap: str,sect:str) -> 'CardType':
        key,page = cls.keytitle(root)
        q,a  = root.content
        assert q.heading.strip() == 'Question', q.heading
        assert a.heading.strip() == 'Solution', a.heading
        return Exercise(q=Tree.from_str(q.content),a=Tree.from_str(a.content),
                        chap=chap,sect=sect,key=key,page=page)

class Def(CardType):
    def __init__(self,term:Tree,def_:Tree,chap:str,sect:str,part:str,key:str,page:str=None) -> None:
        self.term = term; self.def_ = def_;
        super().__init__(chap,sect,page)
        self.key = key; self._part = part
        assert self.key[0]=='D'
    @property
    def ID(self)->str: return self.key
    @property
    def tag(self)->str: return 'Def'
    @property
    def front(self)->str: return self.term.showflat()
    @property
    def back(self)->str: return self.def_.showflat()
    @property
    def part(self)->str: return self._part
    @classmethod
    def parse(cls, root : OrgNode,chap:str,sect:str,part:str) -> 'CardType':
        key,page = cls.keytitle(root)
        term,def_ = root.content
        assert term.heading.strip() == 'Term', term.heading
        assert def_.heading.strip() == 'Def', def_.heading
        return Def(term=Tree.from_str(term.content),def_=Tree.from_str(def_.content),
                    chap=chap,sect=sect,part=part,key=key,page=page)

class Prop(CardType):
    def __init__(self, prop : Tree, proof : Tree, chap:str,sect:str,part:str,key:str,page:str=None) -> None:
        self.prop = prop; self.proof = proof;
        self.key = key; self._part = part
        super().__init__(chap,sect,page)
        assert self.key[0]=='P'

    @property
    def ID(self)->str: return self.key
    @property
    def tag(self)->str: return 'Prop'
    @property
    def front(self)->str: return self.prop.showflat()
    @property
    def back(self)->str: return self.proof.showflat()
    @property
    def part(self)->str: return self._part

    @classmethod
    def parse(cls, root : OrgNode,chap:str,sect:str,part:str) -> 'CardType':
        key,page = cls.keytitle(root)
        pp,pf = root.content
        assert pp.heading.strip() == 'Proposition'
        assert pf.heading.strip() == 'Proof'
        return Prop(prop=Tree.from_str(pp.content),proof=Tree.from_str(pf.content),
                    chap=chap,sect=sect,part=part,key=key,page=page)

################################################################################
################################################################################

def parse_aluffi()->L[CardType]:
    '''Parses orgmode file with a very specific structure'''
    home = environ['HOME']
    pth  = join(home,'aluffi.org')
    base = OrgDataStructure()
    base.load_from_file(pth)
    chaps = filter(lambda x: isinstance(x,OrgNode.Element),base.root.content)
    cards = [] # type: L[CardType]
    for chap in chaps:
        if chap.heading.lower().strip() not in ['info','local variables']:
            for section in chap.content:
                for part in section.content:
                    #print('\n\t',part.heading,part.content)
                    #import pdb;pdb.set_trace()
                    cards.extend(flatten([CardType.parseall(x) for x in part.content]))
    return cards
if __name__=='__main__':
    parse_aluffi()
