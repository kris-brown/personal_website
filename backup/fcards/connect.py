# External
from typing         import Any,Set as S, List as L, Union as U
from json           import dumps, load
from urllib.request import urlopen

###############################################################################
def request(action : str, **params : Any) -> dict:
    return {'action': action, 'params': params, 'version': 6}

def invoke(action : str, **params : Any) -> dict:
    requestJson = dumps(request(action, **params)).encode()
    response = load(urlopen('http://localhost:8765', data = requestJson))
    print(requestJson)
    assert set(response.keys())==set(['error','result']), 'Unexpected result fields: '+str(response.keys())
    if response['error'] is not None:
        raise Exception(response['error'])
    return response['result']
###############################################################################
class Card(object):
    def __init__(self,fields:dict,tags : L[str] = None,css:str=None)->None:
        self.fields = fields; self.tags = tags or []
        self.css = css or self._cssdefault

    @property
    def _cssdefault(self)->str: return '.card {\n font-family: times new roman;\n font-size: 30px;\n text-align: center;\n color: black;\n background-color: white;\n line-height: 40px;\n}\n'

    def add(self,deckName:str='aluffi',modelName:str='Basic',)->None:
        try:
            invoke('addNote',note = dict(deckName  = deckName,
                                         modelName = modelName,
                                         fields    = self.fields,
                                         tags      = self.tags))
        except Exception as e:
            assert str(e)=='cannot create note because it is a duplicate', e
            noteids = invoke('findNotes',query='Title:'+self.fields['Title'])
            if len(noteids)!=1:
                print(invoke('notesInfo',notes=noteids));import pdb;pdb.set_trace() #-- see state right before modification

            assert len(noteids)==1, noteids
            invoke("updateNoteFields",note=dict(deckName  = deckName, modelName = modelName, id=noteids[0], fields=self.fields))

    @staticmethod
    def from_id(id:U[int,L[int]])->L['Card']:
        result = invoke('cardsInfo',cards=[id] if isinstance(id,int) else id)
        return [Card(fields=card['fields'],css=card['css']) for card in result]


def removeUnseen(seen:S[str])->None:
    '''Removes any cards that have a Title attribute not found in argument seen'''
    cardIDs =  invoke('findCards',query='deck:aluffi')
    delete,deleteNames  = [],[]
    for res in invoke('cardsInfo',cards=cardIDs):
        if res['fields']['Title']['value'] not in seen:
            delete.append(res['note'])
            deleteNames.append(res['fields']['Title']['value'])
    if delete:
        x = input('Delete %d cards?\n%s'%(len(delete),'\n'.join(deleteNames)))
        if x and x[0].lower() == 'y':
            invoke('deleteNotes',notes=delete)

def test()->None:
    '''MISC THINGS TO TEST'''
    print(invoke('findCards',query='deck:aluffi'))

    # cs = Card.from_id([1510426323930])
    #
    # new = Card(fields = dict(Front =  'LATEX TEST&nbsp;<div><br></div><div>[$]\\frac{1}{2} [/$]<br></div><div><br></div><div>[$] \\chemfig{A-B} [/$]</div><div><br></div><div>[$]\\begin{tikzpicture}[column sep=1in,row sep=1in] \\matrix (A) [matrix of math nodes] { A&amp; B\\\\ C&amp; D\\\\ }; \\draw[-&gt;] (A-1-1) -- (A-1-2); \\draw[-&gt;] (A-1-1) -- (A-2-1); \\draw[-&gt;] (A-2-1) -- (A-2-2); \\draw[-&gt;] (A-2-2) -- (A-1-2); \\end{tikzpicture} [/$]</div><div></div>',
    #                          Back  =  'Temporal difference learning<div><br></div><div>[$]\\frac{1}{2} [/$]</div><div><br></div>',),
    #             tags = ['t1','t2'])
    # new.add()

if __name__ == '__main__':
    test()
