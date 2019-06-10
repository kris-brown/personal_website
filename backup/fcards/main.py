from parse import parse_aluffi
from connect import invoke, removeUnseen

if __name__=='__main__':
    # Create deck if not exists
    invoke('createDeck', deck='aluffi')
    # Add new cards
    ids = set()
    for card in parse_aluffi():
        card.add()
        ids.add(card.ID)

    removeUnseen(ids)
