# "Accordian" Patience

# Problem statement taken from:
# http://www.streamtech.nl/problemset/127.html

class Game:
    def __init__(self, deck):
        """A game is played with one full deck of cards.  When we 
        initialize a game, we make 52 CardPiles with one Card each, 
        which we receive as a list of two-character strings."""
        self.piles = []
        for str in deck:
            pile = CardPile()
            card = Card(str)
            pile.add_card(card)
            self.piles.append(pile)
        self.current_pile = 0
    def count_piles(self):
        """Returns the number of CardPiles in this Game."""
        return len(self.piles)
    def leftmost_pile(self):
        return self.piles[0]
    def rightmost_pile(self):
        return self.piles[(len(self.piles) - 1)]
    def get_current_pile(self):
        return self.piles[self.current_pile]
    def increment_current_pile(self):
        self.current_pile += 1
        return self.current_pile
    def reset_current_pile(self):
        self.current_pile = 0

class Card:
    """A playing card in a standard deck."""
    def __init__(self, card):
        self.rank = card[0]
        self.suit = card[1]
        self.name = card
    def matches(self, card):
        return (self.rank == card.rank or self.suit == card.suit)

class CardPile:
    """A stack of cards."""
    def __init__(self):
        self.pile = []
    def peek_card(self):
        """Returns the top Card of the pile without popping it.  For a
        human-readable string, use peek_card().name."""
        return self.pile[(len(self.pile) - 1)]
    def add_card(self, card):
        self.pile.append(card)
    def remove_card(self):
        return self.pile.pop()
        
class GameSet:
    def __init__(self, filehandle):
        self.games = []
        
        # read in all the cards
        cards = []
        for line in open(filehandle):
            if '#' not in line:
                line = line.strip()
                cards.extend(line.split(" "))
        
        # divide up games
        for i in range(len(cards)/52):
            game = Game(cards[i * 52:(i + 1) * 52])
            self.games.append(game)
    def __getitem__(self, index):
        return self.games[index]
    def __len__(self):
        return len(self.games)

games = GameSet('input.txt')

game_zero = games[0]
game_one  = games[1]
    
print "Number of games:", len(games)
print "Number of piles in game zero:", game_zero.count_piles()
print "Number of piles in game one:", game_one.count_piles()
print "Top card of leftmost pile in game zero:", game_zero.leftmost_pile().peek_card().name
print "Top card of rightmost pile in game zero:", game_zero.rightmost_pile().peek_card().name
print "Top card of leftmost pile in game one:", game_one.leftmost_pile().peek_card().name
print "Top card of rightmost pile in game one:", game_one.rightmost_pile().peek_card().name

