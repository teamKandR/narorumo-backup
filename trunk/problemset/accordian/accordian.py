# "Accordian" Patience

# Problem statement taken from:
# http://www.streamtech.nl/problemset/127.html

class Game:
    def __init__(self, deck):
        """A game is played with one full deck of cards.  When we 
        initialize a game, we have 52 CardPiles with one card each, 
        which we receive as a list of strings."""
        self.piles = []
        for card in deck:
            pile = CardPile()
            pile.add_card(card)
            self.piles.append(pile)
    def count_piles(self):
        """Returns the number of piles in this game."""
        return len(self.piles)
    def leftmost_pile(self):
        return self.piles[0]
    def rightmost_pile(self):
        return self.piles[(len(self.piles) -1)]

class Card:
    """A playing card in a standard deck."""
    def __init__(self, card):
        self.rank = card[0]
        self.suit = card[1]
        self.name = card

class CardPile:
    """A stack of cards."""
    def __init__(self):
        self.pile = []
    def peek_card(self):
        """Returns the top card of the pile without popping it."""
        return self.pile[(len(self.pile) - 1)]
    def add_card(self, card):
        self.pile.append(card)
    def remove_card(self):
        return self.pile.pop()

# read in all the cards
cards = []
for line in open('input.txt'):
    line = line.strip()
    if not line == '#':
        cards.extend(line.split(" "))

# divide up games
games = []
for i in range(len(cards)/52):
    game = Game(cards[i * 52:(i + 1) * 52])
    games.append(game)

game_zero = games[0]
game_one  = games[1]
    
print "Number of games:", len(games)
print "Number of piles in game zero:", game_zero.count_piles()
print "Number of piles in game one:", game_one.count_piles()
print "Top card of leftmost pile in game zero:", game_zero.leftmost_pile().peek_card()
print "Top card of rightmost pile in game zero:", game_zero.rightmost_pile().peek_card()
print "Top card of leftmost pile in game one:", game_one.leftmost_pile().peek_card()
print "Top card of rightmost pile in game one:", game_one.rightmost_pile().peek_card()

