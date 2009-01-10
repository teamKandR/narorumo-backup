# "Accordian" Patience

# Problem statement taken from:
# http://www.streamtech.nl/problemset/127.html

class Game:
    def __init__(self, deck):
        """A game is played with one full deck of cards.  When we 
        initialize a game, we make 52 CardPiles with one Card each, 
        which we receive as a list of two-character strings."""
        self.piles = []
        self.current_pile = -1
        for str in deck:
            pile = CardPile()
            card = Card(str)
            pile.add_card(card)
            self.piles.append(pile)
    def consolidate_piles(self):
        """Removes all the empty CardPiles from this Game."""
        self.piles = filter(len, self.piles)
    def count_piles(self):
        """Returns the number of CardPiles in this Game."""
        return len(self.piles)
    def get_current_pile(self):
        return self.piles[self.current_pile]
    def increment_current_pile(self):
        if self.current_pile == (self.count_piles() - 1):
            raise IndexError("You're already at the rightmost pile.")
        else:
            self.current_pile += 1
    def reset_current_pile(self):
        self.current_pile = 0
    def pile_to_left(self, n):
        """Returns the pile n places to the left of the current pile."""
        if self.current_pile <= (n - 1):
            raise IndexError("You can't move that far to the left.")
        else:
            return self.piles[self.current_pile - n]
    def show_game(self):
        """Returns this whole Game as a human-readable list."""
        return [pile.show_pile() for pile in self.piles]
    def play(self):
        while self.current_pile < (self.count_piles() - 1):
            self.increment_current_pile()
            self.play_step()
        else:
            count = self.count_piles();
            if count == 1:
                print "1 pile remaining: 52"
            else:
                print count, "piles remaining:",
                for pile in self.piles:
                    print len(pile),
                print
    def play_step(self, verbose=0):
        """Simulates one step of the game."""
        
        if (verbose):
            print "--------"
            print self.show_game()
            print "Current pile:", self.current_pile
            print "Number of piles:", self.count_piles()
            
        # Look at the top card on the current pile.
        current = self.get_current_pile()
        current_card = current.peek_card()
        # If it matches the top card on the pile [three|one] to the 
        # left, move it to the top of that pile and reset current_pile.
        for offset in [3, 1]:            
            if self.current_pile >= offset:
                left = self.pile_to_left(offset)
                left_card = left.peek_card()
                if current_card.matches(left_card):
                    left.add_card(current.remove_card())
                    self.consolidate_piles()
                    self.reset_current_pile()
                    return

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
    def show_pile(self):
        """Returns this whole CardPile as a human-readable list."""
        return [card.name for card in self.pile]
    def __len__(self):
        return len(self.pile)

class GameSet:
    def __init__(self, filehandle):
        self.games = []
        
        # Read in all the cards.
        cards = []
        for line in open(filehandle):
            if '#' not in line:
                line = line.strip()
                cards.extend(line.split(" "))
        
        # Divide into games of 52 cards each.
        for i in range(len(cards)/52):
            game = Game(cards[i * 52:(i + 1) * 52])
            self.games.append(game)
    def play(self):
        for game in self.games:
            game.play()

GameSet('input.txt').play()

