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
    def consolidate_piles(self):
        """Removes all the empty CardPiles from this Game."""
        self.piles = filter(len, self.piles)
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
        if self.current_pile == self.rightmost_pile():
            raise IndexError("You're already at the rightmost pile.")
        else:
            self.current_pile += 1
        return self.current_pile
    def reset_current_pile(self):
        self.current_pile = 0
    def pile_to_left(self, n):
        """Returns the pile n places to the left of the current pile."""
        if self.current_pile <= n - 1:
            raise IndexError("You can't move that far to the left.")
        else:
            return self.piles[self.current_pile - n]
    def show_game(self):
        """Returns this whole Game as a human-readable list."""
        return [pile.show_pile() for pile in self.piles]
    def play(self):
        print self.show_game()
        print "Current pile:", self.current_pile
        print "Number of piles:", self.count_piles()
        # Consolidate any empty piles.
        self.consolidate_piles()
        # Look at the top card on the current pile.
        current = self.get_current_pile()
        current_card = current.peek_card()
        # If it matches the top card on the pile three to the left, 
        # move it to the top of that pile and reset current pile to 0,
        # and return to the beginning of the algorithm. 
        if self.current_pile >= 3:
            left_3 = self.pile_to_left(3)
            left_3_card = left_3.peek_card()
            if current_card.matches(left_3_card):
                print current_card.name, "matches", left_3_card.name
                left_3.add_card(current.remove_card())
                self.reset_current_pile()
                self.play()
        # Do the same for the top card on the pile one to the left.
        elif self.current_pile >= 1:
            left_1 = self.pile_to_left(1)
            left_1_card = left_1.peek_card()
            if current_card.matches(left_1_card):
                print current_card.name, "matches", left_1_card.name
                left_1.add_card(current.remove_card())
                self.reset_current_pile()
                self.play()
        # If we can't do either of those things, try incrementing the 
        # current pile by 1.
        elif self.current_pile < (self.count_piles() - 1):
            self.increment_current_pile()
            self.play()
        # Otherwise, we're done!
        else: 
            print "We're done!"

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
    def play(self):
        for game in self.games:
            game.play()
    def __getitem__(self, index):
        return self.games[index]
    def __len__(self):
        return len(self.games)

class Main:
    games = GameSet('input.txt')
    
    games[0].play()

    #print "Number of piles left in game 0:", games[0].count_piles()
    #print "Number of piles left in game 1:", games[1].count_piles()


