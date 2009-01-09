# "Accordian" Patience

# Problem statement taken from:
# http://www.streamtech.nl/problemset/127.html

decks = []
cards = []
for line in open('input.txt'):
    line = line.strip()
    if not line == '#':
        cards.extend(line.split(" "))

# divide cards into decks of 52
for i in range(len(cards)/52):
    decks.append(cards[i * 52:(i + 1) * 52])

print len(cards)
decklengths = [len(deck) for deck in decks]
print decklengths
print decks




