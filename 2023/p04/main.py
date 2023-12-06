def run():
    res = 0
    repeats = {}
    points = {}

    with open("in", "r") as f:
        cards = f.readlines()

        for ix, card in enumerate(cards):
            (lhs, rhs) = card.strip().split(" | ")
            (_, winners) = lhs.strip().split(": ")
            picks = [x for x in rhs.split(" ") if x]
            wins = [x for x in winners.split(" ") if x]
            won = [1 for pick in picks if pick in wins]
            points[ix] = 0 if not won else 2 ** (sum(won) - 1)

            if ix not in repeats:
                repeats[ix] = 1
            else:
                repeats[ix] += 1

            for _ in range(repeats[ix]):
                for w in range(1, len(won) + 1):
                    offset = ix + w
                    if offset not in repeats:
                        repeats[offset] = 1
                    else:
                        repeats[offset] += 1
    res = sum(repeats.values())
    print(res)


run()
