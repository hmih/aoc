from collections import Counter


def encode_hands(hands: tuple) -> tuple:
    res = []
    trans = str.maketrans("AKQJ", "ZYX1")

    for hand in hands:
        encoded: str = hand.translate(trans)
        res.append(encoded)

    return tuple(res)


def classify(hands: tuple):
    res = {}

    for hand in hands:
        prefreq = Counter(hand)
        topfreqs = sorted([(c, s) for (s, c) in prefreq.most_common() if s != "1"], reverse=True)
        topfreq = topfreqs[0][1] if topfreqs else "1"
        subd = hand.replace("1", topfreq)
        print(f"{hand=}: {subd=}")

        freqmap = Counter(subd)
        size = len(freqmap)
        classification = -1
        freqs: list = sorted(freqmap.values(), reverse=True)

        # five of a kind
        if size == 1 and freqs[0] == 5:
            classification = 6
        # four of a kind
        elif size == 2 and freqs[0] == 4 and freqs[1] == 1:
            classification = 5
        # full house
        elif size == 2 and freqs[0] == 3 and freqs[1] == 2:
            classification = 4
        # three of a kind
        elif size == 3 and freqs[0] == 3 and freqs[1] == 1 and freqs[2] == 1:
            classification = 3
        # two pair
        elif size == 3 and freqs[0] == 2 and freqs[1] == 2 and freqs[2] == 1:
            classification = 2
        # one pair
        elif size == 4 and freqs[0] == 2:
            classification = 1
        # high card
        elif size == 5:
            classification = 0
        else:
            raise ValueError(f"bad classification: {hand=} {freqs=}")

        tmp = tuple([classification, *hand])
        res[tmp] = hand

    return res


def read():
    with open("in") as f:
        lines = f.readlines()
        parts = tuple(x.strip().split(" ") for x in lines if x and x.strip())
        raw_hands = tuple(x[0].strip() for x in parts if x)
        bids = tuple(int(x[1].strip()) for x in parts if x)
        encoded = encode_hands(raw_hands)
        return (encoded, dict(zip(encoded, raw_hands)), dict(zip(encoded, bids)))


def run():
    hands, handrawmap, handbidmap = read()
    classifications = classify(hands)
    ranks = sorted(classifications.keys())

    res = 0

    for rank, clsf in enumerate(ranks):
        hand = classifications[clsf]
        rawhand = handrawmap[hand]
        bid = handbidmap[hand]
        msg = f"{rawhand=} {hand=} {clsf=}\t: {res=} += {bid=} * rank={rank + 1}"
        print(msg)
        res += bid * (rank + 1)

    # too low
    # 248399269
    # correct
    # 248747492
    print(res)


run()
