from pprint import pprint

Range = tuple[int, int]
Ranges = list[Range]
Entry = tuple[int, int, int]
Entries = list[Entry]
Map = tuple[str, Entries]


def mapper(ranges: Ranges, mapping: Map) -> Ranges:
    name, vals = mapping
    print(f"{name=} takes {ranges}")
    res = []

    for entry in vals:
        dst, lo, size = entry
        hi = lo + size - 1
        assert lo <= hi, f"Bad map interval: {name=} {lo=} {hi=}"

        tmp = []

        for start, end in ranges:
            assert start <= end, f"Bad seed range: {name=} {start=} {end=}"
            (mapped, unmapped) = split(start, end, lo, hi, dst)
            res.extend(mapped)
            tmp.extend(unmapped)

    print(f"{name=} produces {res}")
    return res


def split(start, end, lo, hi, dst) -> tuple[Ranges, Ranges]:
    res: Ranges = []

    # 1) ignore lower & upper: low outside / high outside
    # 2) low outside / high inside
    # 3) low inside  / high inside
    # 4) low inside  / high outside
    # 5) low outside / middle inside / high outside
    if (start < lo and end < lo) or (start > hi and end > hi):
        pass
    elif start < lo and lo <= end <= hi:
        fst = (start, lo - 1)
        snd = (dst, dst + end - lo)
        res = [fst, snd]
    elif lo <= start <= hi and lo <= end <= hi:
        fst = (dst + start - lo, dst + end - lo)
        res = [fst]
    elif lo <= start <= hi and end > hi:
        fst = (dst + start - lo, dst + hi - lo)
        snd = (hi + 1, end)
        res = [fst, snd]
    elif start < lo and end > hi:
        fst = (start, lo - 1)
        snd = (dst, dst + (hi - lo))
        trd = (hi + 1, end)
        res = [fst, snd, trd]
    else:
        raise ValueError("Unhandled case!")

    return res


def read_maps(lines):
    res = {}
    tmp = []

    for line in lines:
        if line:
            tmp.append(line)
            continue

        header = tmp.pop(0).split(" ")[0]
        entries = [list(map(int, t.split(" "))) for t in tmp]
        res[header] = entries
        tmp = []

    if tmp:
        header = tmp.pop(0).split(" ")[0]
        entries = [list(map(int, t.split(" "))) for t in tmp]
        res[header] = entries

    return res


def test_splits():
    tests = [
        ([1, 3, 4, 6, 4], []),
        ([7, 9, 4, 6, 4], []),
        ([5, 9, 4, 6, 10], [(11, 12), (7, 9)]),
        ([1, 2, 2, 3, 4], [(1, 1), (4, 4)]),
        ([1, 4, 2, 3, 4], [(1, 1), (4, 5), (4, 4)]),
    ]

    for g, e in tests:
        tmp = split(*g)
        assert tmp == e, f"{tmp} != {e}"


def run():
    maps = {}
    seeds = []

    test_splits()

    with open("in") as f:
        lines = [l.strip() for l in f.readlines()]
        seedline = [int(x) for x in lines.pop(0).split(": ")[1].strip().split()]
        seeds = [(seedline[i], seedline[i + 1]) for i in range(0, len(seedline), 2)]
        lines.pop(0)
        maps = read_maps(lines)

    finals = []

    # seeds: start, end, amount
    # maps: lo, hi, size
    for seed, amount in seeds:
        ranges = [(seed, seed + amount - 1)]

        for name, vals in maps.items():
            tmp = mapper(ranges, (name, vals))
            ranges = tmp

        finals.extend(ranges)

    # 60294664
    res = sorted(finals)
    pprint(res[:5])
    pprint(min(res)[0])


run()
