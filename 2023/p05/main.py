from pprint import pprint

Range = tuple[int, int]
Ranges = list[Range]
Entry = tuple[int, int, int]
Entries = list[Entry]
Map = tuple[str, Entries]


def mapper(ranges: Ranges, mapping: Map) -> Ranges:
    name, vals = mapping
    res = []
    print(f"{name=} takes {ranges}")

    for start, end in ranges:
        assert start <= end, f"Bad seed range: {name=} {start=} {end=}"

        for entry in vals:
            dst, lo, size = entry
            hi = lo + size
            assert lo <= hi, f"Bad map interval: {name=} {lo=} {hi=}"
            remapped = split(start, end, lo, hi, dst)
            res.extend(remapped)

        if not res:
            default = (start, end)
            res.append(default)

    print(f"{name=} produces {res}")
    return res


def split(start, end, lo, hi, dst) -> Ranges:
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


def run():
    with open("in") as f:
        lines = [l.strip() for l in f.readlines()]
        seedline = [int(x) for x in lines.pop(0).split(": ")[1].strip().split()]
        seeds = [(seedline[i], seedline[i + 1]) for i in range(0, len(seedline), 2)]
        lines.pop(0)

        maps = {}
        tmpmap = []
        finals = []

        for line in lines:
            if line:
                tmpmap.append(line)
                continue

            header = tmpmap.pop(0).split(" ")[0]
            entries = [list(map(int, t.split(" "))) for t in tmpmap]
            maps[header] = entries
            tmpmap = []

        if tmpmap:
            header = tmpmap.pop(0).split(" ")[0]
            entries = [list(map(int, t.split(" "))) for t in tmpmap]
            maps[header] = entries
            tmpmap = []

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
        pprint(res[:10])


run()
