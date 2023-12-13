from pprint import pprint
from typing import Iterable

Range = tuple[int, int]
Ranges = Iterable[Range]
Entry = tuple[int, int, int]
Entries = Iterable[Entry]


def apply_maps(seed: Range, maps: dict[str, Entries]) -> Ranges:
    res = []
    work = {seed}
    done = set()
    seen = {}

    # seeds: start, end, amount
    # maps: lo, hi, size
    for name, vals in maps.items():
        print(f"{name=} got {work=}")

        while work:
            start, end = work.pop()

            for dst, lo, size in vals:
                hi = li + size - 1

        print(f"{name=} made {done=}")

    return res


def split(start, end, lo, hi, dst):
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


def run():
    maps = {}
    seeds = []

    with open("in") as f:
        lines = [l.strip() for l in f.readlines()]
        seedline = [int(x) for x in lines.pop(0).split(": ")[1].strip().split()]
        seeds = [(seedline[i], seedline[i + 1]) for i in range(0, len(seedline), 2)]
        lines.pop(0)
        maps = read_maps(lines)

    finals = []

    for seed, amount in seeds:
        initial = (seed, seed + amount - 1)
        locs = apply_maps(initial, maps)
        finals.extend(locs)

    # 60294664
    res = sorted(finals)
    pprint(res[:5])
    pprint(min(res)[0])


run()
