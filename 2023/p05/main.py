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

        for seed, amount in seeds:
            # bot / top
            ranges = {(seed, seed + amount - 1)}

            for name, vals in maps.items():
                tmp = set()

                for entry in vals:
                    dst, lo, size = entry
                    hi = lo + size
                    assert lo < hi, f"Bad interval: {name=} {lo=} {hi=}"

                    for start, end in ranges:
                        assert start <= end, f"Bad range: {name=} {start=} {end=}"
                        size = end - start
                        # 1) lower & upper: low outside / high outside
                        # 2) low outside / high inside
                        # 3) low inside  / high inside
                        # 4) low inside  / high outside
                        # 5) low outside / middle inside / high outside
                        if (start < lo and end < lo) or (start > hi and end > hi):
                            s, e = start, end
                            tmp.add((s, e))
                        elif start < lo and end <= hi:
                            fst = (start, lo - 1)
                            snd = (dst, dst + (end - lo))
                            tmp.add(fst)
                            tmp.add(snd)
                        elif lo <= start and end <= hi:
                            offset = start - lo
                            s, e = dst + offset, dst + offset + size
                            tmp.add((s, e))
                        elif lo <= start <= hi and end > hi:
                            fst = (dst + start - lo, (dst + (hi - lo)))
                            snd = (hi + 1, hi + 1 + (end - hi))
                            tmp.add(fst)
                            tmp.add(snd)
                        elif start < lo and start > hi:
                            fst = (start, lo - 1)
                            snd = (dst, dst + (end - lo))
                            trd = (hi + 1, end)
                            tmp.add(fst)
                            tmp.add(snd)
                            tmp.add(trd)

                print(f"{name=} {seed=} {amount=} {tmp=}")
                ranges.clear()
                ranges = tmp

            finals.extend(ranges)
            ranges = []

        print(finals)
        print(min(finals)[0])


run()
