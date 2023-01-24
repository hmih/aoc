#!/usr/bin/env python3


def grow_pond(pond, days):
    for _ in range(days):
        next_day = {
            0: pond[1],
            1: pond[2],
            2: pond[3],
            3: pond[4],
            4: pond[5],
            5: pond[6],
            6: pond[7] + pond[0],
            7: pond[8],
            8: pond[0],
        }

        pond = next_day

    return pond


def parse_pond():
    pond = {
        0: 0,
        1: 0,
        2: 0,
        3: 0,
        4: 0,
        5: 0,
        6: 0,
        7: 0,
        8: 0,
    }

    with open("in", "r") as f:
        lines = f.readlines()
        stripped = [l.strip() for l in lines][0]
        numbers = sorted([int(n) for n in stripped.split(",")])

        for n in numbers:
            pond[n] += 1

    return pond


def pond_size(pond):
    res = 0

    for v in pond.values():
        res += v

    return res


def run():
    pond = parse_pond()
    result = grow_pond(pond, 256)
    print(pond_size(result))


if __name__ == "__main__":
    run()
