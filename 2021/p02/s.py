#!/usr/bin/env python3

# 443 wrong, too low


def run():
    with open("in", "r") as f:
        vals = [int(l.strip()) for l in f.readlines()]
        slices = [vals[i : i + 3] for i in range(0, len(vals) - 2)]
        windows = [sum(s) for s in slices]
        print(windows[:3])

        acc = 0
        prev = windows[0]

        for curr in windows[1:]:
            if curr > prev:
                acc += 1

            prev = curr

        print(acc)


if __name__ == "__main__":
    run()
