import math

def run():
    cleaner = (
        lambda s: s.strip()
        .replace("(", "")
        .replace(")", "")
        .replace("=", "")
        .replace(",", "")
        .replace("  ", " ")
        .split()
    )

    with open("in") as f:
        lines = [l.strip() for l in f.readlines()]
        instrs = [i.strip() for i in lines.pop(0)]
        lines.pop(0)
        parts = [cleaner(l) for l in lines]
        nodes: dict[str, tuple[str, str]] = {n: (l, r) for (n, l, r) in parts}
        starts: list[str] = [k for k in nodes.keys() if k.endswith("A")]
        multiples = []

        for start in starts:
            selection = start
            steps = 0

            while not selection.endswith("Z"):
                curr = instrs[0]
                ix = 1 if curr == "R" else 0
                selection = nodes[selection][ix]
                instrs.append(instrs.pop(0))
                steps += 1

            multiples.append(steps)

        res = math.lcm(*multiples)
        print(multiples)
        print(res)


run()
