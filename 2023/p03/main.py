# def is_symbol(l: str) -> bool:
#    return l != "." and not l.isalnum()
#
#
# with open("in", "r") as f:
#    read = f.readlines()
#    lines = [l.strip() for l in read]
#    res = 0
#
#    for lix, line in enumerate(lines):
#        i = 0
#
#        while i < len(line) - 1:
#            letter = line[i]
#
#            if not letter.isdigit():
#                i += 1
#                continue
#
#            start = i
#            end = i
#
#            while end < len(line) and line[end].isdigit():
#                end += 1
#
#            while i < end:
#                top = lix - 1 if lix > 0 else lix
#                bot = lix + 1 if lix < len(lines) - 1 else lix
#                left = i - 1 if i > 0 else i
#                right = i + 1 if i < len(line) - 1 else i
#                box = [
#                    lines[bot][right],
#                    lines[bot][left],
#                    lines[bot][i],
#                    lines[top][right],
#                    lines[top][left],
#                    lines[top][i],
#                    lines[lix][right],
#                    lines[lix][left],
#                ]
#
#                has_symbol = map(is_symbol, box)
#
#                if any(has_symbol):
#                    res += int(line[start:end])
#                    break
#
#                i += 1
#
#            i = end
#
#    print(res)
# with open("in", "r") as f:
#    read = f.readlines()
#    lines = [l.strip() for l in read]
#    res = 0
#
#    def isdig(x, y):
#        return lines[x][y].isdigit()
#
#    for iline, line in enumerate(lines):
#        coords = []
#        numbers = 0
#
#        for ilet, letter in enumerate(line):
#            if not letter == "*":
#                continue
#
#            top = iline - 1 if iline > 0 else None
#            bot = iline + 1 if iline < len(lines) - 1 else None
#            left = ilet - 1 if ilet > 0 else None
#            right = ilet + 1 if ilet < len(line) - 1 else None
#
#            # we handle the middle below
#            selected_lines = [x for x in [top, bot] if x is not None]
#            selected_ixs = [x for x in [left, ilet, right] if x is not None]
#
#            for sel_line in selected_lines:
#                checks = []
#
#                for sel_ix in selected_ixs:
#                    if isdig(sel_line, sel_ix):
#                        checks.append((sel_line, sel_ix))
#
#                dists = [ck[1] for ck in checks]
#
#                if not dists:
#                    # . . .
#                    pass
#                elif len(dists) == 1:
#                    # x . .
#                    # . . x
#                    # . x .
#                    numbers += 1
#                    coords.extend(checks)
#                elif len(dists) == 2 and dists[1] - dists[0] > 1:
#                    # x . x
#                    numbers += 2
#                    coords.extend(checks)
#                else:
#                    # either x x . / . x x  / x x x
#                    coords.append(checks[0])
#                    numbers += 1
#
#            # x * x
#            # . * x
#            # x * .
#            # . * .
#            selected_ixs = [x for x in [left, right] if x]
#            for sel_ix in selected_ixs:
#                if isdig(iline, sel_ix):
#                    coords.append((iline, sel_ix))
#                    numbers += 1
#
#        if numbers != 2:
#            coords.clear()
#            continue
#
#        nums = []
#
#        for coord in coords:
#            x, y = coord
#
#            rhs = y
#            lhs = y
#
#            while lines[x][rhs].isdigit() and rhs + 1 < len(line):
#                rhs += 1
#
#            while lines[x][lhs - 1].isdigit() and lhs > 0:
#                lhs -= 1
#
#            tmp = int(lines[x][lhs:rhs])
#            nums.append(tmp)
#
#        res += nums[0] * nums[1]
#
#    print(res)
def run():
    with open("in", "r") as f:
        read = f.readlines()
        lines = [l.strip() for l in read]
        res = 0
        cache = {}

        for lix, line in enumerate(lines):
            i = 0

            while i < len(line) - 1:
                letter = line[i]

                if not letter.isdigit():
                    i += 1
                    continue

                start = i
                end = i

                while end < len(line) and line[end].isdigit():
                    end += 1

                while i < end:
                    top = lix - 1 if lix > 0 else lix
                    bot = lix + 1 if lix < len(lines) - 1 else lix
                    left = i - 1 if i > 0 else i
                    right = i + 1 if i < len(line) - 1 else i

                    box = [
                        (lines[bot][right], (bot, right)),
                        (lines[bot][left], (bot, left)),
                        (lines[bot][i], (bot, i)),
                        (lines[top][right], (top, right)),
                        (lines[top][left], (top, left)),
                        (lines[top][i], (top, i)),
                        (lines[lix][right], (lix, right)),
                        (lines[lix][left], (lix, left)),
                    ]

                    for s, coords in box:
                        if s == "*":
                            if not coords in cache:
                                cache[coords] = set()
                            cache[coords].add((lix, start, end))

                    i += 1
                i = end

        for gear, vals in cache.items():
            if len(vals) != 2:
                print(f"bad candidate {gear=} {vals=}")
                continue

            nums = []

            for val in vals:
                l = val[0]
                s = val[1]
                e = val[2]
                v = int(lines[l][s:e])
                nums.append(v)

            res += nums[0] * nums[1]

        print(res)


run()
