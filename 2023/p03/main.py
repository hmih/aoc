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
with open("in", "r") as f:
    read = f.readlines()
    lines = [l.strip() for l in read]
    res = 0

    for i, line in enumerate(lines):
        for j, letter in enumerate(line):
            if not letter == "*":
                continue

            top = i - 1 if i > 0 else i
            bot = i + 1 if i < len(lines) - 1 else i
            left = j - 1 if j > 0 else j
            right = j + 1 if j < len(line) - 1 else j

            box = [
                lines[bot][right],
                lines[bot][left],
                lines[bot][i],
                lines[top][right],
                lines[top][left],
                lines[top][i],
                lines[i][right],
                lines[i][left],
            ]

    print(res)
