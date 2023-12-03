with open("in", "r") as f:
    lines = f.readlines()

    digits = [
        "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
    ]

    res = 0

    for line in lines:
        buf = []

        for ix, char in enumerate(line):
            if str.isdigit(char):
                buf.append(char)
                continue

            for num, digit in enumerate(digits):
                if line[ix : ix + len(digit)] == digit:
                    buf.append(str(num))
                    continue

        res += int(buf[0] + buf[-1])

    print(res)
