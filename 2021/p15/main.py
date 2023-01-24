#!/usr/bin/env python3

# too low
# 218469

closers = {
    "(": ")",
    "{": "}",
    "<": ">",
    "[": "]",
}

openers = {
    ")": "(",
    "}": "{",
    ">": "<",
    "]": "[",
}

vals = {
    ")": 1,
    "]": 2,
    "}": 3,
    ">": 4,
}


def is_open(l):
    return l in openers.values()


def matching(l):
    if l in closers:
        return closers[l]
    else:
        return openers[l]


def score_symbol(l):
    return {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137,
    }[l]


def parse_line(line):
    stack = []

    for letter in line:
        if is_open(letter):
            stack.append(letter)
        else:
            last = stack.pop()
            if matching(last) != letter:
                return False

    return stack


def complete(stack):
    total = 0
    stack.reverse()

    for c in stack:
        total = total * 5
        total += vals[matching(c)]

    return total


def run():
    with open("in", "r") as f:
        lines = [l.strip() for l in f.readlines()]

        stacks = []

        for l in lines:
            res = parse_line(l)

            if res:
                stacks.append(res)

        values = []

        for stack in stacks:
            values.append(complete(stack))

        values = sorted(values)
        middle = int(len(values) / 2)
        print(values[middle])


if __name__ == "__main__":
    run()
