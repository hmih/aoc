#!/usr/bin/env python3

# 20 is wrong
# 37 is wrong
# 1335 is low

def run():
    with open('in', 'r') as f:
        vals = [int(l.strip()) for l in f.readlines()]

        acc = 0
        prev = vals[0]

        for curr in vals[1:]:
            if curr > prev:
                acc += 1
            prev = curr

        print(acc)

if __name__ == '__main__':
    run()
