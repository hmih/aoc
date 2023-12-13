def run():
    with open("in") as f:
        lines = [l.strip() for l in f.readlines()]
        res = 0

        for line in lines:
            parts = [int(x) for x in line.split()]
            seqs = [parts[:]]
            print(f"{line=} {parts=} {seqs=}")

            while True:
                last = seqs[-1]
                tmp = []
                for i in range(0, len(last) - 1):
                    tmp.append(last[i + 1] - last[i])
                seqs.append(tmp)

                if all([x == 0 for x in tmp]):
                    break

            revd = list(reversed(seqs))

            for i in range(1, len(revd)):
                curr = revd[i]
                prev = revd[i - 1]
                tmp = curr[0] - prev[0]
                curr.insert(0, tmp)

            for seq in seqs:
                print(seq)

            res += seqs[0][0]

        # too low
        # 222
        print("res")
        print(res)


run()
