def run():
    distances = []
    times = []
    with open("in") as f:
        lines = f.readlines()
        distances = [int(x.strip()) for x in lines.pop().split(" ")[1:] if x]
        times = [int(x.strip()) for x in lines.pop().split(" ")[1:] if x]

    print(f"{distances=} {times=}")

    res: int = 1

    for i in range(len(times)):
        hold = 0
        limit = times[i]
        goal = distances[i]
        wins = 0

        while hold < limit:
            remtime = limit - hold

            if goal < hold * remtime and remtime >= 0:
                # print(f"{i=} wins {hold=} {remtime=} to {goal=} under {limit=}")
                wins += 1

            hold += 1

        # print(f"{i=} {limit=} {goal=} {wins=}")
        res *= wins

    # 0
    print(res)


run()
