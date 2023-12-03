RED = 12
GREEN = 13
BLUE = 14

with open("in", "r") as f:
    games = f.readlines()
    res = []

    for game in games:
        (game, rounds) = game.split(":")
        game_id = int(game.split()[1])
        draws = rounds.split(";")
        possible = True

        # r g b
        mins = [0, 0, 0]

        for draw in draws:
            selections = draw.split(", ")
            group = {}

            for selection in selections:
                (number, color) = selection.split()
                num = int(number)
                group[color] = num

            for c, n in group.items():
                if (
                    (c == "red" and n > RED)
                    or (c == "blue" and n > BLUE)
                    or (c == "green" and n > GREEN)
                ):
                    possible = False

            mins[0] = max(mins[0], group.get("red", 0))
            mins[1] = max(mins[1], group.get("green", 0))
            mins[2] = max(mins[2], group.get("blue", 0))

        res.append(mins)

    pow = 0

    for v in res:
        pow += v[0] * v[1] * v[2]

    print(pow)
