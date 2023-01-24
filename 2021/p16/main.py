#!/usr/bin/env python3
import numpy as np

THRESHOLD = 9


def trigger_flashers(grid, flashers):
    for (fx, fy) in flashers:
        ns = get_neighbours(grid, fx, fy)
        for (nx, ny) in ns:
            grid[nx][ny] = cell_value(grid[nx][ny])


def cell_value(v):
    if v == 0:
        return 0

    if v > THRESHOLD:
        return 0

    return v + 1


def get_neighbours(grid, x, y):
    default = (x, y)

    return [
        safe_index(grid, default, x, y),
        safe_index(grid, default, x - 1, y - 1),
        safe_index(grid, default, x - 1, y + 1),
        safe_index(grid, default, x + 1, y + 1),
        safe_index(grid, default, x + 1, y - 1),
        safe_index(grid, default, x - 1, y),
        safe_index(grid, default, x + 1, y),
        safe_index(grid, default, x, y - 1),
        safe_index(grid, default, x, y + 1),
    ]


def safe_index(grid, default, x, y):
    xb = len(grid) - 1
    yb = len(grid[0]) - 1

    if x < 0 or x > xb:
        nx = default[0]
    else:
        nx = x

    if y < 0 or y > yb:
        ny = default[1]
    else:
        ny = y

    return (nx, ny)


def find_flashers(grid):
    results = []

    for (x, row) in enumerate(grid):
        for (y, v) in enumerate(row):
            if v > THRESHOLD:
                results.append((x, y))

    return results


def step(grid):
    for (x, row) in enumerate(grid):
        for (y, _) in enumerate(row):
            grid[x][y] += 1


def read_grid():
    with open("in", "r") as f:
        lines = [l.strip() for l in f.readlines()]
        cs = [list(map(lambda x: int(x), list(line))) for line in lines]
        return np.asarray(cs, dtype=int)


def pretty_print(grid):
    for (_, row) in enumerate(grid):
        print(row)


def run():
    grid = read_grid()
    print("--")

    pretty_print(grid)
    print("--")

    for _ in range(2):
        step(grid)

        while True:
            flashers = find_flashers(grid)

            if not flashers:
                break

            trigger_flashers(grid, flashers)

    pretty_print(grid)


if __name__ == "__main__":
    run()
