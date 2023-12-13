import functools

def parse(line):
    s, groups = line.strip().split(" ")
    lookup = {"#": 2, "?": 1, ".": 0}
    return tuple(lookup[char] for char in s), tuple(int(g) for g in groups.split(","))


data = [(parse(x), x) for x in open("./inputs/12.txt").readlines()]


def match_beginning(data, length):
    return all(x > 0 for x in data[:length]) and (
        (len(data) == length) or data[length] < 2
    )


@functools.cache
def count(data, blocks):
    total = sum(blocks)
    minimum = sum(x == 2 for x in data)
    maximum = sum(x > 0 for x in data)
    if minimum > total or maximum < total:
        return 0
    if total == 0:
        return 1
    if data[0] == 0:
        return count(data[1:], blocks)
    if data[0] == 2:
        l = blocks[0]
        if match_beginning(data, l):
            if l == len(data):
                return 1
            return count(data[l + 1 :], blocks[1:])
        return 0
    return count(data[1:], blocks) + count((2,) + data[1:], blocks)


for (line, s) in data:
    print(f"row: {s.strip()} | result: {count(*line)}")
