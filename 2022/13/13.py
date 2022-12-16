
def lines():
    with open('input.txt') as f:
        for line in f:
            if line.strip() == '':
                continue
            yield eval(line)

l = [_ for _ in lines()]


def comp(left, right):
    if isinstance(left, list) and isinstance(right, list):
        for l, r in zip(left, right):
            c = comp(l, r)
            if c != 0:
                return c
        return len(right) - len(left)
    elif not isinstance(left, list) and isinstance(right, list):
        return comp([left], right)
    elif isinstance(left, list) and not isinstance(right, list):
        return comp(left, [right])
    else:
        return right - left


def find_lines():
    for i in range(1, len(l) // 2 + 1):
        a = l[i * 2 - 2]
        b = l[i * 2 - 1]
        if comp(a, b) > 0:
            yield i

print("part1")
print(sum(find_lines()))

# part2
from functools import cmp_to_key

all_lines = [l for l in lines()] + [[[2]], [[6]]]
s = sorted(all_lines, key=cmp_to_key(comp), reverse=True)
print("\n".join(str(_) for _ in s))

print("part2")
print((s.index([[2]]) + 1) * (s.index([[6]]) + 1))
