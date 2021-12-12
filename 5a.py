import numpy as np

def get_intput():
    f = open("5.input", "r")
    # f = open("5.example", "r")
    inp = []

    for l in f:
        x = l.split(" -> ")
        a = x[0].split(",")
        b = x[1].split(",")

        inp.append(((int(a[0]), int(a[1])), (int(b[0]), int(b[1]))))
    return inp

def isStraight(l):
    a = l[0]
    b = l[1]

    return a[0] == b[0] or a[1] == b[1]

def moveLine(grid, l):
    a = l[0]
    b = l[1]

    if a[0] > b[0]:
        for i in range(b[0], a[0]+1):
            grid[a[1]][i] += 1
    elif a[0] < b[0]:
        for i in range(a[0], b[0]+1):
            grid[a[1]][i] += 1
    elif a[1] > b[1]:
        for i in range(b[1], a[1]+1):
            grid[i][a[0]] +=1
    elif a[1] < b[1]:
        for i in range(a[1], b[1]+1):
            grid[i][a[0]] +=1


def f(lines):
    mp = (987+1,989+1)
    # mp = (9+1,9+1)
    mp1 = (mp[1], mp[0])
    grid = np.zeros(mp1, np.int16)

    sl = filter(isStraight, lines)

    for l in sl:
        moveLine(grid, l)

    # print(grid)
    print((np.count_nonzero(grid >= 2)))



inp = get_intput()

f(inp)

