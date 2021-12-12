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
    f.close()
    return inp

def isStraight(l):
    a = l[0]
    b = l[1]

    return a[0] == b[0] or a[1] == b[1]

def p1(a):
    return a+1
def m1(a):
    return a-1

def id(a):
    return a

def ops(l):
    a = l[0]
    b = l[1]
    os = [p1, p1]
    if a[0] > b[0]:
        os[0] = m1
    elif a[0] == b[0]:
        os[0] = id
    if a[1] > b[1]:
        os[1] = m1
    elif a[1] == b[1]:
        os[1] = id
    return os



def moveLine(grid, l):
    a = list(l[0])
    b = list(l[1])

    os = ops(l)

    # if a[0] > b[0]:
    while True:
        # print(a,b)
        grid[a[1]][a[0]] += 1
        if a[0] == b[0] and a[1] == b[1]:
            break;
        a[0] = os[0](a[0])
        a[1] = os[1](a[1])
    # elif a[0] < b[0]:
    #     for i in range(a[0], b[0]+1):
    #         grid[a[1]][i] += 1
    # elif a[1] > b[1]:
    #     for i in range(b[1], a[1]+1):
    #         grid[i][a[0]] +=1
    # elif a[1] < b[1]:
    #     for i in range(a[1], b[1]+1):
    #         grid[i][a[0]] +=1


def f(lines):
    mp = (987+1,989+1)
    # mp = (9+1,9+1)
    mp1 = (mp[1], mp[0])
    grid = np.zeros(mp1, np.int16)

    # sl = filter(isStraight, lines)
    sl = lines

    for l in sl:
        moveLine(grid, l)

    # print(grid)
    print((np.count_nonzero(grid >= 2)))



inp = get_intput()

f(inp)

