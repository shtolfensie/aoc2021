def get_intput():
    f = open("11.input", "r")
    # f = open("11.example", "r")
    inp = []

    for l in f:
        inp.append([[int(c), False] for c in l.strip()])
    return inp

def print_rows(grid):
    for row in grid:
        print(row)
    print("\n")

def incLvl(grid):
    return list(map(lambda xs: list(map(lambda x: [x[0]+1,x[1]], xs)), grid))

def flashed(grid):
    fd = []
    for ir, row in enumerate(grid):
        for ic, octo in enumerate(row):
            if octo[0] > 9:
                fd.append([ir, ic])
    return fd

def inBounds(y, x):
    return x >= 0 and x <= 9 and y >= 0 and y <= 9

def handleNeighbors(point, grid, count):
    if grid[point[0]][point[1]][1] == True:
        return grid, count;
    # set the current center point to flashed
    count += 1
    
    grid[point[0]][point[1]][0] = 0
    grid[point[0]][point[1]][1] = True

    newFlash = []
    dirs = [(-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1,1), (0,1), (-1,1)]
    for d in dirs:
        if inBounds(point[0]+d[0], point[1]+d[1]):
            lvl:int      = grid[point[0]+d[0]][point[1]+d[1]][0] + 1
            flashed:bool = grid[point[0]+d[0]][point[1]+d[1]][1]
            if lvl > 9 and not flashed:
                # newFlash.append([point[0]+d[0], point[1]+d[1]])
                # grid[point[0]+d[0]][point[1]+d[1]][0] = 0
                # grid[point[0]+d[0]][point[1]+d[1]][1] = True
                # print_rows(grid)
                grid, count = handleNeighbors([point[0]+d[0], point[1]+d[1]], grid, count)
                
            elif not flashed:
                grid[point[0]+d[0]][point[1]+d[1]][0] = lvl
    return grid, count



def propagate(flash, grid, count):
    stack = [flash]

    while len(stack) > 0:
        # n = handleNeighbors(stack[-1], grid)
        gird, count = handleNeighbors(stack[-1], grid, count)
        # print("new flash: ", n)
        stack.pop()
        # stack.extend(n)
    return grid, count

def reset_flash(grid):
    for ir, row in enumerate(grid):
        for ic, _ in enumerate(row):
            grid[ir][ic][1] = False
    return grid

def step(grid, count):
    grid = incLvl(grid)
    fd = flashed(grid)
    print(fd)
    for flash in fd:
        grid, count = propagate(flash, grid, count)
    return reset_flash(grid), count

def run():
    grid = get_intput()
    count = 0
    for i in range(100):
        grid, count = step(grid, count)

    print_rows(grid)
    print(count)

run()
