package AoC;

private var PART = 0;

def start(part: Int) = {
    PART = part;
}

def solve(part: Int, solution: () => Any) = {
    if (part == PART) {
        println(s"P$part: ${solution()}");
    }
}
