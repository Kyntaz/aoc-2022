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

package utils {
    extension [T](l: List[T]) def blockBy(f: T => Boolean) = {
        var blocks = List[List[T]]();
        var block = List[T]();

        for (el <- l) {
            if (f(el)) {
                blocks = blocks.appended(block);
                block = List[T]();
            } else {
                block = block.appended(el);
            }
        }

        blocks;
    }
}
