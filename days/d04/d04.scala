def readSections = {
    io.Source.stdin.getLines()
        .map { line =>
            val Array(elf1, elf2) = line.split(",");
            val Array(start1, end1) = elf1.split("-");
            val Array(start2, end2) = elf2.split("-");
            ((start1.toInt, end1.toInt), (start2.toInt, end2.toInt));
        }
}

def fullyOverlap(sections: ((Int, Int), (Int, Int))) = {
    val ((s1, e1), (s2, e2)) = sections;
    if (
        (s1 >= s2 && e1 <= e2) ||
        (s2 >= s1 && e2 <= e1)
    ) true else false;
}

def overlap(sections: ((Int, Int), (Int, Int))) = {
    val ((s1, e1), (s2, e2)) = sections;
    if (
        (s1 > e2) || (s2 > e1)
    ) false else true;
}

def p1() = {
    readSections
        .filter(fullyOverlap)
        .length;
}

def p2() = {
    readSections
        .filter(overlap)
        .length;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
