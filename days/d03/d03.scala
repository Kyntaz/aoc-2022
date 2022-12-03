def getRucksacks = {
    io.Source.stdin.getLines().toList;
}

def getCompartments(rucksack: String) = {
    rucksack.splitAt(rucksack.length() / 2);
}

def getGroups(rucksacks: List[String]) = {
    rucksacks.grouped(3).toList;
}

def commonItem(groups: List[String]) = {
    var all = groups.flatten.toSet
    groups.foldLeft(all) { (common, curr) =>
        common & curr.toSet;
    }.head;
}

def toPriority(c: Char) = {
    if (c >= 'a' && c <= 'z') {
        c - 'a' + 1;
    } else if (c >= 'A' && c <= 'Z') {
        c - 'A' + 27;
    } else 0;
}

def p1() = {
    getRucksacks
        .map(getCompartments(_))
        .map { (strings) =>
            val (s1, s2) = strings;
            commonItem(List(s1, s2));
        }.map(toPriority(_))
        .sum;
}

def p2() = {
    getGroups(getRucksacks)
        .map(commonItem(_))
        .map(toPriority(_))
        .sum;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
