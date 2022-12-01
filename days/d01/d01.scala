import AoC.utils.blockBy;

def getCaloryLists = {
    io.Source.stdin.getLines.toList
        .blockBy(_ == "")
        .map {
            _.map(_.toInt);
        }
}

def p1() = {
    getCaloryLists
        .map(_.sum)
        .max;
}

def p2() = {
    getCaloryLists
        .map(_.sum)
        .sortWith(_ > _)
        .slice(0, 3)
        .sum;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
