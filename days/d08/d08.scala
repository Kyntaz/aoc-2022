import scala.util.control.Breaks.{ breakable, break };

def getTrees = {
    io.Source.stdin.getLines()
        .map {
            _.map(_.toInt)
                .toArray;
        }.toArray
}

def visibleFromWest(trees: Array[Array[Int]]) = {
    val result = trees.map(_.clone);
    for (line <- 0 until trees.length) {
        var max = -1;
        for (column <- 0 until trees(0).length) {
            val tree = trees(line)(column);
            if (tree > max) {
                result(line)(column) = 1;
                max = tree;
            } else {
                result(line)(column) = 0;
            }
        }
    }

    result;
}

def visibleFromEast(trees: Array[Array[Int]]) = {
    val result = trees.map(_.clone);
    for (line <- trees.length - 1 to 0 by -1) {
        var max = -1;
        for (column <- trees(0).length - 1 to 0 by -1) {
            val tree = trees(line)(column);
            if (tree > max) {
                result(line)(column) = 1;
                max = tree;
            } else {
                result(line)(column) = 0;
            }
        }
    }

    result;
}

def visibleFromNorth(trees: Array[Array[Int]]) = {
    val result = trees.map(_.clone);
    for (column <- 0 until trees(0).length) {
        var max = -1;
        for (line <- 0 until trees.length) {
            val tree = trees(line)(column);
            if (tree > max) {
                result(line)(column) = 1;
                max = tree;
            } else {
                result(line)(column) = 0;
            }
        }
    }

    result;
}

def visibleFromSouth(trees: Array[Array[Int]]) = {
    val result = trees.map(_.clone);
    for (column <- trees(0).length - 1 to 0 by -1) {
        var max = -1;
        for (line <- trees.length - 1 to 0 by -1) {
            val tree = trees(line)(column);
            if (tree > max) {
                result(line)(column) = 1;
                max = tree;
            } else {
                result(line)(column) = 0;
            }
        }
    }

    result;
}

def partialScenicScore(trees: Array[Array[Int]], tree: Int, range: Range, getTree: (Int) => Int) = {
    var count = 0;

    breakable {
        for (i <- range) {
            val other = getTree(i);
            if (other < tree) {
                count += 1;
            } else {
                count += 1;
                break;
            }
        }
    }

    count;
}

def scenicScore(trees: Array[Array[Int]], line: Int, column: Int) = {
    val tree = trees(line)(column);

    partialScenicScore(trees, tree, line - 1 to 0 by -1, (i) => trees(i)(column)) *
    partialScenicScore(trees, tree, line + 1 until trees.length, (i) => trees(i)(column)) *
    partialScenicScore(trees, tree, column - 1 to 0 by -1, (i) => trees(line)(i)) *
    partialScenicScore(trees, tree, column + 1 until trees(0).length, (i) => trees(line)(i));
}

def p1() = {
    val trees = getTrees;
    val e = visibleFromEast(trees);
    val w = visibleFromWest(trees);
    val n = visibleFromNorth(trees);
    val s = visibleFromSouth(trees);

    var count = 0;
    for (line <- 0 until e.length) {
        for (column <- 0  until e(0).length) {
            if (
                e(line)(column) +
                w(line)(column) +
                n(line)(column) +
                s(line)(column) > 0
            ) {
                count += 1;
            }
        }
    }

    count;
}

def p2() = {
    val trees = getTrees;
    var max = 0;

    for (line <- 0 until trees.length) yield {
        for (column <- 0 until trees(0).length) yield {
            val score = scenicScore(trees, line, column);
            if (score > max) {
                max = score;
            }
        }
    }

    max;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
