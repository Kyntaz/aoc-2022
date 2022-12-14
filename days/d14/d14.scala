import scala.collection.mutable.Map;
import scala.compiletime.ops.boolean

enum Tile {
    case Empty, Sand, Rock;
}

val MAP = Map[(Int, Int), Tile]().withDefaultValue(Tile.Empty);
var MAX_Y = 0;

def addRockSegment(start: (Int, Int), end: (Int, Int)) = {
    val (x1, y1) = start;
    val (x2, y2) = end;

    if (x1 == x2) {
        val step = if (y1 > y2) -1 else 1;
        for (y <- y1 to y2 by step) {
            MAP((x1, y)) = Tile.Rock;
        }
    } else if (y1 == y2) {
        val step = if (x1 > x2) -1 else 1;
        for (x <- x1 to x2 by step) {
            MAP((x, y1)) = Tile.Rock;
        }
    }

    MAX_Y = List(MAX_Y, y1, y2).max;
}

def buildMap() = {
    io.Source.stdin.getLines()
        .foreach { structure =>
            structure.split(" -> ")
                .sliding(2)
                .foreach { segment =>
                    val Array(sStart, sEnd) = segment;
                    val Array(x1, y1) = sStart.split(",");
                    val Array(x2, y2) = sEnd.split(",");

                    addRockSegment((x1.toInt, y1.toInt), (x2.toInt, y2.toInt));
                }
        }
}

def dropSand(position: (Int, Int), floor: Int, stop: ((Int, Int)) => Boolean) = {
    var current = position;
    var stable = false;
    while (!stable) {
        val (x, y) = current;

        val u = (x, y + 1);
        val ul = (x - 1, y + 1);
        val ur = (x + 1, y + 1); 
        if (y >= floor - 1) {
            stable = true;
        } else if (MAP(u) == Tile.Empty) {
            current = u;
        } else if (MAP(ul) == Tile.Empty) {
            current = ul;
        } else if (MAP(ur) == Tile.Empty) {
            current = ur;
        } else {
            stable = true;
        }
    }

    MAP(current) = Tile.Sand;

    if (stop(current)) false else true;
}

def p1() = {
    buildMap();

    var count = 0;
    while (dropSand((500, 0), MAX_Y + 1, (sand) => sand._2 >= MAX_Y)) {
        count += 1;
    }
    count;
}

def p2() = {
    buildMap();

    var count = 1;
    while (dropSand((500, 0), MAX_Y + 2, (sand) => sand == (500, 0))) {
        count += 1;
    }
    count;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
