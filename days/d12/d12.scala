import scala.collection.mutable.{ PriorityQueue, Map, ListBuffer };

def getHeightMap = {
    io.Source.stdin.getLines()
        .map(_.toArray)
        .toArray;
}

def buildPath(cameFrom: Map[(Int, Int), (Int, Int)], end: (Int, Int)) = {
    val path = ListBuffer[(Int, Int)](end);

    var current = end;
    while (cameFrom.contains(current)) {
        current = cameFrom(current);
        current +=: path;
    }

    path;
}

// A*
def findPath(
    start: (Int, Int),
    end: (Int, Int),
    map: Array[Array[Char]]
): Option[List[(Int, Int)]] = {
    val h = (position: (Int, Int)) =>
            Math.abs(position._1 - end._1) +
            Math.abs(position._2 - end._2);
    
    val getHeight = (position: (Int, Int)) => {
        val c = map(position._1)(position._2);
        if (c == 'S') 'a'
        else if (c == 'E') 'z'
        else c;
    }
    
    val getNeighbors = (position: (Int, Int)) =>
        List[(Int, Int)](
            (position._1 + 1, position._2),
            (position._1 - 1, position._2),
            (position._1, position._2 + 1),
            (position._1, position._2 - 1),
        ).filter { neighbor =>
            neighbor._1 < map.length &&
                neighbor._1 >= 0 &&
                neighbor._2 < map(neighbor._1).length &&
                neighbor._2 >= 0 &&
                getHeight(neighbor) <= getHeight(position) + 1; 
        }

    val cameFrom = Map[(Int, Int), (Int, Int)]();

    val fScore = Map[(Int, Int), Int]();
    fScore(start) = h(start);

    val gScore = Map[(Int, Int), Int]().withDefaultValue(Int.MaxValue);
    gScore(start) = 0;

    val openSet = PriorityQueue[(Int, Int)]()(Ordering.by(-fScore(_)));
    openSet += start;

    while (!openSet.isEmpty) {
        val current = openSet.dequeue();
        if (current == end) {
            return Option(buildPath(cameFrom, current).toList);
        }

        val neighbors = getNeighbors(current);
        for (neighbor <- neighbors) {
            val tentativeG = gScore(current) + 1;
            if (tentativeG < gScore(neighbor)) {
                cameFrom(neighbor) = current;
                gScore(neighbor) = tentativeG;
                fScore(neighbor) = tentativeG + h(neighbor);
                
                if (!openSet.toList.contains(neighbor)) {
                    openSet += neighbor;
                }
            }
        }
    }

    Option.empty[List[(Int, Int)]];
}

def findPositions(check: (Char) => Boolean, map: Array[Array[Char]]) = {
    map.zipWithIndex
        .toList
        .flatMap { (indexedLine) =>
            val (line, row) = indexedLine;
            line.zipWithIndex
                .filter((line) => check(line._1))
                .map((col) => (row, col._2))
                .toList;
        }
}

def p1() = {
    val heightMap = getHeightMap;
    val start = findPositions(_ == 'S', heightMap).head;
    val end = findPositions(_ == 'E', heightMap).head;

    val path = findPath(start, end, heightMap).get;
    path.length - 1;
}

def p2() = {
    val heightMap = getHeightMap;
    val possibleStarts = findPositions((c) => c == 'S' || c == 'a', heightMap);
    val end = findPositions(_ == 'E', heightMap).head;

    possibleStarts
        .map { start =>
            val maybePath = findPath(start, end, heightMap);
            if (maybePath.isDefined) maybePath.get.length - 1 else Int.MaxValue;
        }.min
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
