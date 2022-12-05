import AoC.utils.blockBy;
import scala.collection.mutable.ListBuffer

def readInput = {
    val List(start, moves) = io.Source.stdin.getLines
        .toList
        .blockBy(_ == "");
    (start, moves);
}

def parseStart(start: List[String]) = {
    val state = (for (_ <- 1 to 9) yield ListBuffer[Char]())
        .toArray;
    start.slice(0, start.length - 1)
            .map(_.grouped(4))
            .map { level =>
                level.zipWithIndex
                    .foreach { position =>
                        val (content, row) = position;
                        if (content(1) != ' ') {
                            state(row) += content(1);
                        }
                    }
            }
    state;
}

def parseMoves(moves: List[String]) = {
    moves.map { move => 
        val parsedOption = """move (\d+) from (\d+) to (\d+)""".r
            .findFirstMatchIn(move);
        if (parsedOption.isDefined) {
            val parsed = parsedOption.get;
            (parsed.group(1).toInt, parsed.group(2).toInt, parsed.group(3).toInt);
        } else (-1, -1, -1);
    }.filter(_ != (-1, -1, -1));
}

def moveFromTo(state: Array[ListBuffer[Char]], from: Int, to: Int) = {
    val fromStack = state(from - 1);
    val v = fromStack.remove(0);

    val toStack = state(to - 1);
    v +=: toStack;
}

def move1(state: Array[ListBuffer[Char]], move: (Int, Int, Int)) = {
    val (n, from, to) = move;
    for (_ <- 1 to n) moveFromTo(state, from, to);
}

def move2(state: Array[ListBuffer[Char]], move: (Int, Int, Int)) = {
    val (n, from, to) = move;
    val fromStack = state(from - 1);
    val toStack = state(to - 1);
    
    val vs = fromStack.slice(0, n);
    fromStack.remove(0, n);
    vs ++=: toStack;
}

def p(move: (Array[ListBuffer[Char]], (Int, Int, Int)) => Unit) = () => {
    val (sStart, sMoves) = readInput;
    val moves = parseMoves(sMoves);
    val state = parseStart(sStart);
    moves.map(move(state, _));
    state.map(_.head).mkString;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p(move1));
    AoC.solve(2, p(move2));
}
