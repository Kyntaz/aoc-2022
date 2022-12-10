import scala.collection.mutable.ListBuffer;

def getInstructions = {
    io.Source.stdin.getLines()
        .map { line => 
            val args = line.split(" ");
            args match {
                case Array("addx", arg) => ("addx", arg.toInt);
                case Array("noop") => ("noop", 0);
            }
        }.toList
}

def makeFx(instructions: List[(String, Int)]) = {
    val fx = ListBuffer[Int]();
    var x = 1;

    for (inst <- instructions) {
        inst match {
            case ("addx", arg) => {
                fx += x;
                fx += x;
                x += arg;
            }
            case ("noop", _) => {
                fx += x;
            }
        }
    }

    fx.toList;
}

def makeFcrt(fx: List[Int]) = {
    fx.zipWithIndex
        .map { pixel =>
            val (x, pos) = pixel;
            val col = pos % 40;
            if (Math.abs(col - x) <= 1) '#' else ' ';
        }
}

def p1() = {
    val fx = makeFx(getInstructions);
    List(19, 59, 99, 139, 179, 219)
        .map(c => fx(c) * (c + 1))
        .sum;
}

def p2() = {
    val fx = makeFx(getInstructions);
    makeFcrt(fx)
        .grouped(40)
        .map(_.mkString)
        .foreach(println(_));
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
