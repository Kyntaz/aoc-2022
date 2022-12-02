def getStrategy = {
    io.Source.stdin.getLines()
        .map {
            _.split(" ").toList;
        }
}

def playScore1(play: List[String]) = {
    play match {
        case List(_, you) => {
            you match {
                case "X" => 1;
                case "Y" => 2;
                case "Z" => 3;
            }
        };
        case _ => 0;
    }
}

def playScore2(play: List[String]) = {
    val base = play match {
        case List(op, you) => {
            val opScore = op match {
                case "A" => 1;
                case "B" => 2;
                case "C" => 3;
            }
            val delta = you match {
                case "X" => -1;
                case "Y" => 0;
                case "Z" => 1;
            }
            opScore + delta;
        }
        case _ => 0;
    }

    base match {
        case 0 => 3;
        case 4 => 1;
        case _ => base;
    }
}

def winScore1(play: List[String]) = {
    play match {
        case List(op, you) => {
            (op, you) match {
                case ("A", "Y") => 6;
                case ("B", "Z") => 6;
                case ("C", "X") => 6;
                case ("A", "X") => 3;
                case ("B", "Y") => 3;
                case ("C", "Z") => 3;
                case _ => 0;
            }
        }
        case _ => 0;
    }
}

def winScore2(play: List[String]) = {
    play match {
        case List(_, you) => {
            you match {
                case "X" => 0;
                case "Y" => 3;
                case "Z" => 6;
            }
        }
        case _ => 0;
    }
}

def p1() = {
    getStrategy
        .map { play => playScore1(play) + winScore1(play) }
        .sum;
}

def p2() = {
    getStrategy
        .map { play => playScore2(play) + winScore2(play) }
        .sum;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
