import scala.collection.mutable.ListBuffer;

class Operation(val operator: Char, val left: Long, val right: Long) {
    def apply(old: Long) = {
        val currLeft = if (left == -1) old else left;
        val currRight = if (right == -1) old else right;

        operator match {
            case '+' => currLeft + currRight;
            case '*' => currLeft * currRight;
            case _ => 0l;
        }
    }
}

class Monkey(
    val items: ListBuffer[Long],
    val operation: Operation,
    val test: Long,
    val ifTrue: Int,
    val ifFalse: Int
) {
    var inspections = 0l;

    def receiveItem(item: Long) = {
        items += item;
    }

    def processItem(item: Long) = {
        val worryFactor = if (WORRIED) 1l else 3l;
        val newItem = (operation.apply(item) / worryFactor) % NORMALIZER;
        val destination = if (newItem % test == 0) ifTrue else ifFalse;
        MONKEYS(destination).receiveItem(newItem);
    }

    def doTurn() = {
        for (item <- items) {
            processItem(item);
            inspections += 1;
        }
        items.clear();
    }
}

val MONKEYS = new ListBuffer[Monkey]();
var WORRIED = false;
var NORMALIZER = 1l;

def readMonkeys() = {
    io.Source.stdin.getLines()
        .grouped(7)
        .foreach { info =>
            val itemsMatch = """\s*Starting items: (.*)""".r
                .findFirstMatchIn(info(1)).get;
            val operationMatch = """\s*Operation: new = ([a-z0-9]+) (.) ([a-z0-9]+)""".r
                .findFirstMatchIn(info(2)).get;
            val testMatch = """\s*Test: divisible by (\d+)""".r
                .findFirstMatchIn(info(3)).get;
            val trueMatch = """\s*If true: throw to monkey (\d*)""".r
                .findFirstMatchIn(info(4)).get;
            val falseMatch = """\s*If false: throw to monkey (\d*)""".r
                .findFirstMatchIn(info(5)).get;

            val parseIntOrOld = (str: String) => if (str == "old") -1l else str.toLong;
            
            val monkey = new Monkey(
                ListBuffer[Long](
                    itemsMatch.group(1)
                        .split(", ")
                        .map(_.toLong): _ *
                ),
                new Operation(
                    operationMatch.group(2)(0),
                    parseIntOrOld(operationMatch.group(1)),
                    parseIntOrOld(operationMatch.group(3))
                ),
                testMatch.group(1).toLong,
                trueMatch.group(1).toInt,
                falseMatch.group(1).toInt
            );

            MONKEYS += monkey;
            NORMALIZER *= monkey.test;
        }
}

def round() = {
    for (monkey <- MONKEYS) {
        monkey.doTurn();
    }
}

def p(rounds: Int, worried: Boolean) = () => {
    WORRIED = worried;
    readMonkeys();
    for (i <- 1 to rounds) {
        round();
    }

    MONKEYS.map(_.inspections)
        .sorted
        .slice(MONKEYS.length - 2, MONKEYS.length)
        .foldLeft(1l) { (inspections, acc) =>
            acc * inspections;
        }
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p(20, false));
    AoC.solve(2, p(10000, true));
}
