import scala.collection.mutable.Set;
import scala.annotation.switch

class Motion(val dir: String, val amount: Int);

var KNOTS: Array[(Int, Int)] = null;
val VISITED = Set[(Int, Int)]();

def getMotions = {
    io.Source.stdin.getLines()
        .map { line =>
            val Array(dir, sAmount) = line.split(" ");
            new Motion(dir, sAmount.toInt);
        }.toList;
}

def initKnots(n: Int) = {
    KNOTS = Array.ofDim(n);
    for (i <- 0 until n) {
        KNOTS(i) = (0, 0);
    }
}

def move(dir: String) = {
    val (x, y) = KNOTS(0);
    dir match {
        case "R" => KNOTS(0) = (x + 1, y);
        case "L" => KNOTS(0) = (x - 1, y);
        case "U" => KNOTS(0) = (x, y + 1);
        case "D" => KNOTS(0) = (x, y - 1);
    }
}

def follow(nth: Int) = {
    val (hx, hy) = KNOTS(nth - 1);
    val (tx, ty) = KNOTS(nth);

    if (Math.abs(hx - tx) > 1 || Math.abs(hy - ty) > 1) {
        var ux = tx;
        var uy = ty;
        if (tx - hx > 0) {
            ux -= 1;
        } else if (tx - hx < 0) {
            ux += 1;
        }

        if (ty - hy > 0) {
            uy -= 1;
        } else if (ty - hy < 0) {
            uy += 1;
        }

        KNOTS(nth) = (ux, uy);
    }
}

def step(dir: String) = {
    move(dir);
    for (i <- 1 until KNOTS.length) {
        follow(i);
    }
    VISITED += KNOTS.last;
}

def applyMotion(motion: Motion) = {
    for (_ <- 1 to motion.amount) {
        step(motion.dir);
    }
}

def p(n: Int) = () => {
    initKnots(n);
    getMotions.foreach(applyMotion(_));
    VISITED.size;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p(2));
    AoC.solve(2, p(10));
}
