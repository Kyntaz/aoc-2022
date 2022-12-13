import AoC.utils.blockBy;
import scala.collection.mutable.ListBuffer;

type PacketSegment = Int | List[_];

def segmentStr(str: String) = {
    val segments = ListBuffer[String]();
    var layer = 0;
    var currStr = "";

    for (c <- str) {
        if (layer == 0) {
            c match {
                case ',' => {
                    segments += currStr;
                    currStr = "";
                }
                case '[' => {
                    currStr += c;
                    layer += 1;
                }
                case _ => {
                    currStr += c;
                }
            }
        } else {
            c match {
                case '[' => {
                    currStr += c;
                    layer += 1;
                }
                case ']' => {
                    currStr += c;
                    layer -= 1;
                }
                case _ => {
                    currStr += c;
                }
            }
        }
    }

    if (!currStr.isEmpty()) {
        segments += currStr;
    }

    segments.toList;
}

def parseList(str: String): PacketSegment = {
    if (str(0) == '[') {
        val innerStr = str.slice(1, str.length() - 1);
        val segments = segmentStr(innerStr);

        segments
            .map { segment =>
                if (segment.isEmpty()) List[PacketSegment]()
                else parseList(segment);
            }
    } else {
        str.toInt;
    }
}

def getPackets = {
    io.Source.stdin.getLines()
        .toList
        .blockBy(_ == "")
        .map { packets =>
            val List(s1, s2) = packets;
            (parseList(s1), parseList(s2));
        }
}

def getPacketList = {
    io.Source.stdin.getLines()
        .toList
        .filter(_ != "")
        .map(parseList(_))
}

def comparePackets(left: PacketSegment, right: PacketSegment): Int = {
    if (left.isInstanceOf[Int] && right.isInstanceOf[Int]) {
        val iLeft = left.asInstanceOf[Int];
        val iRight = right.asInstanceOf[Int];

        if (iLeft == iRight) 0
        else if (iLeft < iRight) 1
        else -1;
    } else if (left.isInstanceOf[List[_]] && right.isInstanceOf[List[_]]) {
        val lLeft = left.asInstanceOf[List[PacketSegment]];
        val lRight = right.asInstanceOf[List[PacketSegment]];

        if (lLeft.isEmpty && !lRight.isEmpty) 1
        else if (!lLeft.isEmpty && lRight.isEmpty) -1
        else if (lLeft.isEmpty && lRight.isEmpty) 0
        else {
            val comparison = comparePackets(lLeft.head, lRight.head);
            if (comparison != 0) comparison else comparePackets(lLeft.tail, lRight.tail);
        }
    } else {
        val lLeft = if (left.isInstanceOf[Int]) List(left) else left.asInstanceOf[List[PacketSegment]];
        val lRight = if (right.isInstanceOf[Int]) List(right) else right.asInstanceOf[List[PacketSegment]];
        comparePackets(lLeft, lRight);
    }
}

object PacketOrdering extends Ordering[PacketSegment] {
    def compare(x: PacketSegment, y: PacketSegment): Int = - comparePackets(x, y);
}

def p1() = {
    getPackets
        .map { packets =>
            val result = comparePackets.tupled(packets);
            result;
        }
        .zipWithIndex
        .filter(_._1 == 1)
        .map(_._2 + 1)
        .sum
}

def p2() = {
    val List(d1, d2) = getPacketList
        .appendedAll(List(List(List(2)), List(List(6))))
        .sorted(PacketOrdering)
        .zipWithIndex
        .filter { entry =>
            val (packet, _) = entry;
            packet match {
                case List(List(2)) => true;
                case List(List(6)) => true;
                case _ => false;
            }
        }
        .map(_._2 + 1);
    
    d1 * d2;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
