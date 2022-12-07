import scala.collection.mutable.{ ListBuffer, Set };
import scala.annotation.tailrec;

class Directory(val name: String, val parent: Directory) {
    val contents = ListBuffer[Int | Directory]();
    var size = -1;

    def add(thing: Int | Directory) = {
        contents += thing;
    }

    def getSize: Int = {
        if (size >= 0) size else {
            getFiles.sum + getSubdirectories
                .map(_.getSize).sum;
        }
    }

    def getFiles = {
        contents.filter(_.isInstanceOf[Int])
            .map(_.asInstanceOf[Int])
            .toList;
    }

    def getSubdirectories = {
        contents.filter(_.isInstanceOf[Directory])
            .map(_.asInstanceOf[Directory])
            .toList;
    }

    def getSubDirectory(name: String) = {
        getSubdirectories
            .filter(_.name == name)
            .head;
    }

    def getAllSubdirectories: List[Directory] = {
        val self = this;
        getSubdirectories
            .flatMap(_.getAllSubdirectories)
            :+ self;
    }

    def write(prefix: String): Unit = {
        println(prefix + name);
        getSubdirectories
            .foreach(_.write(prefix + "-"));
    }
}

val ROOT: Directory = new Directory("/", null);
var CD: Directory = null;

def getCommands = {
    io.Source.stdin.getLines()
        .map(_.split(" ").toList)
        .toList;
}

def exec(command: List[String]) = {
    command match {
        case List("$", "cd", dir) => dir match {
            case "/" => CD = ROOT
            case ".." => CD = CD.parent;
            case _ => CD = CD.getSubDirectory(dir);
        }
        case List("$", "ls") => null;
        case List("dir", name) => CD.add(new Directory(name, CD));
        case List(size, _) => CD.add(size.toInt);
        case _ => null;
    }
}

def p1() = {
    getCommands.foreach(exec(_));
    ROOT.getAllSubdirectories
        .map(_.getSize)
        .filter(_ <= 100000)
        .sum;
}

def p2() = {
    getCommands.foreach(exec(_));
    val freeSpace = 70000000 - ROOT.getSize;
    val missingSpace = 30000000 - freeSpace;
    ROOT.getAllSubdirectories
        .map(_.getSize)
        .sorted
        .find(_ > missingSpace)
        .get
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p1);
    AoC.solve(2, p2);
}
