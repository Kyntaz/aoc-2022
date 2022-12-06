def getSignal = {
    io.StdIn.readLine();
}

def p(windowSize: Int) = () => {
    val (_, i) = getSignal.toList
        .sliding(windowSize)
        .zipWithIndex
        .filter { iWindow =>
            val (window, _) = iWindow;
            window.length == window.toSet.size;
        }.toList.head;
    i + windowSize;
}

@main def main(part: Int) = {
    AoC.start(part);
    AoC.solve(1, p(4));
    AoC.solve(2, p(14));
}
