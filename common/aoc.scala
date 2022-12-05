package AoC;

import collection.mutable.ListBuffer;

private var PART = 0;

/**
  * Declares that you are starting to solve an AoC problem.
  *
  * @param part the part you are attempting to solve.
  */
def start(part: Int) = {
    PART = part;
}

/**
  * Declares the solution to a given part of the AoC problem.
  *
  * @param part the part to which the solution applies.
  * @param solution the solution to the given part.
  */
def solve(part: Int, solution: () => Any) = {
    if (part == PART) {
        println(s"P$part: ${solution()}");
    }
}

package utils {
    /**
      * Splits a list into a list of lists, using elements that satisfy the `predicate` as separators.
      * The separators are excluded from the result.
      *
      * @param predicate the predicate that decides whether an element is a separator or not.
      * @return the list of lists.
      */
    extension [T](l: List[T]) def blockBy(predicate: T => Boolean) = {
        val blocks = ListBuffer[List[T]]();
        var block = ListBuffer[T]();

        for (el <- l) {
            if (predicate(el)) {
                blocks += block.toList;
                block = ListBuffer[T]();
            } else {
                block += el;
            }
        }

        if (!block.isEmpty) {
            blocks += block.toList;
        }

        blocks.toList;
    }
}
