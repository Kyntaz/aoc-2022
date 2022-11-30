$day = "d{0:d2}" -f $args[0];

if(!(test-path -PathType container ./build)) {
    ni -itemType directory -path ./build;
}

scalac "./days/$day/$day.scala" "./utils/aoc.scala" -d "./build/";
gc "./days/$day/input.txt" | & scala -cp "./build" "main";
