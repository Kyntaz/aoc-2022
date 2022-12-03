$day = "d{0:d2}" -f $args[0];
$part = $args[1];

if(!(test-path -PathType container ./build)) {
    ni -itemType directory -path ./build;
}

echo "Compiling...";
scalac "./days/$day/$day.scala" "./common/aoc.scala" -d "./build/";

echo "Running!";
gc "./days/$day/input.txt" | & scala -cp "./build" "main" $part;
