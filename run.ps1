$day = "d{0:d2}" -f $args[0];

if(!(test-path -PathType container ./build)) {
    ni -itemType directory -path ./build;
}

g++ "./src/$day/main.cpp" -o "./build/$day.exe" -I.;
gc "./src/$day/input.txt" | & "./build/$day.exe";
