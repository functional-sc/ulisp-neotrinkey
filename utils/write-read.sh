#/bin/bash
# TODO set stty to 9600 baud
echo "(+ 1 1 40)" > /dev/ttyACM0 & tail -f /dev/ttyACM0 > out.txt & (sleep 2 ; killall tail)
#cat out.txt
sed 1,1d out.txt | head -n 1
