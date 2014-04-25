#!/bin/bash
#gnuplot -p <<< 'plot [x=0:50] [y=0:10] "gnuplot_data2.txt" using 1:2 with lines, "gnuplot_data2.txt" using 1:3 with lines'
FILENAME="$1"
XMAX="$2"
gnuplot -p <<< "plot [x=0:$XMAX] \"$FILENAME\" using 1:2 with lines, \"$FILENAME\" using 1:3 with lines"
