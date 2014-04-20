#!/bin/bash
gnuplot -p <<< 'plot [x=0:50] [y=0:10] "gnuplot_data.txt" using 1:2 with lines, "gnuplot_data.txt" using 1:3 with lines'
