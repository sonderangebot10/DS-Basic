set term postscript eps enhanced color
set output "results3.eps"
set xlabel "Number of processors"
set ylabel "Handle time (ms)"
set xrange [0:10]
set yrange [0:2500]

plot "experiment3.dat" using 1:2 with linespoints title "Experiment"

