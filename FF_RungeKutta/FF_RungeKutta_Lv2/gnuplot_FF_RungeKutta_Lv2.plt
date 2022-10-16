set xrange [0:200]
set yrange [-300:25]
plot "data_FF_Euler_Lv2.dat" using 2:4 w p pt 2 ps 2
replot "data_FF_RungeKutta_Lv2.dat" using 2:4 w p pt 3 ps 2
replot "sol_FF_RungeKutta_Lv2.dat" using 2:4 w p pt 4 ps 2
