set yrange [-20:25]
plot "data_FF_Euler_Lv1.dat" using 1:3 w p pt 2 ps 2
replot "data_FF_RungeKutta_Lv1.dat" using 1:3 w p pt 3 ps 2
replot "sol_FF_RungeKutta_Lv1.dat" using 1:3 w p pt 4 ps 2
