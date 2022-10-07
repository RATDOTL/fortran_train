set yrange [-20:20]
plot "data_FF_Euler_Lv1.dat" using 1:3 w p pt 2 ps 2
replot "sol_FF_Euler_Lv1.dat" using 1:3 w p pt 3 ps 2
