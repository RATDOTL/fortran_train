set yrange [-5:5]
plot "data_FF_Euler_Lv3.dat" using 1:4 w p pt 2 ps 2
replot "sol_FF_Euler_Lv3.dat" using 1:4 w p pt 3 ps 2
