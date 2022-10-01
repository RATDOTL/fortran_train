set nokey
set xrange[-25:25]
set yrange[-110:10]
set xlabel "x [m]"
set ylabel "y [m]"
set term gif animate
set output "PM_Lv3.gif"
do for [n=0:50]{
plot "data1_FF_Lv3.dat" index n using 1:2 w p ps 2,\
"data2_FF_Lv3.dat" index n using 1:2 w p ps 2,\
"data3_FF_Lv3.dat" index n using 1:2 w p ps 2,\
"data4_FF_Lv3.dat" index n using 1:2 w p ps 2,\
"data5_FF_Lv3.dat" index n using 1:2 w p ps 2,\
"data6_FF_Lv3.dat" index n using 1:2 w p ps 2
}
set output

