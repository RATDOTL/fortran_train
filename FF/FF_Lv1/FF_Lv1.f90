!The position of the object at time t is calculated numerically and displayed in gnuplot.
program FFLv1
implicit none
real(8) , parameter ::  g = 9.8D0, t= 5.D0, Vx = 5.D0, Vy = 2.D0 
real(8) :: y, x

open(1,file='data_FF_Lv1.dat')

y = Vy * t -1.D0 / 2.D0 * g * t ** 2

x = Vx * t

write(1,'(2F10.3)')0.D0, 0.D0
write(1,'(2F10.3)')x, y

end program FFLv1
