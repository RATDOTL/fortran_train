!The trajectory of the motion from time t=0 to an arbitrary time is calculated numerically and displayed in gnuplot.
program FFLv2
implicit none
real(8) , parameter ::  g = 9.8D0, Vx = 5.D0, Vy = 10.D0 
real(8) :: y, x
integer :: t

open(1,file='data_FF_Lv2.dat')

do t = 0, 50
  y = Vy*dble(t)/10.D0 - 0.5D0*g*(dble(t)/10.D0)**2
  x = Vx*dble(t)/10.D0
  write(1,'(2F10.3)') x, y
end do

end program FFLv2
