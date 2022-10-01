!The trajectory of the motion up to an arbitrary time is calculated numerically and a gif animation is created in gnuplot.
program FFLv3
implicit none
real(8) , parameter ::  g = 9.8D0, Ax = 5.D0, Ay = 8.660254D0 !Ax = 5.D0, Ay = 8.660254D0, Bx = -5.D0, By = 8.660254D0, Cx = 5.D0, Cy = 5.D0, Dx = -5.D0, Dy = 5.D0, Ex = 5.D0, Ey = 2.8867513D0, Fx = -5.D0, Fy = 2.8867513D0
real(8) :: y, x
integer :: t, i=0

open(1,file='dataA_FF_Lv3.dat')
do i = 0, 50
do t = 0, i
  y = Ay*dble(t)/10.D0 - 0.5D0*g*(dble(t)/10.D0)**2
  x = Ax*dble(t)/10.D0
  write(1,'(2F10.3)') x, y
  if (t == i) then
    write(1,*)''
    write(1,*)''
  end if
end do
end do

end program FFLv3
