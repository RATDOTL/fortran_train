!The velocity v and displacement y as a function of time t for vertical throw-up or vertical throw-down are calculated numerically and illustrated in Gnuplot along with the analytical solution.
program FF_Euler_Lv1
implicit none
real(8) , parameter ::  g = 9.8D0, dt = 0.1D0, y0 = 0.D0, Vy = 20.D0 
real(8), allocatable :: y(:), v(:), t(:)
real(8) :: solVy, soly
integer :: i, imax=100
character*40 :: f
allocate(y(imax), v(imax), t(imax))

open(1,file='data_FF_Euler_Lv1.dat')
open(2,file='sol_FF_Euler_Lv1.dat')

t(1) = 0.D0
y(1) = y0
v(1) = Vy

f="(2x,F10.2,6x,F10.2, 6x, F10.2,6x,F10.2)"

do i=2,imax
    t(i) = t(i-1) + dt
    v(i) = v(i-1) - g*dt
end do

do i=2,imax
    y(i) = y(i-1) + v(i)*dt
end do

do i=1,100
    write(1,f)t(i), 0.D0, y(i), v(i)
    write(2,f)t(i), 0.D0, soly(t(i), g, y(1), v(1)), solVy(t(i), g, v(1))
end do

end program FF_Euler_Lv1

!Function:Analytical solution for speed
real*8 function solVy(tt, g, v0)
real(8), intent(IN) :: tt, g, v0

solVy = v0 - g*tt

return
end

!Function:Analytical solution for displacement y
real*8 function soly(tt, g, y0, v0)
real(8), intent(IN) :: tt, g, y0, v0

soly = y0 - 0.5D0*g*tt**2 + v0*tt

return
end