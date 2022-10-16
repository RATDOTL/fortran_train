!The velocity v and displacements x and y as a function of time t for horizontal or projectile projection are calculated numerically and illustrated in Gnuplot along with the analytical solution.
program FF_Euler_Lv2
implicit none
real(8) , parameter ::  g = 9.8D0, dt = 0.1D0, y0 = 0.D0, x0 = 0.D0, Vx = 20.D0, Vy = 20.D0 
real(8), allocatable :: y(:), x(:), v1(:), v2(:), t(:)
real(8) :: solVx, solx, solVy, soly
integer :: i, imax=100
character*60 :: f
allocate(y(imax), x(imax) , v1(imax), v2(imax), t(imax))

open(1,file='data_FF_Euler_Lv2.dat')
open(2,file='sol_FF_Euler_Lv2.dat')

t(1) = 0.D0
x(1) = x0
v1(1) = Vx
y(1) = y0
v2(1) = Vy

f="(2x,F10.2,6x,F10.2, 6x, F10.2,6x,F10.2, 6x,F10.2)"

do i=2,imax
    t(i) = t(i-1) + dt
    v1(i) = v1(i-1)
    v2(i) = v2(i-1) - g*dt
end do

do i=2,imax
    x(i) = x(i-1) + v1(i)*dt
    y(i) = y(i-1) + v2(i)*dt
end do

do i=1,100
    write(1,f)t(i), x(i), v1(i), y(i), v2(i)
    write(2,f)t(i), solx(t(i), x(1), v1(1)), solVx(v1(1)), soly(t(i), g, y(1), v2(1)), solVy(t(i), g, v2(1))
end do

end program FF_Euler_Lv2

!Function:Analytical solution for speed x
real*8 function solVx(v0)
real(8), intent(IN) :: v0

solVx = v0

return
end

!Function:Analytical solution for displacement x
real*8 function solx(tt, x0, v0)
real(8), intent(IN) :: tt, x0, v0

solx = x0 + v0*tt

return
end

!Function:Analytical solution for speed y
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