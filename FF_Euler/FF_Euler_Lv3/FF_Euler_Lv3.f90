!The solution of the single vibration m*((d^2x)/(dt^2)) = -kx is obtained numerically and illustrated in Gnuplot along with the analytical solution.
program FF_Euler_Lv3
implicit none
real(8) , parameter :: m=2.D0, k = 4.D0, dt = 0.01D0, x0 = 2.D0 
real(8), allocatable :: x(:), v(:), t(:)
real(8) :: solVx, solx, w
integer :: i, imax=1000
character*40 :: f
allocate(x(imax), v(imax), t(imax))

open(1,file='data_FF_Euler_Lv3.dat')
open(2,file='sol_FF_Euler_Lv3.dat')

t(1) = 0.D0
x(1) = x0
v(1) = 0.D0
w = sqrt(k/m)

f="(2x,F10.2,6x,F10.2, 6x, F10.2,6x,F10.2)"

do i=2,imax
    t(i) = t(i-1) + dt
    v(i) = v(i-1) - w**2*x(i-1)*dt
    x(i) = x(i-1) + v(i-1)*dt
end do

do i=1,1000
    write(1,f)t(i), x(i), 0.D0, v(i)
    write(2,f)t(i), solx(t(i), x(1), w), 0.D0, solVx(t(i), w, x(1), v(1))
end do

end program FF_Euler_Lv3

!Function:Analytical solution for speed
real*8 function solVx(tt, w, x0, v0)
real(8), intent(IN) :: tt, w, x0, v0

solVx = -x0*w*sin(w*tt)

return
end

!Function:Analytical solution for displacement y
real*8 function solx(tt, x0, w)
real(8), intent(IN) :: tt, x0, w

solx = x0*cos(w*tt)

return
end