!The solution of the single vibration m*((d^2x)/(dt^2)) = -kx is obtained numerically and illustrated in Gnuplot along with the analytical solution.
program FF_Euler_Lv3
implicit none
real(8) , parameter :: m=2.D0, k = 4.D0, dt = 0.01D0, x0 = 2.D0 
real(8), allocatable :: x(:), v(:), t(:)
real(8) :: solVx, solx, w
real(8) :: k1,k2,k3,k4,l1,l2,l3,l4
real(8) :: f1, f2
integer :: i, imax=1000
character*40 :: f
allocate(x(imax), v(imax), t(imax))

open(1,file='data_FF_RungeKutta_Lv3.dat')
open(2,file='sol_FF_RungeKutta_Lv3.dat')

t(1) = 0.D0
x(1) = x0
v(1) = 0.D0
w = sqrt(k/m)

f="(2x,F10.2,6x,F10.2, 6x, F10.2,6x,F10.2)"

do i=2,imax
    k1=dt*f1(t(i-1),v(i-1),x(i-1),w)
    k2=dt*f1(t(i-1)+dt/2.D0,v(i-1)+k1/2.D0,x(i-1)+l1/2.D0,w)
    k3=dt*f1(t(i-1)+dt/2.D0,v(i-1)+k2/2.D0,x(i-1)+l2/2.D0,w)
    k4=dt*f1(t(i-1)+dt,v(i-1)+k3,x(i-1)+k3,w)

    t(i)=t(i-1)+dt
    v(i)=v(i-1)+(k1+2.D0*k2+2.D0*k3+k4)/6.D0

    l1=dt*f2(t(i-1),v(i-1),x(i-1),w)
    l2=dt*f2(t(i-1)+dt/2.D0,v(i-1)+k1/2.D0,x(i-1)+l1/2.D0,w)
    l3=dt*f2(t(i-1)+dt/2.D0,v(i-1)+k2/2.D0,x(i-1)+l2/2.D0,w)
    l4=dt*f2(t(i-1)+dt,v(i-1)+k3,x(i-1)+l3,w)

    x(i)=x(i-1)+(l1+2.D0*l2+2.D0*l3+l4)/6.D0

end do


do i=1,1000
    write(1,f)t(i), x(i), 0.D0, v(i)
    write(2,f)t(i), solx(t(i), x(1), w), 0.D0, solVx(t(i), w, x(1), v(1))
end do

end program FF_Euler_Lv3

real(8) function f1(t,v,x,w) result(z)
real(8), intent(IN) :: t,v,x,w

z = -w**2*x

return
end function f1

real(8) function f2(t,v,x,w) result(z)
real(8), intent(IN) :: t,v,x,w

z = v

return
end function f2

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