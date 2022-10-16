!The velocity v and displacements x and y as a function of time t for horizontal or projectile projection are calculated numerically and illustrated in Gnuplot along with the analytical solution.
program FF_RungeKutta_Lv2
implicit none
real(8) , parameter ::  g = 9.8D0, dt = 0.1D0, y0 = 0.D0, x0 = 0.D0, Vx = 20.D0, Vy = 20.D0 
real(8), allocatable :: y(:), x(:), v1(:), v2(:), t(:)
real(8) :: solVx, solx, solVy, soly
real(8) :: k1,k2,k3,k4, l1,l2,l3,l4
real(8) :: f1, f2, g1, g2 !g(x),f(y)
integer :: i, imax=100
character*60 :: f
allocate(y(imax), x(imax) , v1(imax), v2(imax), t(imax))

open(1,file='data_FF_RungeKutta_Lv2.dat')
open(2,file='sol_FF_RungeKutta_Lv2.dat')

t(1) = 0.D0
x(1) = x0
v1(1) = Vx
y(1) = y0
v2(1) = Vy

f="(2x,F10.2,6x,F10.2, 6x, F10.2,6x,F10.2, 6x,F10.2)"

do i=2,imax
    k1=dt*f1(t(i-1),v2(i-1),g)
    k2=dt*f1(t(i-1)+dt/2.D0,v2(i-1)+k1/2.D0,g)
    k3=dt*f1(t(i-1)+dt/2.D0,v2(i-1)+k2/2.D0,g)
    k4=dt*f1(t(i-1)+dt,v2(i-1)+k3,g)
    t(i)=t(i-1)+dt
    v2(i)=v2(i-1)+(k1+2.D0*k2+2.D0*k3+k4)/6.D0

    l1=dt*g1(t(i-1),v1(i-1))
    l2=dt*g1(t(i-1)+dt/2.D0,v1(i-1)+l1/2.D0)
    l3=dt*g1(t(i-1)+dt/2.D0,v1(i-1)+l2/2.D0)
    l4=dt*g1(t(i-1)+dt,v1(i-1)+l3)
    t(i)=t(i-1)+dt
    v1(i)=v1(i-1)+(l1+2.D0*l2+2.D0*l3+l4)/6.D0

end do

do i=2,imax
    k1=dt*f2(t(i-1),y(i-1),g,v2(1))
    k2=dt*f2(t(i-1)+dt/2.D0,y(i-1)+k1/2.D0,g,v2(1))
    k3=dt*f2(t(i-1)+dt/2.D0,y(i-1)+k2/2.D0,g,v2(1))
    k4=dt*f2(t(i-1)+dt,y(i-1)+k3,g,v2(1))
    y(i)=y(i-1)+(k1+2.D0*k2+2.D0*k3+k4)/6.D0

    l1=dt*g2(t(i-1),x(i-1),v1(1))
    l2=dt*g2(t(i-1)+dt/2.D0,x(i-1)+l1/2.D0,v1(1))
    l3=dt*g2(t(i-1)+dt/2.D0,x(i-1)+l2/2.D0,v1(1))
    l4=dt*g2(t(i-1)+dt,x(i-1)+l3,v1(1))
    x(i)=x(i-1)+(l1+2.D0*l2+2.D0*l3+l4)/6.D0

end do

do i=1,100
    write(1,f)t(i), x(i), v1(i), y(i), v2(i)
    write(2,f)t(i), solx(t(i), x(1), v1(1)), solVx(v1(1)), soly(t(i), g, y(1), v2(1)), solVy(t(i), g, v2(1))
end do

end program FF_RungeKutta_Lv2

real(8) function f1(t,y,g) result(z)
real(8), intent(IN) :: y,g,t

z = -g

return
end function f1

real(8) function f2(t,y,g,v0) result(z)
real(8), intent(IN) :: y,g,t,v0

z = -g*t + v0

return
end function f2

real(8) function g1(t,x) result(z)
real(8), intent(IN) :: t,x

z = 0.D0

return
end function g1

real(8) function g2(t,x,v0) result(z)
real(8), intent(IN) :: t,x,v0

z = v0

return
end function g2



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