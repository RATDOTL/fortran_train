!The velocity v and displacement y as a function of time t for vertical throw-up or vertical throw-down are calculated numerically and illustrated in Gnuplot along with the analytical solution.
program FF_RungeKutta_Lv1
implicit none
real(8) , parameter ::  g = 9.8D0, dt = 0.1D0, y0 = 0.D0, Vy = 20.D0 
real(8), allocatable :: y(:), v(:), t(:)
real(8) :: solVy, soly
real(8) :: k1,k2,k3,k4,l1,l2,l3,l4
real(8) :: f1, f2
integer :: i, imax=100
character*40 :: f
allocate(y(imax), v(imax), t(imax))

open(1,file='data_FF_RungeKutta_Lv1.dat')
open(2,file='sol_FF_RungeKutta_Lv1.dat')

t(1) = 0.D0
y(1) = y0
v(1) = Vy

f="(2x,F10.2,6x,F10.2, 6x, F10.2,6x,F10.2)"

do i=2,imax
    l1=dt*f1(t(i-1),v(i-1),g)
    l2=dt*f1(t(i-1)+dt/2.D0,v(i-1)+l1/2.D0,g)
    l3=dt*f1(t(i-1)+dt/2.D0,v(i-1)+l2/2.D0,g)
    l4=dt*f1(t(i-1)+dt,v(i-1)+l3,g)

    t(i)=t(i-1)+dt
    v(i)=v(i-1)+(l1+2.D0*l2+2.D0*l3+l4)/6.D0

    k1=dt*f2(t(i-1),y(i-1),g,v(i-1))
    k2=dt*f2(t(i-1)+dt/2.D0,y(i-1)+k1/2.D0,g,v(i-1)+l1/2.D0)
    k3=dt*f2(t(i-1)+dt/2.D0,y(i-1)+k2/2.D0,g,v(i-1)+l2/2.D0)
    k4=dt*f2(t(i-1)+dt,y(i-1)+k3,g,v(i-1)+l3)
    y(i)=y(i-1)+(k1+2.D0*k2+2.D0*k3+k4)/6.D0
end do

do i=1,100
    write(1,f)t(i),0.D0,y(i),v(i) 
    write(2,f)t(i),0.D0,soly(t(i),g,y(1),v(1)),solVy(t(i),g,v(1))
end do

end program FF_RungeKutta_Lv1

real(8) function f1(t,y,g) result(z)
real(8), intent(IN) :: y,g,t

z = -g

return
end function f1

real(8) function f2(t,y,g,v) result(z)
real(8), intent(IN) :: y,g,t,v

z = v

return
end function f2

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