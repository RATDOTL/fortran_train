!Use double integrals (variables dr and dθ) to find the area of a circle. Compare the analytical solution with the results of the rectangular formula, trapezoidal formula, and Simpson's formula.
program FF_Integral_Lv2
implicit none
integer, parameter :: nmax = 32
real(8) :: sumrec, sumtra, sumsimp, sumsol, rmin, rmax, amin, amax, f, g, sol
real(8) :: sumrec1, sumtra1, sumsimp1, sumrec2, sumtra2, sumsimp2

amin = 0
amax = 2 * 3.1415926

write(*,'("INPUT == rmin =?")')
read(5,*) rmin
write(*,'("INPUT == rmax =?")')
read(5,*) rmax

call rec(rmin,rmax,nmax,sumrec,1)
sumrec1=sumrec
call tra(rmin,rmax,nmax,sumtra,1)
sumtra1=sumtra
call simp(rmin,rmax,nmax,sumsimp,1)
sumsimp1=sumsimp

call rec(amin,amax,nmax,sumrec,2)
sumrec2=sumrec
call tra(amin,amax,nmax,sumtra,2)
sumtra2=sumtra
call simp(amin,amax,nmax,sumsimp,2)
sumsimp2=sumsimp

sumrec = sumrec1*sumrec2
sumtra = sumtra1*sumtra2
sumsimp = sumsimp1*sumsimp2

sumsol = sol(rmin,rmax,amin,amax)

write(*,'("SUM RECTANGLE =", F10.4)')sumrec
write(*,'("SUM TRAPEZOID =", F10.4)')sumtra
write(*,'("SUM SIMPSON = ", F10.4)')sumsimp
write(*,'("EXACT SOLUTION = ", F10.4)')sumsol

end program FF_Integral_Lv2

! Integral function r
function f(r) result(y)
implicit none
real(8), intent(IN) :: r
real(8):: y
y = r
return
end function f

! Integral function θ
function g(a) result(y)
implicit none
real(8), intent(IN) :: a
real(8):: y
y = 1
return
end function g

! analytical solution
function sol(rmin,rmax,amin,amax) result(y)
implicit none
real(8), intent(IN) :: rmin,rmax,amin,amax
real(8):: y
y = 1.D0/2.D0*(rmax**2 - rmin**2)*(amax - amin)
return
end function sol

! rectangular formula
subroutine rec(xmin,xmax,nmax,sumrec,k)
implicit none
real(8), intent(IN) :: xmin,xmax
real(8), intent(OUT) :: sumrec 
integer, intent(IN) :: nmax,k
real(8) :: dx, x, f, g
integer :: ix
dx = (xmax-xmin)/dble(nmax)
sumrec = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+xmin

if(k==1) then
sumrec = sumrec +f(x)*dx
else
sumrec = sumrec +g(x)*dx
end if

end do
return
end subroutine rec

! trapezoidal formula
subroutine tra(xmin,xmax,nmax,sumtra,k)
implicit none
real(8), intent(IN) :: xmin,xmax
real(8), intent(OUT) :: sumtra
integer, intent(IN) :: nmax,k
real(8) :: dx, x, facx,f,g
integer :: ix
dx = (xmax-xmin)/dble(nmax)
sumtra = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+xmin
facx = 1.D0
if(ix==0.or.ix==nmax)facx=0.5D0

if(k==1) then
sumtra = sumtra + f(x)*dx*facx
else
sumtra = sumtra + g(x)*dx*facx
end if

end do
return
end subroutine tra

! Simpson's formula
subroutine simp(min,max,nmax,sumsimp,k)
implicit none
real(8), intent(IN) :: min,max
real(8), intent(OUT) :: sumsimp
integer, intent(IN) :: nmax,k
real(8) :: dx, x, facx,f,g
integer :: ix
dx = (max-min)/dble(nmax)
sumsimp = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+min
if(ix==0.or.ix==nmax)then
facx = dx/3.D0
else if(mod(ix,2)==1)then 
facx = 4.D0*dx/3.D0
else if(mod(ix,2)==0)then 
facx = 2.D0*dx/3.D0
else
write(*,'("SOMTHING WRONG (ix)")')
stop
end if
if(k==1) then
sumsimp = sumsimp + f(x)*facx
else
sumsimp = sumsimp + g(x)*facx
end if

end do
return
end subroutine simp

