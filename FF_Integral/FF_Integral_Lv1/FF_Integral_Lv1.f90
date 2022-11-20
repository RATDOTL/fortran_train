
program FF_Integral_Lv1
implicit none
integer, parameter :: nmax = 64
real(8) :: sumrec, sumtra, sumsimp, sumsol, xmin, xmax, f, sol

write(*,'("INPUT == Xmin =?")')
read(5,*) xmin
write(*,'("INPUT == Xmax =?")')
read(5,*) xmax

call rec(xmin,xmax,nmax,sumrec)
call tra(xmin,xmax,nmax,sumtra)
call simp(xmin,xmax,nmax,sumsimp)
sumsol = sol(xmin,xmax)

write(*,'("SUM RECTANGLE =", F10.4)')sumrec
write(*,'("SUM TRAPEZOID =", F10.4)')sumtra
write(*,'("SUM SIMPSON = ", F10.4)')sumsimp
write(*,'("EXACT SOLUTION = ", F10.4)')sumsol

end program FF_Integral_Lv1

! Integral function
function f(x) result(y)
implicit none
real(8), intent(IN) :: x
real(8):: y
y = x**7
return
end function f

! analytical solution
function sol(xmin,xmax) result(y)
implicit none
real(8), intent(IN) :: xmin,xmax
real(8):: y
y = 1.D0/8.D0*(xmax**8 - xmin**8)
return
end function sol

! rectangular formula
subroutine rec(xmin,xmax,nmax,sumrec)
implicit none
real(8), intent(IN) :: xmin,xmax 
real(8), intent(OUT) :: sumrec 
integer, intent(IN) :: nmax 
real(8) :: dx, x, f
integer :: ix
dx = (xmax-xmin)/dble(nmax)
sumrec = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+xmin
sumrec = sumrec +f(x)*dx
end do
return
end subroutine rec

! trapezoidal formula
subroutine tra(xmin,xmax,nmax,sumtra)
implicit none
real(8), intent(IN) :: xmin,xmax 
real(8), intent(OUT) :: sumtra
integer, intent(IN) :: nmax 
real(8) :: dx, x, facx, f
integer :: ix
dx = (xmax-xmin)/dble(nmax)
sumtra = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+xmin
facx = 1.D0
if(ix==0.or.ix==nmax)facx=0.5D0
sumtra = sumtra + f(x)*dx*facx
end do
return
end subroutine tra

! Simpson's formula
subroutine simp(xmin,xmax,nmax,sumsimp)
implicit none
real(8), intent(IN) :: xmin,xmax 
real(8), intent(OUT) :: sumsimp
integer, intent(IN) :: nmax 
real(8) :: dx, x, facx, f
integer :: ix
dx = (xmax-xmin)/dble(nmax)
sumsimp = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+xmin
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
sumsimp = sumsimp + f(x)*facx
end do
return
end subroutine simp

