!Convert the uniform random number in [0,1) to a uniform random number in [1,e), find the definite integral of log(x)/x from 1 to e, and compare with the analytical solution.
program FF_MonteCarlo_Lv3
implicit none

real(8) :: x, y, z
real(8) :: p
integer :: i, n=1000000,isum,jsum
real(8) :: f1

p=0.D0
isum=0
jsum=0

do i=1,n
call random_seed
call random_number(x)
call random_number(y)

x=x*exp(1.D0)
y=y*exp(1.D0)

if (x>1.D0) then
jsum=jsum+1

if(y<f1(x))then
isum=isum+1
end if

end if
end do

p=(exp(1.D0)-1)*exp(1.D0)*dble(isum)/dble(jsum)

write(*,*) 'p = ', p
write(*,*) 'sol = ', log(exp(1.D0))**2/2.D0
end program FF_MonteCarlo_Lv3

real(8) function f1(x) result(z)
real(8), intent(IN) :: x

z = log(x)/x

return
end function f1