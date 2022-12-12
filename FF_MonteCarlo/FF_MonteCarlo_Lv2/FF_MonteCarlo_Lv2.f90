!Find the definite integral of e^(-x) from 0 to 1 using uniform random numbers x,y in [0,1] and compare with the analytical solution.
program FF_Random_Lv2
implicit none

real(8) :: x, y, z
real(8) :: p
integer :: i, n=10000,isum
real(8) :: f1

p=0.D0
isum=0

do i=1,n
call random_seed
call random_number(x)
call random_number(y)


if(y<f1(x))then
isum=isum+1
end if

end do

p=dble(isum)/dble(n)

write(*,*) 'p = ', p
write(*,*) 'sol = ', 1.D0-exp(-1.D0)
end program FF_Random_Lv2

real(8) function f1(x) result(z)
real(8), intent(IN) :: x

z = exp(-x)

return
end function f1