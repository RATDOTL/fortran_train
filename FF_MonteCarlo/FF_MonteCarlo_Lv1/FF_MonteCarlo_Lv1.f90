![0,1)の一様乱数x,yを用いて、円周率πを求める。
program FF_Random_Lv1
implicit none

real(8) :: x, y, z
real(8) :: p
integer :: i, n=1000000,isum

p=0.D0
isum=0

do i=1,n
call random_seed
call random_number(x)
call random_number(y)

z=x*x+y*y

if(z<=1)then
isum=isum+1
end if

end do

p=4.D0*dble(isum)/dble(n)

write(*,*) 'p = ', p
end program FF_Random_Lv1
