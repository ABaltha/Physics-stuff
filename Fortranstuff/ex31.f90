program newrap
implicit none
real*8 :: dx, x, f, ep, x0, nrap
integer :: i = 0

dx = 0.2d0
x0 = .22d0
ep = 1d-8
x = x0

open(10,file="newrap.dat")
!write(10,*) "x", "f(x)"
do while (abs(f(x)) > ep)
	write(10,*) x, f(x),i,(abs(sqrt(5.0d0))-x) 
	x = x + nrap(x)
	
!	if(f(x)*f(x + dx) < 0) then
!		dx = -dx/2.0
	i = i + 1
!	else
!		x = x + dx
!	end if
end do


print*, abs(x), i

end program

real*8 function nrap(x)
real*8, intent(in) :: x
nrap = - (x**2.0d0 - 5.d0)/(2.0d0*x)
return
end function

real*8 function f(x)
real*8, intent(in) :: x
f = x**2.d0 - 5.d0
return
end function


!Com o grafico gerado pode-se ver que a paroximação do valor exato segue uma função quadratica
