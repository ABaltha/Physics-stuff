program intbode

implicit none

real*8 :: f, bode, h, b, a, pi, sumbode
real :: sf, sbode, sh, sb, sa, spi, ssumbode
integer :: i, j, N

spi = 4.0*atan(1.0)
sb = 1.0
sa = 0.0

pi = 4.d0*atan(1.d0)
b = 1.d0
a = 0.d0

N = 4
do i = 1, 10
	sh = (sb - sa)/(N)
	ssumbode = 0.0
	do j = 1, N/4
		ssumbode = ssumbode  + sbode(4*(j-1)*sh,sh)
	end do
	print*,
	print*, "Para N:", N
	print*, "Valor obtido pela Regra de Bode com precisão simples:", ssumbode
	print*, "Valor Exato:", (spi/2.0)
	N = N*2
end do
N = 4
do i = 1, 10
	h = (b - a)/(N)
	sumbode = 0.d0
	do j = 1, N/4
		sumbode = sumbode  + bode(4*(j-1)*h,h)
	end do
	print*,
	print*, "Para N:", N
	print*, "Valor obtido pela Regra de Bode com precisão dupla:", sumbode
	print*, "Valor Exato:", (pi/2.d0)
	N = N*2
end do
!print*, f(0.d0)
!print*, f(1.d0)

end program
real function sf(sx)
real, intent(in) :: sx
real :: st
st = sqrt(1.0 - sx)
sf = 2.0*((2.0 - (st**2.0))**(-1.0/2.0))
!f = ((1.d0 - x**2)**(-1.d0/2.d0))
return
end function

real function sbode(sx,sh)
real, intent(in) :: sx,sh
real :: sf
sbode = (2.0*sh/45.0)*(7.0*sf(sx) + 32.0*sf(sx+sh) + 12.0*sf(sx+(2.0*sh)) + 32.0*sf(sx+(3.0*sh)) +7.0*sf(sx + 4.0*sh))
return
end function

real*8 function f(x)
real*8, intent(in) :: x
real*8 :: t
t = sqrt(1.d0 - x)
f = 2.d0*((2.d0 - (t**2.d0))**(-1.d0/2.d0))
!f = x**2 + x**4 + 2.d0*x
return
end function

real*8 function bode(x,h)
real*8, intent(in) :: x,h
real*8 :: f
bode = (2.d0*h/45.d0)*(7.d0*f(x) + 32.d0*f(x+h) + 12.d0*f(x+(2.d0*h)) + 32.d0*f(x+(3.d0*h)) +7.d0*f(x + 4.d0*h))
return
end function

!Vasculhei e mudei meu código muitas vezes procurando o que causa esse erro no valor obtido, o intrigante é que com outras funções que testei, o valor é correto. Provavelmente deve ter algo a ver com a mudança de variável
