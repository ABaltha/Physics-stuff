program deriv

implicit none

real*8 :: h(10), f, p4deriv1, p5deriv1, p4deriv2, p5deriv2
real*8 :: p4deriv3, p5deriv3, p5deriv4, pi, fexata, d1exata

real :: sh(10), sf, sp4deriv1, sp5deriv1, sp4deriv2, sp5deriv2
real :: sp4deriv3, sp5deriv3, sp5deriv4, spi, sfexata, sd1exata

integer :: i

pi = 4.d0*atan(1.d0)
fexata = sin(pi/2.d0)
d1exata = cos(pi/2.d0)

spi = 4.0*atan(1.0)
sfexata = sin(spi/2.0)
sd1exata = cos(spi/2.0)

sh(1) = 0.5
sh(2) = 0.2
sh(3) = 0.1
sh(4) = 2.0e-2
sh(5) = 1.0e-2
sh(6) = 1.0e-3
sh(7) = 1.0e-4
sh(8) = 1.0e-5
sh(9) = 1.0e-6
sh(10) = 1.0e-7

h(1) = 0.5d0
h(2) = 0.2d0
h(3) = 0.1d0
h(4) = 2.0d-2
h(5) = 1.0d-2
h(6) = 1.0d-3
h(7) = 1.0d-4
h(8) = 1.0d-5
h(9) = 1.0d-6
h(10) = 1.0d-7

do i = 1, 10
	print*,
	print*, "Primeira derivada com precisão simples"
	print*, "h =", sh(i)
	print*, "Derivada com 4 pontos:", sp4deriv1(sh(i))
	print*, "Derivada com 5 pontos:", sp5deriv1(sh(i))
	print*, "Derivada exata:", sd1exata
end do
do i = 1, 10
	print*,
	print*, "Segunda derivada com precisão simples"
	print*, "h =", sh(i)
	print*, "Derivada com 4 pontos:", sp4deriv2(sh(i))
	print*, "Derivada com 5 pontos:", sp5deriv2(sh(i))
	print*, "Derivada Exata:", (-sfexata)
end do
do i = 1, 10
	print*,
	print*, "Terceira derivada com precisão simples"
	print*, "h =", sh(i)
	print*, "Derivada com 4 pontos:", sp4deriv3(sh(i))
	print*, "Derivada com 5 pontos:", sp5deriv3(sh(i))
	print*, "Derivada exata:", (-sd1exata)
end do
do i = 1, 10
	print*,
	print*, "Quarta Derivada com precisão simples"
	print*, "h =", sh(i)
	print*, "Derivada com 5 pontos:", sp5deriv4(sh(i))
	print*, "Derivada exata:", sfexata
end do
!dupla
do i = 1, 10
	print*,
	print*, "Primeira derivada com precisão dupla"
	print*, "h =", h(i)
	print*, "Derivada com 4 pontos:", p4deriv1(h(i))
	print*, "Derivada com 5 pontos:", p5deriv1(h(i))
	print*, "Derivada exata:", d1exata
end do
do i = 1, 10
	print*,
	print*, "Segunda derivada com precisão dupla"
	print*, "h =", h(i)
	print*, "Derivada com 4 pontos:", p4deriv2(h(i))
	print*, "Derivada com 5 pontos:", p5deriv2(h(i))
	print*, "Derivada Exata:", (-fexata)
end do
do i = 1, 10
	print*,
	print*, "Terceira derivada com precisão dupla"
	print*, "h =", h(i)
	print*, "Derivada com 4 pontos:", p4deriv3(h(i))
	print*, "Derivada com 5 pontos:", p5deriv3(h(i))
	print*, "Derivada exata:", (-d1exata)
end do
do i = 1, 10
	print*,
	print*, "Quarta Derivada com precisão dupla"
	print*, "h =", h(i)
	print*, "Derivada com 5 pontos:", p5deriv4(h(i))
	print*, "Derivada exata:", fexata
end do

end program


real function sf(sh)
real, intent(in) :: sh
real :: spi
spi = 4.0*atan(1.0)
sf = sin((spi/2.0) + sh)
return
end function

real function sp4deriv1(sh)
real, intent(in) :: sh
real :: sf
sp4deriv1 = ((1.0/6.0)*(-2.0*sf(-1.0*sh) - (3.0*sf(0.0)) + (6.0*sf(sh)) - sf(2.0*sh)))/sh
return
end function

real function sp5deriv1(sh)
real, intent(in) :: sh
real :: sf
sp5deriv1 = ((1.0/12.0)*(sf(-2.0*sh) - 8.0*sf(-sh) + 8.0*sf(sh) -sf(2.0*sh)))/sh
return
end function

real function sp4deriv2(sh)
real, intent(in) :: sh
real :: sf
sp4deriv2 = (sf(-sh) - 2.0*sf(0.0) + sf(sh))/(sh**2.0)
return
end function

real function sp5deriv2(sh)
real, intent(in) :: sh
real :: sf
sp5deriv2 = (1.0/12.0)*(-sf(-2.0*sh) + 16.0*sf(-sh) - 30.0*sf(0.0) + 16.0*sf(sh) - sf(2.0*sh))/(sh**2.0)
return
end function

real function sp4deriv3(sh)
real, intent(in) :: sh
real :: sf
sp4deriv3 = (-sf(-sh) + 3.0*sf(0.0) - 3.0*sf(sh) + sf(2.0*sh))/(sh**3.0)
return
end function

real function sp5deriv3(sh)
real, intent(in) :: sh
real :: sf
sp5deriv3 = (1.0/2.0)*(-sf(-2.0*sh) + 2.0*sf(-sh) - 2.0*sf(sh) + sf(2.0*sh))/(sh**3.0)
return
end function

real function sp5deriv4(sh)
real, intent(in) :: sh
real :: sf
sp5deriv4 = (sf(-2.0*sh) - 4.0*sf(-sh) + 6.0*sf(0.0) - 4.0*sf(sh) + sf(2.0*sh))/(sh**4.0)
return
end function

!double

real*8 function f(h)
real*8, intent(in) :: h
real*8 :: pi
pi = 4.d0*atan(1.d0)
f = sin((pi/2.d0) + h)
return
end function

real*8 function p4deriv1(h)
real*8, intent(in) :: h
real*8 :: f
p4deriv1 = ((1.d0/6.d0)*(-2.d0*f(-1.d0*h) - (3.d0*f(0.d0)) + (6.d0*f(h)) - f(2.d0*h)))/h
return
end function

real*8 function p5deriv1(h)
real*8, intent(in) :: h
real*8 :: f
p5deriv1 = ((1.d0/12.d0)*(f(-2.d0*h) - 8.d0*f(-h) + 8.d0*f(h) -f(2.d0*h)))/h
return
end function

real*8 function p4deriv2(h)
real*8, intent(in) :: h
real*8 :: f
p4deriv2 = (f(-h) - 2.d0*f(0.d0) + f(h))/(h**2.d0)
return
end function

real*8 function p5deriv2(h)
real*8, intent(in) :: h
real*8 :: f
p5deriv2 = (1.d0/12.d0)*(-f(-2.d0*h) + 16.d0*f(-h) - 30.d0*f(0.d0) + 16.d0*f(h) - f(2.d0*h))/(h**2.d0)
return
end function

real*8 function p4deriv3(h)
real*8, intent(in) :: h
real*8 :: f
p4deriv3 = (-f(-h) + 3.d0*f(0.d0) - 3.d0*f(h) + f(2.d0*h))/(h**3.d0)
return
end function

real*8 function p5deriv3(h)
real*8, intent(in) :: h
real*8 :: f
p5deriv3 = (1.d0/2.d0)*(-f(-2.d0*h) + 2.d0*f(-h) - 2.d0*f(h) + f(2.d0*h))/(h**3.d0)
return
end function

real*8 function p5deriv4(h)
real*8, intent(in) :: h
real*8 :: f
p5deriv4 = (f(-2.d0*h) - 4.d0*f(-h) + 6.d0*f(0.d0) - 4.d0*f(h) + f(2.d0*h))/(h**4.d0)
return
end function

!Os resultados mostram a natureza um tanto instável dessas aproximações, principalmente na terceira e quarta derivada, com valores cada vez menores de h surgem resultados com valores enormes ou que zeram de repente, mostrando que a precisão e otimização das aproximações não está necessáriamente relacionada a h.
