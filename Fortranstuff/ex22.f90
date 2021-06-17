Program integral
Implicit none
Real*8 :: b, a, f, h, trapezio, simpson, inttrapezio, intsimpson
real :: sf, sh, sa, sb, strapezio, ssimpson, sinttrapezio, sintsimpson
Integer :: i, j, N

b = 1.d0
a = 0.0d0
N = 4 

sb = 1.
sa = 0.

do i = 1, 10
	sh = (sb - sa)/N
	sinttrapezio = 0.0
	sintsimpson = 0.0
	do j = 1, N/2
		sinttrapezio = sinttrapezio + strapezio(sh,(2*j - 1)*sh)
		sintsimpson = sintsimpson + ssimpson(sh,(2*j - 1)*sh)
	end do
	print*,
	print*, "Para N =", N
	print*, "Valor Exato:", (1.d0-(exp(-1.d0)))
	print*, "Resultado p/ Regra do Trapézio com precisão simples:", sinttrapezio
	print*, "Resultado p/ Regra de Simpson com precisão simples: ", sintsimpson
	N = N*2
end do

N = 4

do i = 1, 10
	h = (b - a)/N
	inttrapezio = 0.d0
	intsimpson = 0.d0
	do j = 1, N/2
		inttrapezio = inttrapezio + trapezio(h,(2*j - 1)*h)
		intsimpson = intsimpson + simpson(h,(2*j - 1)*h)
	end do
	print*,
	print*, "Para N =", N
	print*, "Valor Exato:", (1.d0-(exp(-1.d0)))
	print*, "Resultado p/ Regra do Trapézio com precisão dupla: ", inttrapezio
	print*, "Resultado p/ Regra de Simpson com precisão dupla:  ", intsimpson
	N = N*2
end do

end program

real function sf(sx)
real, intent(in) :: sx
sf = exp(-(sx))
return
end function

real function strapezio(sh,sx)
real, intent(in) :: sh,sx
real :: sf
strapezio = (sh/2.)*(sf(sx - sh) + 2.*sf(sx) + sf(sx + sh))
return
end function

real function ssimpson(sh,sx)
real, intent(in) :: sh,sx
real :: sf
ssimpson = (sh/3.)*(sf(sx - sh) + 4.*sf(sx) + sf(sx + sh))
return
end function

real*8 function f(x)
real*8, intent(in) :: x
f = exp(-(x))
return
end function

real*8 function trapezio(h,x)
real*8, intent(in) :: h,x
real*8 :: f
trapezio = (h/2.d0)*(f(x - h) + 2.d0*f(x) + f(x + h))
return
end function

real*8 function simpson(h,x)
real*8, intent(in) :: h,x
real*8 :: f
simpson = (h/3.d0)*(f(x - h) + 4.d0*f(x) + f(x + h))
return
end function

!Nesse caso pode-se observar que com o aumento de pontos da rede N, ou seja, um menor valor para h, ps valores das integrais fica cada vez mais preciso sem haver aparentes excessões, como havia no caso das derivadas.
