program euleraprox
implicit none
real*8 :: aproxeuler, euler, x = 0.1d0, fat, erro
integer :: i

do i = 1, 5
	x = 10.0d0**(i - 2.0d0)
	euler = exp(-x)
	erro = abs(euler - aproxeuler(x))
	print*, "Aproximação:",aproxeuler(x)
	print*, "Valor exato:",euler
	print*, "Diferença:", erro
	print*,
end do

end program

real*8 function fat(n)
implicit none
integer :: j,f
integer, intent(in) :: n
f = 1
	do j = 1,n
		f = f*j
	end do
fat = f
return
end function

real*8 function aproxeuler(x)
real*8, intent(in) :: x
real*8 :: a,fat
integer N, i
N = 20

a = 1.d0
do i = 1, N
	a = a + ((-x)**i)/(fat(i))
end do
aproxeuler = a
return
end function 

! A aproximação de exp-x parece divergir com a função exata para valores de x maiores que 1 , mesmo com muitas N interações e precisão quadrupla. O erro aceitavel de E-8 só foi observado nas primieras duas aproximações, para x = 0.1 e x = 1. Nota-se também a o número de interações N chega a um limite de precisão ao qual aumenta-lo não aumenta a precisão para as primeiras aproximações, e muito menos para as outras, onde o número de interações só aumenta a divergência. Após um certo número de interações, os valores fatoriais ficam tão grandes que estoram a precisão do codigo e a aproximação passa a ser NaN.

