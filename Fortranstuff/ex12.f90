PROGRAM OVERFLOWsimple
IMPLICIT NONE
REAL :: over = 1.0,under = 1.0
real :: oldover
real*8 :: dover = 1.d0, dunder = 1.d0,doldover,dlastover,dlastunder
INTEGER :: loopN, i,firstover,firstunder,j = 1,k = 1,dloopN,di,dj = 1,dk = 1
integer :: dfirstover,dfirstunder
do loopN = 115,155 
!testo varios numeros de interaçõe sdiferentes, sempre resetando os valores de over e under para achar quando eles vão a 0 ou inf
!Toda essa quantidade de codigo a mais, os IF em especifico, servem para detectar os valores de over e under e salvar em qual interação eles acontecem
!k e j servem apenas para que os IF salvem apenas a primeira vez que detectar 0 ou inf
over = 1.0
under = 1.0
do i = 1, loopN
	over = over*2.0
	under = under/2.0
	if (oldover == over) then
		if (j == 1) then
			firstover = loopN - 1
			j = j + 1
		else
		end if
	else
	end if
	if (under == 0) then
		if (k == 1) then
			firstunder = loopN
			k = k + 1
		else
		end if
	else
	end if
	oldover = over
end do
print*, over, under
print*, "numero de interações:",loopN
print*,
end do
!Nota-se que os numeros vao até aprox e+-38 o que faz sentido consideirando que o tamanho de cada um é 4 bytes
!repito o programa com precisão dupla
do dloopN = 1000,1800 
dover = 1.0d0
dunder = 1.0d0
do di = 1, dloopN
	dover = dover*2.0d0
	dunder = dunder/2.0d0
	if (doldover == dover) then
		if (dj == 1) then
			dfirstover = dloopN - 1
			dj = dj + 1
		else
		dlastover = doldover
		end if
	else
	end if
	if (dunder == 0) then
		if (dk == 1) then
			dfirstunder = dloopN
		dk = dk + 1
			dlastunder = doldover
		else
		end if
	else
	end if
	doldover = dover
end do
!print*, dover, dunder
!print*, "numero de interações com precisão dupla:",dloopN
!print*, !desativei esses prints para evitar flood
end do
if (dj == 1 .and. dk == 1) then
	if (j == 1 .and. k == 1) then
		print*, "Overflow e Underflow não foram alcançados"
	else
		print*, "Somente com precisão simples foram alcançados"
		print*, "Interações até primeiro overflow:",firstover
		print*, "Interações aré o primeiro underflow:",firstunder
!		print*, "Ultimos valores para Under com precisão dupla:", dunder
!		print*, "Ultimos valores para Over com precisão dupla:", dover
	end if
else
!print*, "Ultimos valores para Under antes de Underflow com precisão dupla:", dlastunder
!print*, "Ultimos valores para Over antes de Overflow com precisão dupla:", dlastover
print*, "Interações até primeiro overflow com precisão dupla:",dfirstover
print*, "Interações aré o primeiro underflow com precisão dupla:",dfirstunder
print*, "Interações até primeiro overflow:",firstover
print*, "Interações aré o primeiro underflow:",firstunder
end if
end program
