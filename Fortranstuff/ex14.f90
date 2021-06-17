program bigsum
implicit none
real*8 :: Eps1,Eps2,Eps3 
real :: sEps1, sEps2, sEps3
integer :: i, N
N = 1E6
print*, "Precisão Simples:"
do i = 1, N
	if (i == 1) then
		print*, "Cálculo p/ N = 1 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
	if (i == 1E1) then
		print*, "Cálculo p/ N = 1E1 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1E1 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1E1 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
	if (i == 1E2) then
		print*, "Cálculo p/ N = 1E2 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1E2 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1E2 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
	if (i == 1E3) then
		print*, "Cálculo p/ N = 1E3 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1E3 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1E3 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
	if (i == 1E4) then
		print*, "Cálculo p/ N = 1E4 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1E4 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1E4 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
	if (i == 1E5) then
		print*, "Cálculo p/ N = 1E5 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1E5 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1E5 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
	if (i == 1E6) then
		print*, "Cálculo p/ N = 1E6 com a primeira fórmula e precisão simples:", sEps1(i)
		print*, " Cálculo p/ N = 1E6 com a segunda fórmula e precisão simples:", sEps2(i)
		print*, "Cálculo p/ N = 1E6 com a terceira fórmula e precisão simples:", sEps3(i)
		print*,
	end if
end do
print*,
print*, "Precisão Dupla"
do i = 1, N
	if (i == 1) then
		print*, "Cálculo p/ N = 1 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
	if (i == 1E1) then
		print*, "Cálculo p/ N = 1E1 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1E1 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1E1 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
	if (i == 1E2) then
		print*, "Cálculo p/ N = 1E2 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1E2 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1E2 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
	if (i == 1E3) then
		print*, "Cálculo p/ N = 1E3 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1E3 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1E3 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
	if (i == 1E4) then
		print*, "Cálculo p/ N = 1E4 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1E4 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1E4 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
	if (i == 1E5) then
		print*, "Cálculo p/ N = 1E5 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1E5 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1E5 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
	if (i == 1E6) then
		print*, "Cálculo p/ N = 1E6 com a primeira fórmula e precisão dupla:", Eps1(i)
		print*, " Cálculo p/ N = 1E6 com a segunda fórmula e precisão dupla:", Eps2(i)
		print*, "Cálculo p/ N = 1E6 com a terceira fórmula e precisão dupla:", Eps3(i)
		print*,
	end if
end do
end program

real function sEps1(N)
integer, intent(in) :: N
integer :: nm
real :: sE1
sE1 = 0.0
do nm = 1, 2*N
	sE1 = sE1 + ((-1.0)**nm)*nm/(nm + 1.0)
end do
sEps1 = sE1
return
end function
	

real function sEps2(N)
integer, intent(in) :: N
integer :: nm
real :: sE2
sE2 = 0.0
do nm = 1, N
	sE2 = sE2 - (((2.0*nm) - 1.0)/(2.0*nm)) + ((2.0*nm)/((2.0*nm) + 1.0))
end do
sEps2 = sE2
return
end function

real function sEps3(N)
integer, intent(in) :: N
integer :: nm
real :: sE3
sE3 = 0.0
do nm = 1, N
	sE3 = sE3 + (1.0/(2.0*nm*(2.0*nm + 1.0)))
end do
sEps3 = sE3
return
end function

real*8 function Eps1(N)
integer, intent(in) :: N
integer :: nmI
real*8 :: E1
E = 0.d0
do nm = 1, 2*N
	E1 = E1 + ((-1.d0)**nm)*nm/(nm + 1.d0)
end do
Eps1 = E1
return
end function
	

real*8 function Eps2(N)
integer, intent(in) :: N
integer :: nm
real*8 :: E2
E2 = 0.d0
do nm = 1, N
	E2 = E2 - ((2.d0*nm) - 1.d0)/(2.d0*nm) + (2.d0*nm/(2.d0*nm + 1.d0))
end do
Eps2 = E2
return
end function
	

real*8 function Eps3(N)
integer, intent(in) :: N
integer :: nm
real*8 :: E3
E3 = 0.d0
do nm = 1, N
	E3 = E3 + (1.d0/(2.d0*nm*(2.d0*nm + 1.d0)))
end do
Eps3 = E3
return
end function


