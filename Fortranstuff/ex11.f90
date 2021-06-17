PROGRAM areacirculo

IMPLICIT NONE

REAL :: Area, a, b, pi

pi = 4.0*ATAN(1.0) !Usei essa definição de pi pois ela pode gerar valores com precisão dupla, que não é o caso, mas usarei quando for preciso

PRINT*, "Cálculo da Área de um Circulo"
PRINT*, "Raio a:"
READ*, a
PRINT*, "Raio b:"
READ*, b
Area = a*b*pi

PRINT*, "Area:",Area

end program areacirculo
