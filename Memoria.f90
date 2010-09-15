
module NREL_tables
implicit none

integer i

!...Earth Periodic Terms

real(8), dimension(64):: A_L0 =(/175347046.d0,3341656.d0,34894.d0,3497.d0,    &
                        3418.d0,3136.d0,2676.d0,2343.d0,1324.d0,1273.d0,      &
                        1199.d0,990.d0,902.d0,857.d0,780.d0,753.d0,505.d0,    &
                        492.d0,357.d0,317.d0,284.d0,271.d0,243.d0,206.d0,     &
                        205.d0,202.d0,156.d0,132.d0,126.d0,115.d0,103.d0,     &
                        102.d0,102.d0,99.d0,98.d0,86.d0,85.d0,85.d0,80.d0,    &
                        79.d0,75.d0,74.d0,74.d0,70.d0,62.d0,61.d0,57.d0,      &
                        56.d0,56.d0,52.d0,52.d0,51.d0,49.d0,41.d0,41.d0,      &
                        39.d0,37.d0,37.d0,36.d0,36.d0,33.d0,30.d0,30.d0,25.d0/)


real(8), dimension(64):: B_L0 =(/0.d0,4.6692568d0,4.6261d0,2.7441d0,2.8289d0, &
                         3.6277d0,4.4181d0,6.1352d0,0.7425d0,2.0371d0,        &
                         1.1096d0,5.233d0,2.045d0,3.508d0,1.179d0,2.533d0,    &
                         4.583d0,4.205d0,2.92d0,5.849d0,1.899d0,0.315d0,      &
                         0.345d0,4.806d0,1.869d0,2.458d0,0.833d0,3.411d0,     &
                         1.083d0,0.645d0,0.636d0,0.976d0,4.267d0,6.21d0,      &
                         0.68d0,5.98d0,1.3d0,3.67d0,1.81d0,3.04d0,1.76d0,     &
                         3.5d0,4.68d0,0.83d0,3.98d0,1.82d0,2.78d0,4.39d0,     &
                         3.47d0,0.19d0,1.33d0,0.28d0,0.49d0,5.37d0,2.4d0,     & 
                         6.17d0,6.04d0,2.57d0,1.71d0,1.78d0,0.59d0,0.44d0,    &
                         2.74d0,3.16d0/)


real(8), dimension(64):: C_L0 =(/0.d0,6283.07585d0,12566.1517d0,5753.3849d0,  &
                         3.5231d0,77713.7715d0,7860.4194d0, 3930.2097d0,      &
                         11506.7698d0,529.691d0,1577.3435d0,5884.927d0,       &
                         26.298d0,398.149d0,5223.694d0,5507.553d0,            &
                         18849.228d0,775.523d0,0.067d0,11790.629d0,           &
                         796.298d0,10977.079d0,5486.778d0,2544.314d0,         &
                         5573.143d0,6069.777d0,213.299d0,2942.463d0,          &
                         20.775d0,0.98d0,4694.003d0,15720.839d0,7.114d0,      &
                         2146.17d0,155.42d0,161000.69d0,6275.96d0,            &
                         71430.7d0,17260.15d0,12036.46d0,5088.63d0,           &
                         3154.69d0,801.82d0,9437.76d0,8827.39d0,              &
                         7084.9d0,6286.6d0,14143.5d0,6279.55d0,               &
                         12139.55d0,1748.02d0,5856.48d0,1194.45d0,            &
                         8429.24d0,19651.05d0,10447.39d0,10213.29d0,          &
                         1059.38d0,2352.87d0,6812.77d0,17789.85d0,            &
                         83996.85d0,1349.87d0,4690.48d0/),L0i


real(8), dimension(34):: A_L1 =(/628331966747.d0,206059.d0,4303.d0,425.d0,    &
                         119.d0,109.d0,93.d0,72.d0,68.d0,67.d0,59.d0,56.d0,   &
                         45.d0,36.d0,29.d0,21.d0,19.d0,19.d0,17.d0,16.d0,     &
                         16.d0,15.d0,12.d0,12.d0,12.d0,12.d0,11.d0,10.d0,     &
                         10.d0,9.d0,9.d0,8.d0,6.d0,6.d0/)


real(8), dimension(34):: B_L1 =(/0.d0,2.678235d0,2.6351d0,1.59d0,5.796d0,     &
                         2.966d0,2.59d0,1.14d0,1.87d0,4.41d0,2.89d0,2.17d0,   &
                         0.4d0,0.47d0,2.65d0,5.34d0,1.85d0,4.97d0,2.99d0,     &
                         0.03d0,1.43d0,1.21d0,2.83d0,3.26d0,5.27d0,2.08d0,    &
                         0.77d0,1.3d0,4.24d0,2.7d0,5.64d0,5.3d0,2.65d0,4.67d0/)


real(8), dimension(34):: C_L1 =(/0.d0,6283.07585d0,12566.1517d0,3.523d0,      &
                         26.298d0,1577.344d0,18849.23d0,529.69d0,398.15d0,    &
                         5507.55d0,5223.69d0,155.42d0,796.3d0,775.52d0,7.11d0,&
                         0.98d0,5486.78d0,213.3d0,6275.96d0,2544.31d0,        &
                         2146.17d0,10977.08d0,1748.02d0,5088.63d0,1194.45d0,  &
                         4694.d0,553.57d0,6286.6d0,1349.87d0,242.73d0,        &
                         951.72d0,2352.87d0,9437.76d0,4690.48d0/),L1i


real(8), dimension(20):: A_L2 =(/52919.d0,8720.d0,309.d0,27.d0,16.d0,16.d0,   &
                         10.d0,9.d0,7.d0,5.d0,4.d0,4.d0,3.d0,3.d0,3.d0,       &
                         3.d0,3.d0,3.d0,2.d0,2.d0/)


real(8), dimension(20):: B_L2 =(/0.d0,1.0721d0,0.867d0,0.05d0,5.19d0,3.68d0,  &
                         0.76d0,2.06d0,0.83d0,4.66d0,1.03d0,3.44d0,5.14d0,    &
                         6.05d0,1.19d0,6.12d0,0.31d0,2.28d0,4.38d0,3.75d0/)


real(8), dimension(20):: C_L2 =(/0.d0,6283.0758d0,12566.152d0,3.52d0,26.3d0,  &
                         155.42d0,18849.23d0,77713.77d0,775.52d0,1577.34d0,   &
                         7.11d0,5573.14d0,796.3d0,5507.55d0,242.73d0,529.69d0,&
                         398.15d0,553.57d0,5223.69d0,0.98d0/),L2i


real(8), dimension(7):: A_L3 =(/289.d0,35.d0,17.d0,3.d0,1.d0,1.d0,1.d0/)


real(8), dimension(7):: B_L3 =(/5.844d0,0.d0,5.49d0,5.2d0,4.72d0,5.3d0,5.97d0/)

real(8), dimension(7):: C_L3 =(/6283.076d0,0.d0,12566.15d0,155.42d0,3.52d0,   &
                        18849.23d0,242.73d0/),L3i

real(8), dimension(3):: A_L4 =(/114.d0,8.d0,1.d0/)

real(8), dimension(3):: B_L4 =(/3.142d0,4.13d0,3.84d0/)

real(8), dimension(3):: C_L4 =(/0.d0,6283.08d0,12566.15d0/),L4i
! Esta obra está bajo una licencia Reconocimiento-No comercial-Compartir bajo la misma licencia 
! 2.0 Chile de Creative Commons. Para ver una copia de esta licencia, visite 
! http://creativecommons.org/licenses/by-nc-sa/2.0/cl/ o envie una carta a Creative Commons, 
! 171 Second Street, Suite 300, San Francisco, California 94105, USA.


real(8), dimension(5):: A_B0 =(/280.d0,102.d0,80.d0,44.d0,32.d0/)

real(8), dimension(5):: B_B0 =(/3.199d0,5.422d0,3.88d0,3.7d0,4.d0/)

real(8), dimension(5):: C_B0 =(/84334.662d0,5507.553d0,5223.69d0,2352.87d0,   &
                        1577.34d0/),B0i

real(8), dimension(2):: A_B1 =(/9.d0,6.d0/)

real(8), dimension(2):: B_B1 =(/3.9d0,1.73d0/)

real(8), dimension(2):: C_B1 =(/5507.55d0,5223.69d0/),B1i

real(8), dimension(40):: A_R0 =(/100013989.d0,1670700.d0,13956.d0,3084.d0,    &
                         1628.d0,1576.d0,925.d0,542.d0,472.d0,346.d0,329.d0,  &
                         307.d0,243.d0,212.d0,186.d0,175.d0,110.d0,98.d0,     &
                         86.d0,86.d0,65.d0,63.d0,57.d0,56.d0,49.d0,47.d0,     &
                         45.d0,43.d0,39.d0,38.d0,37.d0,37.d0,36.d0,35.d0,     &
                         33.d0,32.d0,32.d0,28.d0,28.d0,26.d0/)

real(8), dimension(40):: B_R0 =(/0.d0,3.0984635d0,3.05525d0,5.1985d0,         &
                         1.1739d0,2.8469d0,5.453d0,4.564d0,3.661d0,0.964d0,   &
                         5.9d0,0.299d0,4.273d0,5.847d0,5.022d0,3.012d0,       &
                         5.055d0,0.89d0,5.69d0,1.27d0,0.27d0,0.92d0,2.01d0,   &
                         5.24d0,3.25d0,2.58d0,5.54d0,6.01d0,5.36d0,2.39d0,    &
                         0.83d0,4.9d0,1.67d0,1.84d0,0.24d0,0.18d0,1.78d0,     &
                         1.21d0,1.9d0,4.59d0/)

real(8), dimension(40):: C_R0 =(/0.d0,6283.07585d0,12566.1517d0,77713.7715d0, &
                         5753.3849d0,7860.4194d0,11506.77d0,3930.21d0,        &
                         5884.927d0,5507.553d0,5223.694d0,5573.143d0,         &
                         11790.629d0,1577.344d0,10977.079d0,18849.228d0,      &
                         5486.778d0,6069.78d0,15720.84d0,161000.69d0,         &
                         17260.15d0,529.69d0,83996.85d0,71430.7d0,            &
                         2544.31d0,775.52d0,9437.76d0,6275.96d0,4694d0,       &
                         8827.39d0,19651.05d0,12139.55d0,12036.46d0,2942.46d0,&
                         7084.9d0,5088.63d0,398.15d0,6286.6d0,6279.55d0,      &
                         10447.39d0/),R0i

real(8), dimension(10):: A_R1 =(/103019.d0,1721.d0,702.d0,32.d0,31.d0,25.d0,  &
                         18.d0,10.d0,9.d0,9.d0/)


real(8), dimension(10):: B_R1 =(/1.10749d0,1.0644d0,3.142d0,1.02d0,2.84d0,    &
                         1.32d0,1.42d0,5.91d0,1.42d0,0.27d0/)

real(8), dimension(10):: C_R1 =(/6283.07585d0,12566.1517d0,0.d0,18849.23d0,   &
                         5507.55d0,5223.69d0,1577.34d0,10977.08d0,6275.96d0,  &
                         5486.78d0/),R1i

real(8), dimension(6):: A_R2 =(/4359.d0,124.d0,12.d0,9.d0,6.d0,3.d0/)

real(8), dimension(6):: B_R2 =(/5.7846d0,5.579d0,3.14d0,3.63d0,1.87d0,5.47d0/)

real(8), dimension(6):: C_R2 =(/6283.0758d0,12566.152d0,0d0,77713.77d0,      &
                        5573.14d0,18849.23d0/),R2i

real(8), dimension(2):: A_R3 =(/145.d0,7.d0/)

real(8), dimension(2):: B_R3 =(/4.273d0,3.92d0/)

real(8), dimension(2):: C_R3 =(/6283.076d0,12566.15d0/),R3i


!...Periodic Terms for the Nutation in Longitude and Obliquity

integer, dimension(315):: Ydata=(/0,0,0,0,1,-2,0,0,2,2,0,0,0,2,2,0,0,0,0,2,0, & 
                          1,0,0,0,0,0,1,0,0,-2,1,0,2,2,0,0,0,2,1,0,0,1,2,2,   &
                          -2,-1,0,2,2,-2,0,1,0,0,-2,0,0,2,1,0,0,-1,2,2,2,0,0, &
                          0,0,0,0,1,0,1,2,0,-1,2,2,0,0,-1,0,1,0,0,1,2,1,-2,   &
                          0,2,0,0,0,0,-2,2,1,2,0,0,2,2,0,0,2,2,2,0,0,2,0,0,   &
                          -2,0,1,2,2,0,0,0,2,0,-2,0,0,2,0,0,0,-1,2,1,0,2,0,   &
                          0,0,2,0,-1,0,1,-2,2,0,2,2,0,1,0,0,1,-2,0,1,0,1,     &
                          0,-1,0,0,1,0,0,2,-2,0,2,0,-1,2,1,2,0,1,2,2,0,1,     &
                          0,2,2,-2,1,1,0,0,0,-1,0,2,2,2,0,0,2,1,2,0,1,0,0,    &
                          -2,0,2,2,2,-2,0,1,2,1,2,0,-2,0,1,2,0,0,0,1,0,-1,    &
                          1,0,0,-2,-1,0,2,1,-2,0,0,0,1,0,0,2,2,1,-2,0,2,0,    &
                          1,-2,1,0,2,1,0,0,1,-2,0,-1,0,1,0,0,-2,1,0,0,0,1,    &
                          0,0,0,0,0,0,1,2,0,0,0,-2,2,2,-1,-1,1,0,0,0,1,1,     &
                          0,0,0,-1,1,2,2,2,-1,-1,2,2,0,0,3,2,2,2,-1,0,2,2/)


real(8), dimension(63):: acoeff =(/-171996.d0,-13187.d0,-2274.d0,2062.d0,     &
                         1426.d0,712.d0,-517.d0,-386.d0,-301.d0,217.d0,       &
                         -158.d0,129.d0,123.d0,63.d0,63.d0,-59.d0,-58.d0,     &
                         -51.d0,48.d0,46.d0,-38.d0,-31.d0,29.d0,29.d0,26.d0,  &
                         -22.d0,21.d0,17.d0,16.d0,-16.d0,-15.d0,-13.d0,-12.d0,&
                         11.d0,-10.d0,-8.d0,7.d0,-7.d0,-7.d0,-7.d0,6.d0,6.d0, &
                         6.d0,-6.d0,-6.d0,5.d0,-5.d0,-5.d0,-5.d0,4.d0,4.d0,   &
                         4.d0,-4.d0,-4.d0,-4.d0,3.d0,-3.d0,-3.d0,-3.d0,-3.d0, &
                         -3.d0,-3.d0,-3.d0/)


real(8), dimension(63):: bcoeff =(/-174.2d0,-1.6d0,-0.2d0,0.2d0,-3.4d0,0.1d0, &
                         1.2d0,-0.4d0,0.d0,-0.5d0,0.d0,0.1d0,0.d0,0.d0,0.1d0, &
                         0.d0,-0.1d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
                         0.d0,0.d0,-0.1d0,0.d0,0.1d0, (0.d0,i=1,33)/)


real(8), dimension(63):: ccoeff =(/92025.d0,5736.d0,977.d0,-895.d0,54.d0,     &
                         -7.d0,224.d0,200.d0,129.d0,-95.d0,0.d0,-70.d0,-53.d0,&
                         0.d0,-33.d0,26.d0,32.d0,27.d0,0.d0,-24.d0,16.d0,     &
                         13.d0,0.d0,-12.d0,0.d0,0.d0,-10.d0,0.d0,-8.d0,7.d0,  &
                         9.d0,7.d0,6.d0,0.d0,5.d0,3.d0,-3.d0,0.d0,3.d0,3.d0,  &
                         0.d0,-3.d0,-3.d0,3.d0,3.d0,0.d0,3.d0,3.d0,3.d0,      &
                         (0.d0,i=1,14)/)


real(8), dimension(63):: dcoeff =(/8.9d0,-3.1d0,-0.5d0,0.5d0,-0.1d0,0.d0,     &
                         -0.6d0,0.d0,-0.1d0,0.3d0,(0.d0,i=1,53)/)

End module NREL_tables


Module location_time

real(8) :: phi,sigma               !! latitude, longitude
real(8) :: elev                    !! elevation
real(8) :: press                   !! annual mean pressure (mbar)
real(8) :: temp                    !! annual mean temperature (celsius)

integer :: tz                      !! time zone
integer :: year,month,day

real(8) :: delta_T                 !! correction for terrestrial time (sec)
real(8) :: time_step               !! time step, in seconds, for the trajectory


!...constants
real(8), parameter :: pi = 3.141592653589793238462643d0
real(8), parameter :: deg_to_rad = pi/180.d0, rad_to_deg = 180.d0/pi

end module location_time


!****************************************************************************    Programa principal
PROGRAM Memoria
IMPLICIT NONE
!USE location_time !Verificar uso de variables duplicadas


INTEGER :: i=0,ano, mes, dia , dom
INTEGER :: hora, dtalog
REAL :: hora_norm, deltaT, deltaTp=0
INTEGER :: ierror, errorread, ierror2
REAL :: radGlo,radDif,radDir,radComp
REAL ::  elevation, azimuth
REAL, EXTERNAL :: norm_hora
LOGICAL :: salida=.FALSE.
real(8), parameter :: pi = 3.141592653589793238462643d0

OPEN (UNIT=8, FILE='datos1.csv', IOSTAT=ierror)      !hacer variable el nombre de archivo de entrada
OPEN (UNIT=10, FILE='salida.txt', IOSTAT=ierror2)   !hacer variable el nombre de archivo de salida

WRITE(*,*) ' Campbell #1, Labview #2'
READ(*,*) dtalog

WRITE(10,*) 'ano  diaJu  hora   radGlo   radDif    radDir   Compuesta  elevation azimuth '
IF(ierror2==0) WRITE(*,*) 'Error al escribir en el archivo de salida.  #',ierror2

IF(ierror==0) THEN   !Si abrio bien el archivo de datos, then.. leer datos
	IF(dtalog==2) THEN
		READ(8,*, IOSTAT=errorread) ano, mes, dom, hora
		hora_norm = norm_hora(hora)
	ENDIF


	DO
    	IF(dtalog==1) READ(8,*, IOSTAT=errorread) ano, dia, hora, radGLo, radDif, radDir
    	IF(dtalog==2) READ(8,*, IOSTAT=errorread) deltaT, radDir, radGLo, radDif 
    	IF ( errorread /= 0 ) THEN
        	WRITE(*,*) 'Error al leer el archivo de datos.  #',errorread  !!Siempre sale mensaje al final, cambiar por IF(salida)exit
    		EXIT
    	ELSE    
        	IF(dtalog==1) hora_norm = norm_hora(hora)
        	IF(dtalog==2) THEN			!sumar dias segun deltaT
        		hora_norm=hora_norm+(deltaT-deltaTp)/3600.
        		WRITE(*,*) hora_norm, deltaT, deltaTp
        		deltaTp=deltaT
        		IF (hora_norm>=24) THEN
        			dom=dom+1
        			hora_norm=hora_norm-24.
        			IF (dom==32 .and. (mes==1 .or. mes==3 .or. mes==5 .or. mes==7 .or. mes==8 .or. mes==10 .or. mes==12)) THEN
        				dom=dom-31
        				mes=mes+1
        			ENDIF
        			IF (dom==31 .and. (mes==4 .or. mes==6 .or. mes==9 .or. mes==11)) THEN
        			dom=dom-30
        			mes=mes+1
        			ENDIF
        			IF (dom==28 .and. (mes==2)) THEN
        			dom=dom-27
        			mes=mes+1
        			ENDIF
        			
        			IF (mes==13) THEN !arreglar para año biciesto.
        				ano=ano+1
        				mes=mes-12
        			ENDIF
        		ENDIF
        	ENDIF
        	
        	
!			Calculo del mes y dia del mes segun dia juliano y año
        	IF(dtalog==1) CALL doy (dia, ano, dom, mes)                  
			IF(dtalog==2) CALL diajuliano (dom, mes, ano, dia)    
        
!Calcular Radiacion Compuesta
        	IF (radGlo>1) THEN
        		!	 			Calculo de angulos del sol !!! e(i), az(i)
        		CALL solar_trajectory(hora_norm*3600. , ano, mes, dom, elevation, azimuth  )
            	radComp=radDif+radDir * (COS((pi/180.d0)*(90.-elevation))) 
            	WRITE(*,*) hora_norm, ano, mes, dom, elevation, azimuth 
        	ELSE 
            	radComp=0
            	elevation=0
            	azimuth=0
        	END IF
                
! Impresion de datos en ‘salida.txt’                
        WRITE(10,*)ano, dia, hora_norm, radGlo, radDif,radDir,radComp,elevation,azimuth
!1000    FORMAT (I4,'  ', I2,'  ', I2,'  ', F5.2,'  ', F7.2,'  ', F7.2,'  ', F7.2,'  ', F7.2,'  ', F7.2,'  ', F7.2)
    	ENDIF
	ENDDO

ELSE	!Si no abrio bien el archivo de datos, then.. 
    WRITE(*,*) 'Error al abrir el archivo de datos.  #',ierror
    
ENDIF

IF ( ierror2 /= 0 ) WRITE (*,*) 'Error al tratar de escribir el archivo de salida.  #',ierror2


END PROGRAM

!*******************************************************    Funciones y Subrutinas


FUNCTION norm_hora(value) ! Revisar esta funcion 
! Cambio de hora a forma decimal
IMPLICIT NONE

INTEGER, INTENT(IN) :: value 
REAL :: norm_hora 
CHARACTER(len=4) :: string
CHARACTER(len=2) :: hh, mm 
INTEGER :: h, m

string = ' '

WRITE (string,'(I4)') value
hh=string(1:2)
mm=string(3:4)
READ (hh,'(I2)') h
READ (mm,'(I2)') m

norm_hora = REAL(h)+(REAL(m)/60.)
END FUNCTION norm_hora



SUBROUTINE doy (day_of_year, year, day, month)   !Cambiar nombre de subrutina y comentario
! Calculo del mes y dia del mes segun dia del año
!Purpose:    Pagina 141
!This program calculates the date corresponding to a specified.

IMPLICIT NONE
! Data dictionary: declare variable types, definitions, & units
INTEGER, INTENT(IN) :: day_of_year  !Day of year
INTEGER, INTENT(IN) :: year         !Year (yyyy)
INTEGER, INTENT(OUT) :: month        !Month (mm)
INTEGER, INTENT(OUT) :: day          !Day (dd)
INTEGER :: i            !Index,variable
INTEGER :: leap_day     !Extra day for leap year


! Check for leap year, and add extra day if necessary
IF ( MOD(year,400) == 0 ) THEN
    leap_day = 1    ! Years divisible by 400 are leap years
ELSE IF ( MOD(year,100) == 0 ) THEN
    leap_day = 0    ! Other centuries are not leap years
ELSE IF ( MOD(year,4) == 0 ) THEN
    leap_day = 1    ! Otherwise every 4th year 1S a leap year
ELSE
    leap_day = 0    ! Other years are not leap years
END IF

day=day_of_year
! Calculate day of year
moy: DO i = 1, 12
    ! Add days in months from January to last month
    SELECT CASE (i)
    CASE (1,3,5,7,8,10,12)
    IF (day<=31) THEN
        month=i
        EXIT moy
    END IF
    day = day - 31
    CASE (4,6,9,11)
        IF (day <=30) THEN
        month=i
        EXIT moy
    END IF
    day= day - 30
    CASE (2)
        IF (day <=(28+leap_day)) THEN
        month=i
        EXIT moy
    END IF
    day = day - 28 - leap_day
    END SELECT
END DO moy

END SUBROUTINE doy


SUBROUTINE diajuliano (day, month, year, dayj)   
!Purpose:    Pagina 141
!This program calculates the day of year corresponding to a
!specified date. It illustrates the use of counting loops
!and the SELECT CASE construct.

IMPLICIT NONE
! Data dictionary: declare variable types, definitions, & units
INTEGER, INTENT(IN):: day          !Day (dd)
INTEGER, INTENT(IN) :: month        !Month (mm)
INTEGER, INTENT(IN) :: year         !Year (yyyy)
INTEGER, INTENT(out) :: dayj 		!Day of year
INTEGER :: i            			!Index,variable
INTEGER :: leap_day     			!Extra day for leap year

! Check for leap year, and add extra day if necessary
IF ( MOD(year,400) == 0 ) THEN
    leap_day = 1    ! Years divisible by 400 are leap years
ELSE IF ( MOD(year,100) == 0 ) THEN
    leap_day = 0    ! Other centuries are not leap years
ELSE IF ( MOD(year,4) == 0 ) THEN
    leap_day = 1    ! Otherwise every 4th year 1S a leap year
ELSE
    leap_day = 0    ! Other years are not leap years
END IF


! Calculate day of year
dayj= day
DO i = 1, month-1
    ! Add days in months from January to last month
    SELECT CASE (i)
    CASE (1,3,5,7,8,10,12)
    dayj = dayj + 31
    CASE (4,6,9,11)
    dayj = dayj + 30
    CASE (2)
    dayj = dayj + 28 + leap_day
    END SELECT
END DO

END SUBROUTINE diajuliano



!****************************************************************************  / Programa principal

SUBROUTINE solar_trajectory(time, yt, mt, dt , elevation, azimuth )
use location_time
use NREL_tables
implicit none
REAL, INTENT(IN)  :: time
INTEGER, INTENT(IN)  :: yt, mt, dt
REAL, INTENT(OUT) :: elevation, azimuth  
integer, dimension(3) :: sunrise,sunset,transit
real(8) jd,jde,jc,jce,jme,beta,Dpsi,eps,lambda,R,nu,alpha,delta,e,Phi_az
real(8) sidereal_time,times,timee,hh_dec!, time
integer hh_UT,mm_UT,ss_UT,hh_lst,mm_lst,ss_lst,n,n_steps

namelist/input/tz,phi,sigma,elev,press,Temp,delta_T
!--------------------------------------------------------------------------------

open(unit=1,file='solar.dat')
read(1,input)
close(1)

phi = phi * deg_to_rad
year = yt
month = mt
day = dt

   hh_dec = time/3600.d0
   hh_lst = int(hh_dec) 
   mm_lst = int((hh_dec - hh_lst)*60.d0)
   ss_lst = time - 3600.d0*hh_lst - 60.d0*mm_lst
   if (ss_lst == 60) then
     mm_lst = mm_lst + 1
     ss_lst = 0
   end if

   if (mm_lst == 60) then
     hh_lst = hh_lst + 1
     mm_lst = 0
   end if
  

   call get_UT_time(hh_lst,mm_lst,ss_lst,hh_UT,mm_UT,ss_UT)

   call julian_date(hh_UT,mm_UT,ss_UT,jd,jc)

   call julian_ephemeris(jd,jde,jce,jme)

   call heliocentric_geocentric(jd,jde,jc,jce,jme,eps,beta,lambda,Dpsi,R)


   !...apparent sidereal time at Greenwich (degrees)
   nu = sidereal_time(jd,jc,eps,Dpsi)

   !...geocentric sun right ascension and declination (degrees)
   call geocentric_RA_declination(lambda,eps,beta,alpha,delta)

   call elevation_azimuth(nu,alpha,delta,R,e,Phi_az)

   
 elevation = e
 azimuth= Phi_az

END


subroutine elevation_azimuth(nu,alpha,delta,R,e,Phi_az)
use location_time
use NREL_tables
implicit none
real(8) nu,alpha,delta,R,e,Phi_az
!-----------------------------------------------------------------
real(8) xi,xterm,yterm,H,Hp,Dalpha,deltap,alphap,e0,De,sinphi,cosphi,u

delta = delta * deg_to_rad
sinphi = sin(phi)
cosphi = cos(phi)

!...observer local hour (degrees)
H = nu + sigma - alpha
call range0_360(H) 
H = H * deg_to_rad

!...topocentric sun right ascension (degrees)
xi = 8.794d0/(3600.d0*R)
xi = xi * deg_to_rad

u = atan(0.99664719d0*tan(phi))

xterm = cos(u) + elev/6378140.d0 * cosphi

yterm = 0.99664719d0*sin(u) + elev/6378140.d0 * sinphi

Dalpha = atan2(-xterm*sin(xi)*sin(H) , cos(delta) - xterm*sin(xi)*cos(H))

alphap = alpha + Dalpha*rad_to_deg

!...topocentric sun declination 
deltap = atan2((sin(delta) - yterm*sin(xi))*cos(Dalpha),        &
                cos(delta) - xterm*sin(xi)*cos(H))

!...topocentric local hour angle
Hp = H - Dalpha

!...topocentric elevation angle without atmpospheric refraction correction
e0 = asin(sinphi*sin(deltap) + cosphi*cos(deltap)*cos(Hp))
e0 = e0 * rad_to_deg

!...atmospheric refaction correction (degrees)
u = (e0 + 10.3d0/(e0 + 5.11d0))*deg_to_rad
De = press/1010.d0 * 283.d0/(273.d0 + Temp) * 1.02d0/(60.d0*tan(u))

!...topocentric elevation angle (degrees)
e = e0 + De

!...topocentric azimuth angle (degrees)
Phi_az = atan2(sin(Hp) , cos(Hp)*sinphi - tan(deltap)*cosphi)
Phi_az = Phi_az * rad_to_deg + 180.d0
call range0_360(Phi_az)
End subroutine


subroutine heliocentric_geocentric(jd,jde,jc,jce,jme,eps,beta,lambda,Dpsi,R)
use location_time
use NREL_tables
implicit none
real(8) jd,jde,jc,jce,jme,eps,beta,lambda,Dpsi,R
!--------------------------------------------------------------------
real(8), dimension(63):: arg,delta_psi,delta_epsilon
integer, dimension(5,63) :: Y

real(8) L0,L1,L2,L3,L4,L5,L,B0,B1,B,R0,R1,R2,R3,R4
real(8) theta,x0,x1,x2,x3,x4,Depsilon,u,eps0,Dtau

Y = reshape(source=Ydata,shape=(/5,63/))


!...Earth heliocentric longitude (degrees)
L0i(:) = A_L0(:)*cos(B_L0(:) + C_L0(:)*jme)
L0 = sum(L0i)

L1i(:) = A_L1(:)*cos(B_L1(:) + C_L1(:)*jme)
L1 = sum(L1i)

L2i(:) = A_L2(:)*cos(B_L2(:) + C_L2(:)*jme)
L2 = sum(L2i)

L3i(:) = A_L3(:)*cos(B_L3(:) + C_L3(:)*jme)
L3 = sum(L3i)

L4i(:) = A_L4(:)*cos(B_L4(:) + C_L4(:)*jme)
L4 = sum(L4i)

L5 = cos(3.14d0)

L = (L0 + jme*(L1 + jme*(L2 + jme*(L3 + jme*(L4 + jme*L5)))))/1.d8

L = L * rad_to_deg

call range0_360(L) 

!...Earth heliocentric latitude (degrees)
B0i(:) = A_B0(:)*cos(B_B0(:) + C_B0(:)*jme)
B0 = sum(B0i)

B1i(:) = A_B1(:)*cos(B_B1(:) + C_B1(:)*jme)
B1 = sum(B1i)

B = (B0 + jme*B1)/1.d8
B = B * rad_to_deg


!...Earth heliocentric radius vector (AU)
R0i(:) = A_R0(:)*cos(B_R0(:) + C_R0(:)*jme)
R0 = sum(R0i)

R1i(:) = A_R1(:)*cos(B_R1(:) + C_R1(:)*jme)
R1 = sum(R1i)

R2i(:) = A_R2(:)*cos(B_R2(:) + C_R2(:)*jme)
R2 = sum(R2i)

R3i(:) = A_R3(:)*cos(B_R3(:) + C_R3(:)*jme)
R3 = sum(R3i)

R4 = 4.d0*cos(2.56d0 + 6283.08*jme)

R = (R0 + jme*(R1 + jme*(R2 + jme*(R3 + jme*R4))))/1.d8


!...geocentric longitude
theta = L + 180.d0
call range0_360(theta) 

!...geocentric latitude
beta = -B

!...mean elongation of the moon from the sun (degrees)
x0 = 297.85036d0 + jce*(445267.111480d0 - jce*(0.0019142d0 - jce/189474.d0))

!...mean anomaly of the sun (Earth) (degrees)
x1 = 357.52772d0 + jce*(35999.050340d0 - jce*(0.0001603d0 + jce/300000.d0))

!...mean anomaly of the moon (degrees)
x2 = 134.96298d0 + jce*(477198.867398d0 + jce*(0.0086972d0 + jce/56250.d0))

!...moon's argument of latitude (degrees)
x3 = 93.27191d0 + jce*(483202.017538d0 - jce*(0.0036825d0 - jce/327270.d0))

!...longitude of ascending node of moon's mean orbit on the ecliptic, 
!...measured from the mean equinox of the date (degrees)
x4 = 125.04452d0 + jce*(-1934.136261d0 + jce*(0.0020708d0 + jce/450000.d0))


arg(:) = x0*Y(1,:) + x1*Y(2,:) + x2*Y(3,:) + x3*Y(4,:) + x4*Y(5,:)
arg = arg * deg_to_rad

delta_psi(:) = (acoeff(:) + bcoeff(:)*jce) * sin(arg(:))

delta_epsilon(:) = (ccoeff(:) + dcoeff(:)*jce) * cos(arg(:))

!...nutation in longitude (degrees)
Dpsi = sum(delta_psi)/36000000.d0

!...nutation in obliquity (degrees)
Depsilon = sum(delta_epsilon)/36000000.d0


!...mean obliquity of the ecliptic (in arc seconds)
u = jme/10.d0

eps0 = (((((((((2.45d0*u + 5.79d0)*u + 27.87d0)*u + 7.12d0)*u - 39.05d0)*u    &
     - 249.67d0)*u + 51.38d0)*u + 1999.25d0)*u - 1.55d0)*u - 4680.93d0)*u     &
     + 84381.448d0

!...obliquity of the ecliptic (degrees)
eps = eps0/3600.d0 + Depsilon

!...aberration correction (degrees)
Dtau = -20.4898d0/(3600.d0*R)

!...apparent sun longitude (degrees)
lambda = theta + Dpsi + Dtau

eps = eps * deg_to_rad
beta = beta * deg_to_rad
lambda = lambda * deg_to_rad

End subroutine


real(8) function sidereal_time(jd,jc,eps,Dpsi)
implicit none
real(8) jd,jc,eps,Dpsi
!--------------------------------------------------------
real(8) nu0
!...mean sidereal time at Greenwich (degrees)
nu0 = 280.46061837d0 + 360.98564736629d0*(jd - 2451545.d0) + jc*jc*(3.87933d-4 &
    - jc/38710000.d0)

call range0_360(nu0) 
!...apparent sidereal time at Greenwich (degrees)
sidereal_time = nu0 + Dpsi*cos(eps)
End function


subroutine geocentric_RA_declination(lambda,eps,beta,alpha,delta)
use location_time
implicit none
real(8) lambda,eps,beta,alpha,delta
!-----------------------------------------------------------------
!...geocentric sun right ascension (degrees)
alpha = atan2(sin(lambda)*cos(eps) - tan(beta)*sin(eps) , cos(lambda))

alpha = alpha * rad_to_deg
call range0_360(alpha) 

!...geocentric sun declination 
delta = asin(sin(beta)*cos(eps) + cos(beta)*sin(eps)*sin(lambda))
delta = delta * rad_to_deg
end subroutine


subroutine range0_360(t)
real(8) t
if (t > 360.d0) then
   t = 360.d0*(t/360.d0 - int(t/360.d0))
else if (t < 0) then
   t = 360.d0 - 360.d0*abs(t/360.d0 - int(t/360.d0))
end if
end subroutine


subroutine range0_180(t)
real(8) t
if (t > 180.d0) then
   t = 180.d0*(t/180.d0 - int(t/180.d0))
else if (t < 0) then
   t = 180.d0 - 180.d0*abs(t/180.d0 - int(t/180.d0))
end if
end subroutine


subroutine range0_1(t)
real(8) t
if (t > 1.d0) then
   t = t - int(t)
else if (t < 0) then
   t = 1.d0 - abs(t - int(t))
end if
end subroutine


subroutine rangem180_180(t)
real(8) t
if (t > 360.d0) then
   t = 360.d0*(t/360.d0 - int(t/360.d0))
else if (t < -360.d0) then
   t = -360.d0*abs(t/360.d0 - int(t/360.d0))
end if

if (t <= -180.d0) then
  t = t + 360.d0
else if (t >= 180.d0) then
  t = t -360.d0
end if
end subroutine


subroutine julian_date(hh_UT,mm_UT,ss_UT,jd,jc)
use location_time
implicit none
integer hh_UT,mm_UT,ss_UT
real(8) jd,jc
!------------------------------------------------------------------------
real(8) dday
integer a

if (month <= 2) then
  year = year - 1
  month = month + 12
end if

dday = day + (hh_UT + mm_UT/60.d0 + ss_UT/3600.d0)/24.d0

jd = int(365.25*(year + 4716)) + int(30.6001*(month + 1)) + dday - 1524.5d0

if (jd >= 2299160.d0) then
   a = int(year/100.d0)  
   jd = jd + 2 - a + int(a/4.d0)
end if

!...Julian century
jc = (jd - 2451545.d0)/36525.d0
end subroutine


subroutine julian_ephemeris(jd,jde,jce,jme)
use location_time
implicit none
real(8) jd,jde,jce,jme
!-----------------------------------------------------
!...Julian ephemeris day
jde = jd + delta_T/86400.d0

!...Julian ephemeris century for the 2000 standard epoch
jce = (jde - 2451545.d0)/36525.d0

!...Julian ephemeris millenium for the 2000 standard epoch
jme = jce/10.d0
end subroutine


subroutine get_UT_time(hh_lst,mm_lst,ss_lst,hh_UT,mm_UT,ss_UT)
use location_time
implicit none
integer hh_lst,mm_lst,ss_lst,hh_UT,mm_UT,ss_UT

hh_UT = hh_lst - tz
mm_UT = mm_lst 
ss_UT = ss_lst

end subroutine
 
