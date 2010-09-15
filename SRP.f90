
!/////////////////////////////////////////////
!		Solar Radiation Processor (SRP)
!				for
!		Solar Energy Application
!
!
!   Juan Pablo Justiniano
!   jpjustiniano@gmail.com
!
!   Universidad Tecnico Federico Santa Maria
!   Department of Mechanical Engineering 
!   Valparaiso, Chile.
!   
!		September 09, 2010 
!
!/////////////////////////////////////////////


! Copyright (C) 2010 Juan Pablo Justiniano
!
!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!1. Redistributions of source code must retain the above copyright
!   notice, this list of conditions and the following disclaimer.
!2. Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
!3. All advertising materials mentioning features or use of this software
!   must display the following acknowledgement:
!   This product includes software developed by Juan Pablo Justiniano.
!4. Neither the name of Juan Pablo Justiniano nor the
!   names of its contributors may be used to endorse or promote products
!   derived from this software without specific prior written permission.
!
!THIS SOFTWARE IS PROVIDED BY JUAN PABLO JUSTINIANO ''AS IS'' AND ANY
!EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!DISCLAIMED. IN NO EVENT SHALL JUAN PABLO JUSTINIANO BE LIABLE FOR ANY
!DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



!************************************************************************************  Main   ******************************

Program SRP

!USE DISLIN
implicit none
real(8) :: phi,sigma               !! latitude, longitude
real(8) :: elev                    !! elevation
real(8) :: press                   !! annual mean pressure (mbar)
real(8) :: temp                    !! annual mean temperature (celsius)
real    :: tz                      !! time zone
real(8) :: delta_T                 !! correction for terrestrial time (sec)

!Propias (cambiar al programa pricipal)
INTEGER :: vs					! visualizacion en pantalla (0;1)
INTEGER :: tm					! metodo de seguimiento solar
INTEGER :: sm					! modelo de radiacion difusa
INTEGER :: Indata				! Input data

!...constants
real(8), parameter :: pi = 3.141592653589793238462643d0
real(8), parameter :: deg_to_rad = pi/180.d0, rad_to_deg = 180.d0/pi

integer, dimension(3) :: sunrise,sunset,transit
real(8) jd,jde,jc,jce,jme,beta,Dpsi,eps,lambda,R,nu,alpha,delta,e,Phi_az
real(8) sidereal_time,times,timee
integer hh_UT,mm_UT,ss_UT,hh_lst,mm_lst,ss_lst
real	:: hora, deltaT, deltaTp=0.d0
integer :: hh,mm,ss
INTEGER :: dtalog				! dtalog: formato de datos (Campbell o Labview)
INTEGER :: horac				! hora en formato campbell (hhmm)
REAL(8) :: slope				! pendiente de la superficie
REAL(8) :: OMEGA				! Hour angle
REAL(8) :: gamma				! Surface azimuth angle; South=0; East(-); West(+)
REAL(8) :: costheta, costhetaz, thetaz  ! Angle of incidence, zenith angle
REAL(8) :: r0, Rb
INTEGER :: ierror, errorread, ierror2
INTEGER	:: year,month,day, diaj	! Leidos del archivo de datos
REAL 	:: anno, diaju
integer:: yearj,monthj,dayj		! Usados para los calculos
REAL(8) :: radGlo,radDif,radDir,radComp, radDirHor, radExt, radExtHor
REAL, EXTERNAL :: norm_hora
REAL(8) :: Ai, ff, It, rho, rs, F
Real :: Kt, Kd, Kn
INTEGER :: QC	
INTEGER :: Graph			! Graph data
CHARACTER(len=60) :: argument
CHARACTER(len=4) :: xxx
Character(len=12) :: version ='0.1 beta'
Real :: ha,dec,soldst


namelist/input/tz,phi,sigma,elev,press,Temp,delta_T,vs,tm, sm, Indata, QC, Graph
!--------------------------------------------------------------------------------

call get_command_argument(1, argument)

open(unit=1,file='solar.dat')
read(1,input)       !lee los datos de localizacion
close(1)

write (*,*)
Write (*,*) ' Solar Radiation Processor, ', version
write (xxx,*) argument(LEN_TRIM(argument) - 2 : LEN_TRIM(argument))
Select case (xxx)	!Selector de formato de archivo
Case (' txt', ' TXT')
	write(*,*) ' Texto'
!dtalog==1 
Case (' tm2',' TM2')
	write(*,*) ' TMY2'
!dtalog==2
Case (' tm3',' TM3')
	write(*,*) ' TMY3'
!dtalog==3
Case (' csv',' CSV')
	write(*,*) ' CSV'
!dtalog==4
Case default
	write(*,*) ' File: datos.csv'
  argument ='datos.csv'
  
end select

SELECT CASE (tm) 	! Tracking method 
Case (0)
	write(*,*) ' Tracking method: Disabled '
Case (1)
	write(*,*) ' Tracking method: " Tilted surface, fixed "' 
	WRITE(*,*)' Surface azimuth angle (0-360.):             North=0; '
	READ (*,*) gamma
	gamma = gamma * deg_to_rad
	WRITE(*,*)' Surface slope (0-90.):              '
	READ (*,*) slope
	slope = slope * deg_to_rad	
Case (2)
	write(*,*) ' Tracking method: Horizontal east-west axis, single daily adjustment'
Case (3)
	write(*,*) ' Tracking method: Horizontal east-west axis, continous adjustment '	
Case (4)
	write(*,*) ' Tracking method: Horizontal north-south axis, continous adjustment '		
Case (5)
	write(*,*) ' Tracking method: Fixed slope rotated about a vertical axis '	
	WRITE(*,*) ' Surface slope (0-90.):              '
	READ (*,*) slope   	
	slope = slope * deg_to_rad
Case (6)
	write(*,*) ' Tracking method: Continuously tracking about two axes '
CASE DEFAULT
	write(*,*) ' Error in Tracking method number '
	GOTO 999
END SELECT
	
SELECT CASE (sm) 	! Radiation model
Case (0)
	write(*,*) ' Radiation model: Isotropic'
	write(*,*) 
Case (1)	
	write(*,*) ' Radiation model: Isotropic diffuse'
	WRITE(*,*) ' Diffuse reflectance (0-1.):    '
	READ (*,*) rho	
	write(*,*) 	
Case (2) 
	write(*,*) ' Radiation model: Hay and Davis Isotropic'
	WRITE(*,*) ' Ground reflectance (0-1.):    '
	READ (*,*) rho
	write(*,*) 
Case (3) 
	write(*,*) ' Radiation model: Reindl et al. Isotropic'
	WRITE(*,*) ' Ground reflectance (0-1.):    '
	READ (*,*) rho		
	write(*,*) 
CASE DEFAULT
	write(*,*) ' Error in Radiation on Sloped surface model number '
	write(*,*)
	GOTO 999
END SELECT

If (Graph == 1) Then! Si Graficar!
		
!!Level 0
!	CALL METAFL ('PDF')!Formato de salida
!	Call SETFIL ('Kd_Kt')

!    CALL DISINI !Inicio de DISLIN, Level 0->1
!!Level 1
!	CALL PSFONT ('Times-Bold')
!	CALL HSYMBL (7)  !Defines the size of symbols
!	CALL MARKER (21)  !Defines the  symbols
!	CALL PAGERA !Imprime los bordes de pagina
!    CALL AXSPOS(450,1800) !Posicion esquina sup-izq
!    CALL AXSLEN(2200,1200) !Largo grafico
!    CALL NAME('Kt','X') !Titulo eje
!    CALL NAME('Kd','Y')
!	CALL LABDIG(-1,'X') !cruce ejes
!	CALL TICKS(1,'XY') !Ticks ejes
!	CALL TITLIN('Diffuse index vs Clearness index',3) !Titulo 
!	!CALL DUPLX !Formato fuente
!	CALL LABDIG (1, 'xyz')
!	CALL AX2GRF

!	!CALL QPLSCA (x, y, n) ! Scatter Plot
!	CALL HEIGHT (35)
!	CALL GRAF (0.0,1.0,0.0,0.2,0.0,1.0,0.0,0.2) !Def. Graf., Level 1->2
!!Level 2
!	CALL COLOR('Fore') !Retorna al color por defecto
!	CALL HEIGHT (55)
!	CALL TITLE
!	CALL COLOR ('RED')
EndIf

!WRITE(*,*) ' Campbell #1, TM2 #2, TM3 #3' ! Indica el formato de entrada de datos
!READ(*,*) dtalog
dtalog =1 

OPEN (UNIT=8, FILE=argument, IOSTAT=errorread)  

IF(errorread==0) THEN   !Si abrio bien el archivo de datos, then.. leer datos
	
	!IF(dtalog==2) call TMY23head(tz,phi,sigma,elev)
	
	OPEN (UNIT=21, FILE='salida.txt', IOSTAT=ierror2)  
	IF(ierror2/=0) WRITE(*,*) 'Error al escribir en el archivo de salida.  #',ierror2
	IF (tm == 0) WRITE(21,*) 'ano; diaJ; (hh:mm); radGlo; radDif; radDir; Comp.; elev.; azimuth'
	IF (tm /= 0) WRITE(21,*) 'ano; diaJ; (hh:mm); radGlo; radDif; radDir; Comp.; elev.; azimuth; It; β; kt; kd; kn'
	IF(vs==1 .and. tm == 0) WRITE(*,*)'ano  diaJ (hh:mm)   radGlo   radDif  radDir   Comp.   elev.  azimuth '
	IF(vs==1 .and. tm /= 0) WRITE(*,*)'ano  diaJ (hh:mm)   radGlo   radDif  radDir   Comp.   elev.  &
	                                    & azimuth     It     β'
	
	DO  ! Inicio de ciclo de lectura de datos de radiacion
		IF(dtalog==1) Then ! Lectura de datos segun variables de entrada
		SELECT CASE (Indata) 
		CASE (0)  !Rad. Global(h), Diffuse(h) and Direct(n)
			READ(8,*, IOSTAT=errorread) year, diaj, horac, radGLo, radDif, radDir
    	CASE (1)  !Rad. Global(h)
			READ(8,*, IOSTAT=errorread) year, diaj, horac, radGLo
    	CASE (2)  !Rad. Global(h) and Diffuse(h)
			READ(8,*, IOSTAT=errorread) year, diaj, horac, radGLo, radDif
    	CASE (3)  !Rad. Global(h) and Direct(n)
			READ(8,*, IOSTAT=errorread) year, diaj, horac, radGLo,radDir
        CASE DEFAULT
        write(*,*) "  Error in input data number. Check it out!!"
        Exit	
        END SELECT
        End If 		 		 		    	
    	
    	If (radGLo <= 0.1 ) radGLo = 0.
    	If (radDif <= 0.1 ) radDif = 0.
    	If (radDir <= 0.1 ) radDir = 0.
    	
    	IF ( errorread == -1 ) THEN  ! ==-1
    		WRITE(*,*) '  Terminada la lectura del archivo de datos.'  
    		EXIT
    	ElseIf ( errorread > 0 ) Then
			WRITE(*,*) '  Error en la lectura del archivo de datos.!'  
    		EXIT 
    	ELSE    ! lectura de nueva linea de datos sin problema
        	IF(dtalog==1) hora = norm_hora(horac)
        	
!			Calculo del mes y dia del mes segun dia juliano y año o viceversa
        	IF(dtalog==1) CALL doy (diaj, year, day, month)
        	
        	hora = (tz*3600.+hora)/3600.
        	anno = float(year); diaju=float(diaj)
        	
        	WRITE(*,*) year,diaj,hora,phi,sigma
			call sunae(anno,diaju,hora,phi,sigma,Phi_az,e,ha,dec,soldst)
			WRITE(*,*) Phi_az,e,ha,dec,soldst
        	IF (e > 5. .and. radGLo > 0.) THEN
        	
            	radExt = 1367.*(1+0.033*cos(360.*diaj/365.))   ! Radiacion Extraterrestre
            	radExtHor = radExt *cos(pi/2.- e*deg_to_rad)
            	kt = radGLo/radExtHor		! Clearness index (kt) or global horizontal transmittance
				
				SELECT CASE (Indata)
					CASE (0)  !Rad. Global(h), Diffuse(h) and Direct(n)  !radGLo, radDif, radDir
						radDirHor=radDir*cos(pi/2.- e*deg_to_rad)
					CASE (1)  !Rad. Global(h)
						!Muneer et al. (1996)
						radDif = radGLo * (1.006-0.317*Kt+3.1241*Kt**2-12.7616*Kt**3+9.7166*Kt**4)
						
						!Erbs et al. (1982)
						!IF (Kt .LE. 0.22 ) radDif = radGLo * ( 1.0 - 0.09 * Kt)
						!IF (Kt .GT. 0.22 .and.  Kt .LE. 0.80) radDif = radGLo * (0.9511-0.1604*Kt+4.388*Kt**2-16.638*Kt**3+12.336*Kt**4)
						!IF (Kt .GT. 0.80 ) radDif = radGLo * (0.165)	
						radDirHor= radGLo + radDif
						radDir = radDirHor/cos(pi/2.- e*deg_to_rad)	
					CASE (2)  !Rad. Global(h) and Diffuse(h)
						radDirHor= radGLo + radDif
						radDir = radDirHor/cos(pi/2.- e*deg_to_rad)
					CASE (3)  !Rad. Global(h) and Direct(n)
						radDirHor=radDir*cos(pi/2.- e*deg_to_rad)
						radDif = radGLo - radDirHor
					CASE DEFAULT
					write (*,*) 'Error en numero de datos de entrada (Indata)'
					Exit	
				END SELECT
                    	 	
            	
            	radComp=radDif+radDir * (cos(deg_to_rad * ( 90. - e))) !Calculo de Radiacion Compuesta
            	
            	radExtHor = radExt *Sin(e*deg_to_rad)
            	kd = radDif / radExtHor		! Diffuse horizontal transmittance
            	Kn = radDir / radExt		! Direct normal transmittance
            	
            	!F = (radGLo -radDif)/radExtHor	! clearness function
            	!m = (sin(e*deg_to_rad)+0.50572*(e*deg_to_rad+6.07995)**-1.6364)**-1.	! Air mass (Kasten (1993))
            	!mp = m * (press/1013.25)		! Air mass ajustado segun presion atmosferica
!            	KKd = 1.0 + 0.03344 * cos(diaj/365.25-2.80) !  eccentric earth orbit correction factor
            	
				!If (abs(kt)<1. .and. abs(kd)<1. .and. Graph == 1) CALL RLSYMB (21, Kt, Kd)      	
            	
            	!IBn =  (Muneer,Solar Radiation and Daylight Models, 84) 
						!( Quality control of solar radiation data: Present status andproposednew approaches, Muneer, 1537)
						!					
            	!Quality control of solar radiation data 
!            	IF (QC==1 .and. e>=7) Then 		
					
!					If ( kt>=0 .and. kt<=1 .and. kd>=0 .and. kd<=1) Then
						
						
!					Else
!						!corregir datos si indice es fuera de lo normal...
!					End If
            	
!            	End If
            	
            	
            	! Tracking method
            	IF (tm /= 0) then
            		
            		r0= 2*pi * (diaj-1)/365
            		!omega= (hora - (transit(1)*3600.d0+transit(2)*60.d0+transit(3)*1.d0))/3600.d0 * 15. !Ya calculado ??
            		!omega = omega * deg_to_rad
            		ha = ha * deg_to_rad
            		
            		If (e > 0.) then
            			thetaz= pi/2.- e*deg_to_rad
            			costhetaz = cos(thetaz)
            		else
            			thetaz=pi/2.
            			costhetaz=0.
            		endif

            		SELECT CASE (tm) ! salidas de slope  y gamma en radianes.
    				CASE (1)  ! Tilted surface, fixed.
    				costheta = costhetaz*cos(slope)+sin(thetaz)*sin(slope)*cos(Phi_az*deg_to_rad-gamma)
    				
    				CASE (2) ! Plane rotated about a horizontal east-weas axis, Daily adjustment
    				costheta = (sin(delta))**2 + (cos(delta))**2 * cos(ha)
    				slope = abs(phi * deg_to_rad - delta)
    				gamma = 0. ! hemisferio Sur
    				
    				CASE (3) ! Plane rotated about a horizontal east-weas axis, continuous adjustment
            		costheta=(1- (cos(delta))**2 * (sin(ha))**2 ) **0.5
            		slope = atan( tan(thetaz) * abs(cos(Phi_az*deg_to_rad)))
            		gamma=0.  ! hemisferio Sur
            	
            		CASE (4) ! Plane rotated about a horizontal north-south axis, continuous adjustment
            		costheta=(costhetaz**2 + (cos(delta))**2 * (sin(ha))**2 ) **0.5
            		IF (Phi_az < 180. .and. Phi_az >= 0.) gamma=pi/2
            		IF (Phi_az <= 360. .and. Phi_az >= 180.) gamma=4*pi/3
            		slope= atan(tan(thetaz)*abs(cos(gamma-Phi_az*deg_to_rad)))     
            	
            		CASE (5) ! plane with fixed slope rotated about a vertical axis
            		costheta = costhetaz*cos(slope) + sin(thetaz)*sin(slope)
            		gamma=phi_az * deg_to_rad
            		
            		CASE (6) ! Plane that is continuously tracking about two axes
            		costheta = 1
            		slope = (90. - e)* deg_to_rad
            		gamma = Phi_az * deg_to_rad
            		
            		CASE DEFAULT
            		write(*,*) "  Error in tracking metod number. Check it out!!"
            		Exit	
            		END SELECT
            	
            		!Rb: Indice de aumento de radiación directa en p. elevado sobre p. horizontal
            		Rb = costheta  ! Debido a que la rad directa es de phireliometro.(Normal) 
            		! Rb = costheta / costhetaz en caso de rad. directa horizontal.
            		If (.not. (Rb.GE.-5 .and. Rb.LE.10)) Rb =1         !Evita que valores extremos de Rb produzcan error
            		! /Tracking method   	
            	
            		! Radiation on sloped surface models
            		SELECT CASE (sm)
    				CASE (0)		! Isotropic Sky
    				It= radDif + radDir*Rb
    			
    				CASE (1)		! Isotropic Diffuse
    				It = radDir*Rb + radDif*(1+cos(slope)/2)+ radGlo*rho*(1-cos(slope))/2
    			
    				CASE (2)
    				Ai = radDir/(1367.*(1+0.033*cos(360.*diaj/365.)))
    				It = (radDirHor+RadDif*Ai)*Rb/costhetaz + radDif*(1-Ai)*(1+cos(slope))/2 &
    						+ radGlo*rho*(1-cos(slope))/2

					CASE (3)
					Ai = radDir/(1367.*(1+0.033*cos(360.*diaj/365.)))
					ff = sqrt(radDirHor/radGLo)
					It = (radDirHor+RadDif*Ai)*Rb/costhetaz  &
							+ radDif*(1-Ai)*(1+cos(slope))/2*(1+ff*sin(slope/2)**3) &
    						+ radGlo*rho*(1-cos(slope))/2
				
            		END SELECT
            		
            		Rs = It/radGlo
            		! /Radiation on sloped surface models
            	END IF            	
            	           	
        	ELSE 
            	radComp=0
            	e=0
            	Phi_az =0
            	radDir=0
            	radGLo=0 
            	radDif=0
            	It = 0
            	Rs = 0
            	Rb = 0
        	END IF
		END IF
        
        IF (tm /= 0) then  ! Impresion de datos en ‘salida.txt’ 
        
        	IF(vs==1) WRITE(*,1000)year, diaj,hh_lst,mm_lst,radGlo, radDif,radDir, &
        								radComp,e,Phi_az, It , slope* rad_to_deg
        								
        	WRITE(21,1200) year, diaj, hh_lst,mm_lst,radGlo, radDif,radDir,  &
        								radComp,e,Phi_az, It , slope* rad_to_deg,kt,kd,kn
        else
			IF(vs==1) WRITE(*,1100)year, diaj,hh_lst,mm_lst, radGlo, radDif,radDir,radComp,e,Phi_az
       		WRITE(21,1300) year, diaj, hh_lst,mm_lst, radGlo, radDif,radDir,radComp,e,Phi_az,kt,kd,kn
        endif
		   	
	ENDDO ! Fin de ciclo de lectura de datos de radiacion

!	 If (Graph == 1) CALL DISFIN !Level 2->0
	
ELSE	!Si no abrio bien el archivo de datos, then.. 
    WRITE(*,*) 'Error al abrir el archivo de datos:',argument, 'Error:',ierror
    
ENDIF

IF ( ierror2 /= 0 ) WRITE (*,*) 'Error al tratar de escribir el archivo de salida.  #',ierror2

close(21)

999 STOP 

1000    FORMAT (I5, I5,'  ',I2.2,':',I2.2, F9.2, F9.2, F9.2, F9.2, F9.2,F9.2,F9.2,F6.1) 
1100    FORMAT (I5, I5,'  ',I2.2,':',I2.2, F9.2, F9.2, F9.2, F9.2, F9.2,F9.2 )
1200    FORMAT (I4,';',I3,';',I2.2,':',I2.2,';', F7.2,';', F7.2,';', F7.2,';', F7.2,';', F6.2,';',F6.2,';',F7.2,';',F5.1,&
				&';',F4.2,';',F4.2,';',F4.2) 
1300    FORMAT (I4,';',I3,';',I2.2,':',I2.2,';', F7.2,';', F7.2,';', F7.2,';', F7.2,';', F6.2,';',F6.2,';',&
				& F5.2,';',F5.2,';',F5.2)            

END Program SRP
!************************************************************************************ /Main, Subrutines  ************************

Subroutine TMY23head(tz,latitude,longitude,elevation)
implicit none
INTEGER :: n=1, ierror, sel=1

!TMY2  Head
 CHARACTER (LEN=5) :: WBAN 
 CHARACTER (LEN=22) :: city
 CHARACTER (LEN=2) ::  state
 INTEGER :: tz
 CHARACTER (LEN=1) ::  latitude
 REAL :: LatDeg
 REAL :: LatMin
 CHARACTER (LEN=1) ::  longitude
 REAL :: LonDeg
 REAL :: LonMin 
 INTEGER :: Elevation 
 
  INTEGER :: errorread
 
1022 FORMAT ( 1X,A5,1X,A22,1X,A2,1X,I3,1X,A1,1X,I2,1X,I2,1X,A1,1X,I3,1X,I2,2X,I4 ) !Header
OPEN (UNIT=8, FILE='datos3.tm2', IOSTAT=errorread)	      
READ(8,1022,IOSTAT=ierror)WBAN , city, state, tz, latitude,LatDeg,LatMin, longitude,LonDeg, LonMin, elevation
	Write (*,*) WBAN , city, state, tz
	LatDeg = LatDeg + (LatMin/100.) 
	If (latitude == 'S') LatDeg = -LatDeg
	LonDeg = LonDeg + (LonMin/100.) 
	If (longitude == 'W') LonDeg = -LonDeg
    LatDeg = LatDeg * 3.141591/180.d0   ! Revisar radianes o deg. 
    LatDeg = LatDeg * 3.141591/180.d0
End Subroutine TMY23head


Subroutine seconds_hhmmss(time,hh_lst,mm_lst,ss_lst)
implicit none
real(8) time, hh_dec
integer hh_lst,mm_lst,ss_lst
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
end subroutine seconds_hhmmss


FUNCTION norm_hora(value)
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
norm_hora = REAL(h)*3600.+REAL(m)*60.
END FUNCTION norm_hora


SUBROUTINE doy (day_of_year, year, day, month) 
! Calculo del mes y dia del mes segun dia del año
!This program calculates the date corresponding to a specified julian day.
IMPLICIT NONE

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

      subroutine sunae(year,day,hour,lat,long,az,el,ha,dec,soldst)
!  work with real variables and define some constants, including
!  one to change between degs and radians
      implicit real (a-z)
      data twopi,pi,rad/6.2831853,3.1415927,.017453293/
      
!   get the current julian date (actually add 2,400,000 for jd)
      delta=year-1949.
      leap=aint(delta/4.)
      jd=32916.5+delta*365.+leap+day+hour/24.
!   1st no. is mid. 0 jan 1949 minus 2.4e6; leap=leap days since 1949
!  the last yr of century is not leap yr unless divisible by 400
      if(amod(year,100.).eq.0.0.and.amod(year,400.).ne.0.0)jd=jd-1.

!   calculate ecliptic coordinates
      time=jd-51545.0
!   51545.0 + 2.4e6 = noon 1 jan 2000

!   force mean longitude between 0 and 360 degs
      mnlong=280.460+.9856474*time
      mnlong=mod(mnlong,360.)
      if(mnlong.lt.0.)mnlong=mnlong+360.

!   mean anomaly in radians between 0 and 2*pi
      mnanom=357.528+.9856003*time
      mnanom=mod(mnanom,360.)
      if(mnanom.lt.0.)mnanom=mnanom+360.
      mnanom=mnanom*rad

!   compute the ecliptic longitude and obliquity of ecliptic in radians
      eclong=mnlong+1.915*sin(mnanom)+.020*sin(2.*mnanom)
      eclong=mod(eclong,360.)
      if(eclong.lt.0.)eclong=eclong+360.
      oblqec=23.439-.0000004*time
      eclong=eclong*rad
      oblqec=oblqec*rad

!   calculate right ascension and declination
      num=cos(oblqec)*sin(eclong)
      den=cos(eclong)
      ra=atan(num/den)
!   force ra between 0 and 2*pi
      if(den.lt.0)then
          ra=ra+pi
      elseif(num.lt.0)then
          ra=ra+twopi
      endif

!   dec in radians
      dec=asin(sin(oblqec)*sin(eclong))

!   calculate Greenwich mean sidereal time in hours
      gmst=6.697375+.0657098242*time+hour 
!   hour not changed to sidereal time since 'time' includes
!   the fractional day 
      gmst=mod(gmst,24.)
      if(gmst.lt.0.)gmst=gmst+24.

!   calculate local mean sidereal time in radians 
      lmst=gmst+long/15.
      lmst=mod(lmst,24.)
      if(lmst.lt.0.)lmst=lmst+24.
      lmst=lmst*15.*rad

!   calculate hour angle in radians between -pi and pi
      ha=lmst-ra
      if(ha.lt.-pi)ha=ha+twopi
      if(ha.gt.pi)ha=ha-twopi

!   change latitude to radians
      lat=lat*rad

!   calculate azimuth and elevation
      el=asin(sin(dec)*sin(lat)+cos(dec)*cos(lat)*cos(ha))
      az=asin(-cos(dec)*sin(ha)/cos(el))

!   this puts azimuth between 0 and 2*pi radians
      if(sin(dec)-sin(el)*sin(lat).ge.0.)then
      if(sin(az).lt.0.)az=az+twopi
      else
      az=pi-az
      endif
!   if az=90 degs, elcritical=asin(sin(dec)/sin(lat))
!    elc=asin(sin(dec)/sin(lat))
!    if(el.ge.elc)az=pi-az
!    if(el.le.elc.and.ha.gt.0.)az=twopi+az

!   calculate refraction correction for US stan. atmosphere
!   need to have el in degs before calculating correction
      el=el/rad
!
      if(el.ge.19.225) then 
         refrac=.00452*3.51823/tan(el*rad)
      else if (el.gt.-.766.and.el.lt.19.225) then
         refrac=3.51823*(.1594+.0196*el+.00002*el**2)/ &
     &   (1.+.505*el+.0845*el**2)
      else if (el.le.-.766) then
         refrac=0.0
      end if

!   note that 3.51823=1013.25 mb/288 C
      el=el+refrac
!   elevation in degs
!
!   calculate distance to sun in A.U. & diameter in degs
      soldst=1.00014-.01671*cos(mnanom)-.00014*cos(2.*mnanom)
      soldia=.5332/soldst

!   convert az and lat to degs before returning
      az=az/rad
      lat=lat/rad
	 ha=ha/rad
	 dec=dec/rad

!   mnlong in degs, gmst in hours, jd in days if 2.4e6 added;
!   mnanom,eclong,oblqec,ra,and lmst in radians
      return
      end subroutine

