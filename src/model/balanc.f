*
* METRo : Model of the Environment and Temperature of Roads
* METRo is Free and is proudly provided by the Government of Canada
* Copyright (C) 2006 Environment Canada

*  Questions or bugs report: metro@ec.gc.ca
*  METRo repository: https://gna.org/projects/metro/
*  Documentation: http://documentation.wikia.com/wiki/METRo
*
*
* Code contributed by:
*  Louis-Philippe Crevier - Canadian meteorological center
*  Miguel Tremblay - Canadian meteorological center
*
*  $LastChangedDate$
*  $LastChangedRevision$
*
************************************************************************
*  This program is free software; you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation; either version 2 of the License, or
*  (at your option) any later version.
*
*  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software
*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*
*
*

***
*     Subroutine BALANC: perform the road condition forecast for one station
*                         using the energy balance and the condition of the
*                         surface.
*     Sous-routine BALANC: effectue la prevision des conditions routieres
*                         (pour une station) a partir du bilan energetique 
*                         de la surface et des conditions de surface.
*
*     Auteur: Louis-Philippe Crevier
*     Date: Juillet 1999
***
      SUBROUTINE BALANC ( FS  , FI , P0  , TA , QA , VA , TYP, FT, PR,
     *                    iref, ir40, NTP, NTFM, CNT_IN, ITP, FLAT,
     *                    FCOR, WW , WA, ALN, ALR, FP,
     *                    FSCORR   , FICORR  , ER1, ER2, 
     *                    EPSILON, Z0, Z0T, ZU, ZT, ECHEC, dpRT, 
     *                    dpRA, dpSN,
     *                    npRC, dpIR, dpSF, dpFV,
     *                    dpFC, dpFA, dpG, dpBB, dpFP, dpSST)

      IMPLICIT NONE
      INTEGER i, j
      INTEGER Nl, n
      REAL DT
      INTEGER DTMAX

      COMMON/BUFFER_SIZE/ DTMAX, Nl, DT, n

***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Input
*     -------
*     FS: Incident solar flux (W)
*     FI: Incident infra-red flux (W)
*     P0: Surface pressure (Pa)
*     TA: Air temperature (C) at level ZT
*     QA: Specific humidity (g/kg) at level ZT
*     VA: Wind (m/s) at level ZU
*     TYP: Probable type of precipitation ( 1 -> rain, 2 -> snow )
*     FT: "Forecast time" (GMT)
*     PR: Precipitation rate (m/s)
*     NTP: Index for the start of coupling
*     NTFM: Index for the end of forecast
*     CNT: Grid constants
*     Z0: Roughness length (m)
*     Z0T: Roughness length (m)
*     ZU: Height of level of the wind forecast (m)
*     ZT:  Height of level of the atmospheric forecast (m)
*     LCORR: time shift to retrieve local time
*     EPSILON: Road emissivity
*     TS0: Target temperature for the end of coupling (C)
*     FCOR: Coriolis factor
*     WW: Minimum winds for the day and the night (m/s)
*     WA: Anthropogenic flux
*     ALN: Snow Albedo 
*     ALR: Road Albedo 
*     FP: Frozing point (C)
*     FLAT: Road or bridge
*     ITP: Road temperature profile (C, iref levels != 0)
*     FSCORR: Coupling coefficient of solar flux
*     FICORR: Coupling coefficient of infra-red flux
*     iref:  number of grid levels 
*     ir40: level at 40 cm depth 
***
      LOGICAL FLAT
      INTEGER iref, ir40, NTP, NTFM
      DOUBLE PRECISION FS(DTMAX),  FI(DTMAX), P0(DTMAX)
      DOUBLE PRECISION TA(DTMAX), QA(DTMAX), VA(DTMAX)
      DOUBLE PRECISION TYP(DTMAX), FT(DTMAX), PR(DTMAX)
      DOUBLE PRECISION CNT(n,2), CNT_IN(2*n), ITP(n)
      DOUBLE PRECISION FCOR, WW(2), WA
      DOUBLE PRECISION ALN, ALR, FP      
      DOUBLE PRECISION  FSCORR, FICORR
      DOUBLE PRECISION EPSILON, ZU, ZT, Z0, Z0T
      DOUBLE PRECISION dpSN(DTMAX), dpRA(DTMAX)
      DOUBLE PRECISION dpIR(DTMAX), dpSF(DTMAX)
      DOUBLE PRECISION dpFC(DTMAX), dpFA(DTMAX)
      DOUBLE PRECISION dpG(DTMAX), dpBB(DTMAX)
      DOUBLE PRECISION dpRT(DTMAX), dpFV(DTMAX)
      DOUBLE PRECISION dpFP(DTMAX), dpSST(DTMAX)
      INTEGER npRC(DTMAX)
***
*     Input/Output
*     ---------------
*     FLUX Matrix contening the output fields
*         FLUX(1,i): FT(i)
*         FLUX(2,i): TA(i)
*         FLUX(3,i): VA(i)
*         FLUX(4,i): TYP(i)
*         FLUX(5,i): TS(i)
*         FLUX(6,i): ER1(i)
*         FLUX(7,i): ER2(i)
*         FLUX(8,i): PR(i)
*         FLUX(9,i): not used
*         FLUX(10,i): Road condition
*                     Code: 1. -> Dry road
*                           2. -> Wet road
*                           3. -> Ice
*                           4. -> Dew
*                           5. -> Frost
*                           6. -> Freezing rain
*                           7. -> Snow
*     TS:Time series of surface temperature
***
      LOGICAL ECHEC
      DOUBLE PRECISION ER1, ER2
***
*     Internes
*     --------
***
      INTEGER ier, CHKDIV
      EXTERNAL CHKDIV
      INTEGER next, now
      REAL T(n,2), G(0:n), FGD
      REAL RA, QG, RHO, TSK, AL, M
      REAL CL, PR1, PR2, DX, PRG, FZ
      REAL COFS, COFI
***
*     Variables of FLXSURFZ
*     ---------------------
***
      REAL CMU, CTU, RIB, ILMO, FTEMP, FVAP
      REAL H, UE, LZZ0, LZZ0T, fm, fh
***
*     Initialization of common CLELOG
*     -------------------------------
***

      REAL CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW, EPS1
      REAL DELTA, CAPPA, TGL, CONSOL, GRAV, RAYT, STEFAN, PI
      REAL OMEGA, EPS2
      REAL KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD

      COMMON/CTSPHY/  CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW,
     $                EPS1, EPS2, DELTA, CAPPA, TGL, CONSOL,
     $                GRAV, RAYT, STEFAN, PI, OMEGA,
     $                KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD


      REAL TTT,PRS,QQQ,EEE,TVI,QST
      REAL FOEW,FODLE,FOQST,FODQS,FOEFQ,FOQFE,FOTVT,FOTTV,FOHR
      REAL FOEWA,FODLA,FOQSA,FODQA,FOHRA
      LOGICAL bSilent
      COMMON /SILENT/ bSilent

      INTRINSIC SIGN


*   DEFINITION DES FONCTIONS THERMODYNAMIQUES DE BASE
*   POUR LES CONSTANTES, UTILISER LE COMMON /CTESDYN/
*     NOTE: TOUTES LES FONCTIONS TRAVAILLENT AVEC LES UNITES S.I.
*           I.E. TTT EN DEG K, PRS EN PA, QQQ EN KG/KG
*          *** N. BRUNET - MAI 90 ***
*          * REVISION 01 - MAI 94 - N. BRUNET
*                          NOUVELLE VERSION POUR FAIBLES PRESSIONS
*
*     FONCTION DE TENSION DE VAPEUR SATURANTE (TETENS) - EW OU EI SELON TT
      FOEW(TTT) = 610.78*EXP( AMIN1(SIGN(17.269,TTT-TRPL),SIGN
     W (21.875,TTT-TRPL))*ABS(TTT-TRPL)/(TTT-35.86+AMAX1(0.,SIGN
     W (28.2,TRPL-TTT))))
*
*     FONCTION CALCULANT LA DERIVEE SELON T DE  LN EW (OU LN EI)
      FODLE(TTT)=(4097.93+AMAX1(0.,SIGN(1709.88,TRPL-TTT)))
     W /((TTT-35.86+AMAX1(0.,SIGN(28.2,TRPL-TTT)))*(TTT-35.86+AMAX1(0.
     W ,SIGN(28.2,TRPL-TTT))))
*
*     FONCTION CALCULANT L'HUMIDITE SPECIFIQUE SATURANTE (QSAT)
      FOQST(TTT,PRS)=EPS1/(AMAX1(1.,PRS/FOEW(TTT))-EPS2)
*
*     FONCTION CALCULANT LA DERIVEE DE QSAT SELON T
      FODQS(QST,TTT)=QST*(1.+DELTA*QST)*FODLE(TTT)
*     QST EST LA SORTIE DE FOQST
*
*     FONCTION CALCULANT TENSION VAP (EEE) FN DE HUM SP (QQQ) ET PRS
      FOEFQ(QQQ,PRS) = AMIN1(PRS,(QQQ*PRS) / (EPS1 + EPS2*QQQ))
*
*      FONCTION CALCULANT HUM SP (QQQ) DE TENS. VAP (EEE) ET PRES (PRS)
      FOQFE(EEE,PRS) = AMIN1(1.,EPS1*EEE / (PRS-EPS2*EEE))
*
*      FONCTION CALCULANT TEMP VIRT. (TVI) DE TEMP (TTT) ET HUM SP (QQQ)
      FOTVT(TTT,QQQ) = TTT * (1.0 + DELTA*QQQ)
*
*      FONCTION CALCULANT TTT DE TEMP VIRT. (TVI) ET HUM SP (QQQ)
      FOTTV(TVI,QQQ) = TVI / (1.0 + DELTA*QQQ)
*
*      FONCTION CALCULANT HUM REL DE HUM SP (QQQ), TEMP (TTT) ET PRES (PRS)
*      HR = E/ESAT
       FOHR(QQQ,TTT,PRS) = AMIN1(PRS,FOEFQ(QQQ,PRS)) / FOEW(TTT)
*
*     LES 5 FONCTIONS SUIVANTES SONT VALIDES DANS LE CONTEXTE OU ON
*     NE DESIRE PAS TENIR COMPTE DE LA PHASE GLACE DANS LES CALCULS
*     DE SATURATION.
*   FONCTION DE VAPEUR SATURANTE (TETENS)
      FOEWA(TTT)=610.78*EXP(17.269*(TTT-TRPL)/(TTT-35.86))
*   FONCTION CALCULANT LA DERIVEE SELON T DE LN EW
      FODLA(TTT)=17.269*(TRPL-35.86)/(TTT-35.86)**2
*   FONCTION CALCULANT L'HUMIDITE SPECIFIQUE SATURANTE
      FOQSA(TTT,PRS)=EPS1/(AMAX1(1.,PRS/FOEWA(TTT))-EPS2)
*   FONCTION CALCULANT LA DERIVEE DE QSAT SELON T
      FODQA(QST,TTT)=QST*(1.+DELTA*QST)*FODLA(TTT)
*   FONCTION CALCULANT L'HUMIDITE RELATIVE
      FOHRA(QQQ,TTT,PRS)=AMIN1(PRS,FOEFQ(QQQ,PRS))/FOEWA(TTT)
*
***
*
*     Procedure
*     =========    
      if( .not. bSilent) then
         WRITE(*,*) "DEBUT BALANC"
      end if

**    Conversion of array in matrix
      CALL ARRAY2MATRIXDOUBLEPRECISION(CNT_IN, CNT , n, 2)

*     Initialisation of parametres
*     ----------------------------
      FZ = 0.1
      H = 300.0
      FGD = 0.0
      ILMO = 1.
      next = 1
      now = 2
*     Initialization of the albeda variation depending of the snow
*     ---------------------------------------------------------------
*     |                                                             |
*     |Functions used:                                           |
*     |                                                             |
*     | AL = max( ALR , min( ALN , M*ER2+ALR-M ) )                  |
*     |                                                             |
*     | ou                                                          | 
*     |     M = ( ALN - ALR ) / 5 millimetres water equivalent      |
*     |                                                             |
*     | i.e.                                                        |
*     |                                                             |
*     |      albedo change linearly from ALR to ALN for ER2         |
*     |      inside  1 and 6 millimetres                            |
*     |                                                             |
*     |      AL = ALR for  ER2 < 1 mm                               |
*     |                                                             |
*     |      AL = ALN for  ER2 > 6 mm                               |
*     |                                                             |
*     ---------------------------------------------------------------
      M = (ALN - ALR) / 5.
*     Initialization of temperature profile
*     ---------------------------------------
      G(0) = 0.0
      do j=1,iref
         T(j,now)=ITP(j)
         T(j,next)=ITP(j)
         G(j) = 0.
      end do
*     ++++++++++++++++++++++++++++++++++
*      Beginning of the forecast loop
*     ++++++++++++++++++++++++++++++++++
      do i = NTP+1,NTFM
*        Initialization of the differents balance terms
*        ++++++++++++++++++++++++++++++++++++++++++
*        road temp in Kelvins
         TSK = T(1,now) + TCDK
*        black body radiation
         RA = EPSILON*STEFAN*TSK**4
*        humidite at the suface
         call SRFHUM  ( QG, CL, ER1, ER2, TSK, P0(i), QA(i), FP )
*        Air density at the surface
         ier = CHKDIV ( RGASD * FOTVT ( TSK , QG ), "balanc.ftn", 196 )
         if ( ier .eq. 1 ) then
            WRITE(*,*) "Echec dans balanc.f"
            ECHEC = .true.
            return
         end if
         RHO = P0(i) / ( RGASD * FOTVT ( TSK , QG ) )

*        wind modification 
         call VENMIN  ( WW, FT(i), VA(i) )
*        energy used/freed by the melting snow/freezing rain
         call VERGLAS ( TYP(i), T(1,now), FP, FZ, PR(i), PR1, PR2, PRG )
*        coefficients of heat flux
         call FLXSURFZ( CMU  , CTU , RIB  , FTEMP, FVAP ,
     *                  ILMO, UE   , FCOR , TA(i)+TCDK,
     *                  QA(i), ZU, ZT  , VA(i), TSK  , QG   ,
     *                  H    , Z0  , Z0T  , LZZ0 , LZZ0T,
     *                  FM   , FH  ,  1,  1     )
*        effective albedo 
         AL = 1.0 - max( ALR, min( ALN,M*ER2+ALR-M ) )
*        relaxation of coefficients of flux coupling
         COFS = 1.0 + FSCORR*exp((-DT)*real(i-NTP)/(4.*3600.))
         COFI = 1.0 + FICORR*exp((-DT)*real(i-NTP)/(4.*3600.))
*        Computing of energy balance
*        +++++++++++++++++++++++++++
*         (COFS*AL*FS(i)) : Absorbed solar flux
*         RA : Outgoing infra-red flux
*         COFI*EPSILON*FI(i) : Absorbed infra-red flux
*         RHO*CTU*( CPD*(TA(i)-T(1,now))) FC
*         RHO*CTU*(CL*(QA(i)-QG)) : FV
*         PRG : Phase change flux
*         WA : Anthropogenic flux
         G(0) = max(0.0,REAL(COFS*AL*FS(i))) - RA + COFI*EPSILON*FI(i)
     *        + RHO*CTU*( CPD*(TA(i)-T(1,now)) + CL*(QA(i)-QG)) + PRG
     *        + WA
*        Output information         
         dpIR(i) = COFI*EPSILON*FI(i)
         dpSF(i) = COFS*AL*FS(i)
         dpFV(i) = RHO*CTU*(CL*(QA(i)-QG))
         dpBB(i) = - RA
         dpFC(i) =  RHO*CTU*( CPD*(TA(i)-T(1,now)))
         dpFA(i) = WA
         dpFP(i) = PRG
         dpG(i) = G(0)
         dpSST(i) = T(ir40, now)
         IF ( MOD(i,40) .eq. 0 ) THEN
*           WRITE(*,*) "<vl>", i
           DO j=1, iref
*            WRITE(*,*) '<level-temp num="', j-1, '">', T(j, now), 
*     *        '</lev-temp>'
           END DO
*         WRITE(*,*) "</vl>"
         END IF
         G(1) = CNT(1,1) * ( T(2,now) - T(1,now) )

*        Phase Transition when passing the melting point
*        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         if ( T(1,next).lt.FP .and. T(1,now).ge.FP ) then
*           Degel, G(0) positif 
            FGD = - (ER2 * CHLF)
         else if( T(1,next).gt.FP .and. T(1,now).le.FP ) then
*           Gel, G(0) negatif
            FGD = ER1 * CHLF
         end if
*        NOTE: G(0) = G(0) + G(1) when FGD .ne. 0.0 for energy conservation
         if ( FGD.lt.0.0 .and. G(0).gt.0.0) then
            G(0) = G(0) - G(1)
            FGD = min( 0.0, FGD + DT*G(0) )
            T(1,next) = T(1,now)
            DX = G(0)/CHLF
         else if ( FGD.gt.0.0 .and. G(0).lt.0.0) then
            G(0) = G(0) - G(1)
            FGD = max( 0.0, FGD + DT*G(0) )
            T(1,next) = T(1,now)
            DX = G(0)/CHLF
         else
            DX = 0.0
            T(1,next) = T(1,now)+DT*(CNT(1,2)*( G(1) - G(0)))
         end if
*        Calculation of temperature evolution in the ground
*        +++++++++++++++++++++++++++++++++++++++++++++++++
         call TSEVOL ( T, iref, now, CNT, G, FLAT, TA(i) )
*        Accumulation balance at the surface
*        ++++++++++++++++++++++++++++

         call RODCON ( ER1, ER2, RHO  , CTU, CL , FP , FZ ,
     *                 T(1,now), QA(i), QG , PR1, PR2, PRG, DX , 
     *                 npRC(i) )

         dpRT(i) = T(1,next)
         dpRA(i) = ER1
         dpSN(i) = ER2
*        Inversion of indices
*        +++++++++++++++++++++
         next = 3 - next
         now = 3 - now
      end do
      
**     Conversion of matrix in array
      CALL MATRIX2ARRAYDOUBLEPRECISION(CNT, CNT_IN, n, 2)

      if( .not. bSilent) then
         WRITE(*,*) "FIN BALANC"
      end if

      return
      end
