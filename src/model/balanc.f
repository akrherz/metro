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
*     Sous-routine BALANC: effectue la prevision des conditions routieres
*                         (pour une station) a partir du bilan energetique 
*                         de la surface et des conditions de surface.
*
*     Auteur: Louis-Philippe Crevier
*     Date: Juillet 1999
***
      SUBROUTINE BALANC ( FS  , FI , P0  , TA , QA , VA , TYP, FT, PR,
     *                    iref, NTP, NTFM, CNT_IN, ITP, FLAT,
     *                    FCOR, WW , WA, ALN, ALR, FP,
     *                    FSCORR   , FICORR  , ER1, ER2, 
     *                    EPSILON, Z0, Z0T, ZU, ZT, ECHEC, dpRT, 
     *                    dpRA, dpSN,
     *                    npRC, dpIR, dpSF, dpFV,
     *                    dpFC, dpFA, dpG, dpBB, dpFP)

      IMPLICIT NONE
      INTEGER i, j
      INTEGER Nl, n
      REAL DT
      INTEGER DTMAX, NRECMAX, NCOLMAX

      COMMON/BUFFER_SIZE/ DTMAX, Nl, DT, NRECMAX, NCOLMAX, n

***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Entrees
*     -------
*     FS: Flux solaire incident (W)
*     FI: Flux infra-rouge incident (W)
*     P0: Pression de surface (Pa)
*     TA: Temperature de l'air (C) au niveau ZT
*     QA: Humidite specifique (g/kg) au niveau ZT
*     VA: Vent (m/s) au niveau ZU
*     TYP: Type probable de precipitation ( 1 -> pluie, 2 -> neige )
*     FT: "Forecast time" (heures GMT)
*     PR: Taux de precipitation (m/s)
*     iref: Nombre de niveau dans la grille
*     NTP: Indice de debut de la prevision
*     NTFM: Indice de fin de la prevision
*     CNT: Constantes de la grille
*     Z0: Longueur de rugosite (m)
*     Z0T: Longueur de rugosite (m)
*     ZU: Hauteur du niveau de la prevision de vent (m)
*     ZT: Hauteur du niveau de la prevision de temperature de d'humidite (m)
*     LCORR: Decalage horaire pour retrouver l'heure local (h)
*     EPSILON: Emissivite de la route
*     TS0: Temperature cible pour la fin du couplage (C)
*     FCOR: Facteur de Coriolis
*     WW: Vents minimum le jour et la nuit (m/s)
*     WA: Flux "anthropogenique"
*     ALN: Albedo de la neige
*     ALR: Albedo de la route 
*     FP: Point de congelation (C)
*     FLAT: Specifie si pont ou non
*     ITP: Profil de temperature de la route (C, iref niveaux != 0)
*     FSCORR: Coefficient de couplage du flux solaire
*     FICORR: Coefficient de couplage du flux infra-rouge
***
      LOGICAL FLAT
      INTEGER iref, NTP, NTFM
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
      DOUBLE PRECISION dpFP(DTMAX)
      INTEGER npRC(DTMAX)
***
*     Entrees/Sorties
*     ---------------
*     FLUX Matrice contenant differents champs de sortie
*         FLUX(1,i): FT(i)
*         FLUX(2,i): TA(i)
*         FLUX(3,i): VA(i)
*         FLUX(4,i): TYP(i)
*         FLUX(5,i): TS(i)
*         FLUX(6,i): ER1(i)
*         FLUX(7,i): ER2(i)
*         FLUX(8,i): PR(i)
*         FLUX(9,i): encore indeterminee
*         FLUX(10,i): etat de la route
*                     Code: 1. -> Pave sec
*                           2. -> Pave mouille
*                           3. -> Glace
*                           4. -> Rosee (Dew)
*                           5. -> Gel (Frost)
*                           6. -> Pluie verglacante
*                           7. -> Neige
*     TS: Serie temporelle de temperature de surface
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
*     Variables de FLXSURFZ
*     ---------------------
***
      REAL CMU, CTU, RIB, ILMO, FTEMP, FVAP
      REAL H, UE, LZZ0, LZZ0T, fm, fh
***
*     Initialisation du common CLELOG
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

**     Conversion du array en matrice
      CALL ARRAY2MATRIXDOUBLEPRECISION(CNT_IN, CNT , n, 2)

*     Initialisation de parametres
*     ----------------------------
      FZ = 0.1
      H = 300.0
      FGD = 0.0
      ILMO = 1.
      next = 1
      now = 2
*     Initialisation de la variation d'albedo en fonction de la neige
*     ---------------------------------------------------------------
*     |                                                             |
*     |Fonction utilisee:                                           |
*     |                                                             |
*     | AL = max( ALR , min( ALN , M*ER2+ALR-M ) )                  |
*     |                                                             |
*     | ou                                                          | 
*     |     M = ( ALN - ALR ) / 5 millimetres d'equivalent-eau      |
*     |                                                             |
*     | i.e.                                                        |
*     |                                                             |
*     |      l'albedo varie lineairement de ALR a ALN pour ER2      |
*     |      compris entre 1 et 6 millimetres                       |
*     |                                                             |
*     |      AL = ALR pour ER2 < 1 mm                               |
*     |                                                             |
*     |      AL = ALN pour ER2 > 6 mm                               |
*     |                                                             |
*     ---------------------------------------------------------------
      M = (ALN - ALR) / 5.
*     Initialisation du profil de temperature
*     ---------------------------------------
      G(0) = 0.0
      do j=1,iref
         T(j,now)=ITP(j)
         T(j,next)=ITP(j)
         G(j) = 0.
      end do
*     ++++++++++++++++++++++++++++++++++
*      Entree de la boucle de prevision
*     ++++++++++++++++++++++++++++++++++
      do i = NTP+1,NTFM
*        Preparation des differents termes du bilan
*        ++++++++++++++++++++++++++++++++++++++++++
*        road temp in Kelvins
         TSK = T(1,now) + TCDK
*        radiation corps noir
         RA = EPSILON*STEFAN*TSK**4
*        humidite a la suface
         call SRFHUM  ( QG, CL, ER1, ER2, TSK, P0(i), QA(i), FP )
*        densite de l'air au sol
         ier = CHKDIV ( RGASD * FOTVT ( TSK , QG ), "balanc.ftn", 196 )
         if ( ier .eq. 1 ) then
            WRITE(*,*) "Echec dans balanc.f"
            ECHEC = .true.
            return
         end if
         RHO = P0(i) / ( RGASD * FOTVT ( TSK , QG ) )

*        modification du vent
         call VENMIN  ( WW, FT(i), VA(i) )
*        energie utilisee/liberee par neige fondante/pluie verglacante
         call VERGLAS ( TYP(i), T(1,now), FP, FZ, PR(i), PR1, PR2, PRG )
*        coefficients des flux de chaleur
         call FLXSURFZ( CMU  , CTU , RIB  , FTEMP, FVAP ,
     *                  ILMO, UE   , FCOR , TA(i)+TCDK,
     *                  QA(i), ZU, ZT  , VA(i), TSK  , QG   ,
     *                  H    , Z0  , Z0T  , LZZ0 , LZZ0T,
     *                  FM   , FH  ,  1,  1     )
*        albedo efficace
         AL = 1.0 - max( ALR, min( ALN,M*ER2+ALR-M ) )
*        relaxation des coefficients de couplage des flux
         COFS = 1.0 + FSCORR*exp((-DT)*real(i-NTP)/(4.*3600.))
         COFI = 1.0 + FICORR*exp((-DT)*real(i-NTP)/(4.*3600.))
*        Calcul du bilan energetique
*        +++++++++++++++++++++++++++
*         (COFS*AL*FS(i)) : Flux solaire absorbé
*         RA : Flux montant infra-rouge
*         COFI*EPSILON*FI(i) : Flux infrarouge absorbé
*         RHO*CTU*( CPD*(TA(i)-T(1,now))) FC
*         RHO*CTU*(CL*(QA(i)-QG)) : FV
*         PRG : Flux du changement de phase
*         WA : Flux anthropogénique
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
         G(1) = CNT(1,1) * ( T(2,now) - T(1,now) )

*        Transition de phase lors d'un passage par le point de fusion
*        ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         if ( T(1,next).lt.FP .and. T(1,now).ge.FP ) then
*           Degel, G(0) positif 
            FGD = - (ER2 * CHLF)
         else if( T(1,next).gt.FP .and. T(1,now).le.FP ) then
*           Gel, G(0) negatif
            FGD = ER1 * CHLF
         end if
*        NOTE: G(0) = G(0) + G(1) lorsque FGD .ne. 0.0 pour
*        conservation d'energie
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
*        Calculer l'evolution des temperatures dans le sol
*        +++++++++++++++++++++++++++++++++++++++++++++++++
         call TSEVOL ( T, iref, now, CNT, G, FLAT, TA(i) )
*        Bilan d'accumulations au sol
*        ++++++++++++++++++++++++++++

         call RODCON ( ER1, ER2, RHO  , CTU, CL , FP , FZ ,
     *                 T(1,now), QA(i), QG , PR1, PR2, PRG, DX , PR,
     *                 npRC(i) )

         dpRT(i) = T(1,next)
         dpRA(i) = ER1
         dpSN(i) = ER2
*        Inversion des indices
*        +++++++++++++++++++++
         next = 3 - next
         now = 3 - now
      end do
      
**     Conversion de la matrice en array
      CALL MATRIX2ARRAYDOUBLEPRECISION(CNT, CNT_IN, n, 2)

      if( .not. bSilent) then
         WRITE(*,*) "FIN BALANC"
      end if

      return
      end
