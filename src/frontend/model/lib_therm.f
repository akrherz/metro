***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
***
*     Sous-routine VERGLAS: Sous-routine calculant le gel-degel de la
*                           precipitation au contact de la route
*
*     Auteur / Author: Louis-Philippe Crevier
*     Date: Decembre 1999 / December 1999
***
      SUBROUTINE VERGLAS ( TYP, TS, FP, FZ, PR, PR1, PR2, PRG, FAIL )
      IMPLICIT NONE
***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Entrees
*     -------
      REAL TS, FZ
      DOUBLE PRECISION TYP, FP, PR
***
*     Sorties
*     -------
***
      REAL PR1, PR2, PRG
***
*     Entrees/Sorties
*     ---------------
***
      LOGICAL FAIL
***
*     Internes
*     --------
***
      REAL RATIO
***
*     Declarations des constantes physique 
*     et des fonctions thermodynamiques
*     ------------------------------------
***
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
*
      REAL CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW, EPS1
      REAL DELTA, CAPPA, TGL, CONSOL, GRAV, RAYT, STEFAN, PI
      REAL OMEGA, EPS2
      REAL KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
      COMMON/CTSPHY/  CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW,
     $                EPS1, EPS2, DELTA, CAPPA, TGL, CONSOL,
     $                GRAV, RAYT, STEFAN, PI, OMEGA,
     $                KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
***
*
*     Procedure
*     =========
      RATIO = 0.0
*      WRITE(*,*) "Typ, TS, FP", REAL(Typ), TS, REAL(FP)
*      WRITE(*,*) "PR, CHLF, RAUW", PR, CHLF, RAUW, TYP
      if ( TYP.eq.2. .and. TS.le.FP ) then
*     neige sur route a T < FP
         PR1 = 0.0
         PR2 = 1e3 * PR
         PRG = 0.0
      else if ( TYP.eq.1. .and. TS.gt.FP ) then
*     pluie sur route a T > FP
*         WRITE(*,*) "PR", PR
         PR1 = 1e3 * PR
         PR2 = 0.0
         PRG = 0.0
      else if ( TYP.eq.2..and. TS.gt.FP+FZ ) then
*     neige sur route a T > FZ; neige fond au contact de la route
         PR1 = 1e3 * PR
         PR2 = 0.0
         PRG = - (PR * CHLF * RAUW)
      else if ( TYP.eq.1. .and. TS.le.FP-FZ ) then
*     pluie sur route a T < FZ; pluie gele au contact de la route
         PR1 = 0.0
         PR2 = 1e3 * PR
         PRG = PR * CHLF * RAUW
      else if ( TYP.eq.2. .and.
     *        max(min(TS,REAL(FP+FZ)),REAL(FP)).eq.TS ) then
*     neige sur route a FZ > T > 0; une partie de la neige fond au 
*     contact de la route
         RATIO = 0.25*sin(PI*(TS-FP)/FZ)+0.75
         PR1 = RATIO * 1e3 * PR
         PR2 = (1.0-RATIO) * 1e3 * PR
         PRG = - (RATIO * PR * CHLF * RAUW)
      else if ( TYP.eq.1. .and.
     *        min(max(TS,REAL(FP-FZ)),REAL(FP)).eq.TS ) then
*     pluie sur route a -FZ < T < 0; une partie de la pluie gele 
*     au contact de la route
         RATIO = 0.25*sin(PI*(TS-FP)/FZ)+0.75
         PR1 = (1.0-RATIO) * 1e3 * PR
         PR2 = RATIO * 1e3 * PR
         PRG = RATIO * PR * CHLF * RAUW
      else
         FAIL = .true.
      end if
*      WRITE(*,*) "pr1", PR1
*      WRITE(*,*) "PRt", PR
*      WRITE(*,*) "FIN VERGLAS"
      return
      end
************************************************************************
************************************************************************
************************************************************************
***
*     Sous-routine VENMIN: Sous-routine pour imposer le vents minimum 
*                          specifie dans la variable WW.
*
*     Auteur / Author: Louis-Philippe Crevier
*     Date: Decembre 1999 / December 1999
***
      SUBROUTINE VENMIN ( WW, FT, VA )
      IMPLICIT NONE
***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Entrees
*     -------
***
      DOUBLE PRECISION WW(2), FT
***
*     Entrees/Sorties
*     ---------------
***
      DOUBLE PRECISION VA
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
***
*
*     Procedure
*     =========
*     Specification d'un vent minimum selon l'heure du jour
*     ATT: Heures GMT. Basee sur fuseau de l'est.
*     -----------------------------------------------------
      if ( FT .gt. 12.5 .and. FT .lt. 25.5 .or.
     *     FT .gt. 36.5 .and. FT .lt. 50.0 ) then
         VA = max( WW(1), VA )
      else if ( FT .ge. 11.5 .and. FT .le. 12.5 ) then
         VA = max( WW(2)+(FT-11.5)*(WW(1)-WW(2)), VA )
      else if ( FT .ge. 25.5 .and. FT .le. 26.5 ) then
         VA = max( WW(1)+(FT-25.5)*(WW(2)-WW(1)), VA )
      else if ( FT .ge. 35.5 .and. FT .le. 36.5 ) then
         VA = max( WW(2)+(FT-35.5)*(WW(1)-WW(2)), VA )
      else
         VA = max( WW(2), VA )
      end if
      return
      end
************************************************************************
************************************************************************
************************************************************************
***
*     Sous-routine TSEVOL: Calcul l'evolution du profil de temperature
*                          a partir du bilan de surface
*
*     Auteur / Author: Louis-Philippe Crevier
*     Date: Decembre 1999 / December 1999
***
      SUBROUTINE TSEVOL ( T, iref, now, CNT, G, FLAT, TA )
      IMPLICIT NONE
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
      INTEGER Nl, n
      REAL DT
      INTEGER DTMAX, NRECMAX, NCOLMAX
      COMMON /BUFFER_SIZE/ DTMAX, Nl, DT, NRECMAX, NCOLMAX, n
***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Entrees
*     -------
***
      INTEGER now, iref
      REAL G(0:n)
      DOUBLE PRECISION CNT(n,2), TA
      LOGICAL FLAT
***
*     Entrees/Sorties
*     ---------------
***
      REAL T(n,2)
***
*     Internes
*     --------
***
      INTEGER j, next

***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
***
*
*     Procedure
*     =========
      next = 3 - now
      do j=2,iref-1
         G(j) = CNT(j,1) * ( T(j+1,now) - T(j,now) )
      end do
      do j=2,iref-1
         T(j,next) = T(j,now)+DT*(CNT(j,2)*( G(j) - G(j-1 )))
      end do
      if ( FLAT ) then
*     BC: underside temp. is air temp
         T(iref,next) = TA
      else
*     BC: no flux ( G(iref) = 0.0 )
         T(iref,next) = T(j,now) - DT*CNT(j,2)*G(iref-1)
      end if
      return
      end
************************************************************************
************************************************************************
************************************************************************
***
*     Sous-routine RODCON: Calcul l'etat des reservoir ER1 (eau) et 
*                          ER2 (glace/neige)
*
*     Auteur / Author: Louis-Philippe Crevier
*     Date: Decembre 1999 / December 1999
***
      SUBROUTINE RODCON ( ER1, ER2, RHO, CTU, CL , FP, FZ,
     *                    TS , QA , QG , PR1, PR2, PRG, DX, PR, ETAT )
      IMPLICIT NONE
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
      INTEGER Nl, n
      REAL DT
      INTEGER DTMAX, NRECMAX, NCOLMAX
      COMMON /BUFFER_SIZE/ DTMAX, Nl, DT, NRECMAX, NCOLMAX, n
      REAL CH 
*     MEA -> Quantite d'eau avant ecoulement. MEA(1) : liquide 
*      MEA(2): solide
      REAL MEA(2) 
      DATA MEA /0.2, 1.0/ 
*     CH -> Epaisseur maximale de neige avant le passage de la charrue
      parameter ( CH = 4.0 )
***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Entrees
*     -------
***
*     QA Humidite specifique (g/kg) au niveau ZT
*     QG Humidite specifique a la surface de la route

      DOUBLE PRECISION FP, QA, PR
      INTEGER ETAT
      REAL RHO, CTU, CL, FZ
      REAL TS, QG, PR1, PR2, PRG, DX

***
*     Entrees/Sorties
*     ---------------
***
      DOUBLE PRECISION ER1, ER2
***
*     Internes
*     --------
***
      REAL FV1, FV2, CUTOFF
***
*     Declarations des constantes physique 
*     et des fonctions thermodynamiques
*     ------------------------------------
***
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
*
      REAL CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW, EPS1
      REAL DELTA, CAPPA, TGL, CONSOL, GRAV, RAYT, STEFAN, PI
      REAL OMEGA, EPS2
      REAL KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
      COMMON/CTSPHY/  CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW,
     $                EPS1, EPS2, DELTA, CAPPA, TGL, CONSOL,
     $                GRAV, RAYT, STEFAN, PI, OMEGA,
     $                KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
***
*
*     Procedure
*     =========
      QA = REAL(QA)
*      WRITE(*,*) "RHO", RHO
*      WRITE(*,*) "CTU", CTU
*      WRITE(*,*) "CL", CL
*       WRITE(*,*) "QA", QA
*       WRITE(*,*) "QG", QG
*      WRITE(*,*) "CHLF", CHLF
*      WRITE(*,*) "RHO*CTU*CL", RHO*CTU*CL
*      WRITE(*,*) "(QA - QG)", (QA - QG)
*      WRITE(*,*) "( QA - QG )/CHLF",( QA - QG )/CHLF
*      WRITE(*,*) "TOTAL=",  RHO*CTU*CL*( QA - QG )/CHLF
*     WRITE(*,*) "TS", TS
*     WRITE(*,*) "FP,FZ", FP, FZ, FP+FZ
      if (TS.gt.FP+FZ ) then
         if ( ER2.gt.0.0 .and. (QA - QG).lt.0.0 ) then
            FV1 = 0.0
            FV2 = RHO*CTU*CL*( QA - QG )/CHLF
         else
            FV1 = RHO*CTU*CL*( QA - QG )/CHLF
            FV2 = 0.0
         end if
      else if (TS.lt.FP-FZ) then
         if ( ER1.gt.0.0 .and. (QA-QG).lt.0.0 ) then
            FV1 = RHO*CTU*CL*( QA - QG )/CHLF
            FV2 = 0.0
         else
            FV1 = 0.0
            FV2 = RHO*CTU*CL*( QA - QG )/CHLF
         end if
      else
         if ( ER1.gt.0.0 ) then
            FV1 = RHO*CTU*CL*( QA - QG )/CHLF
            FV2 = 0.0
         else
            FV1 = 0.0
            FV2 = RHO*CTU*CL*( QA - QG )/CHLF
         end if
      endif
      WRITE(*,*) "ER1", ER1
      WRITE(*,*) "DT", DT
      WRITE(*,*) "PR1", PR1
      WRITE(*,*) "FV1", FV1
      WRITE(*,*) "DX", DX
      WRITE(*,*) "MEA(1)", MEA(1)
*    ER1: Reservoir de pluie (mm ?)
*    Les variables suivantes sont des taux, par seconde
*    PR1: Taux de précipitations de pluie (mm/sec )
*    PR2: Taux de précipitation de neige/glace (mm/sec)
*    PRG: Taux de transfert d'un réservoir à l'autre
*    FV1: Condensation (négatif de l'évaporation)
*    DX: Quantité d'eau fondu
*    MEA: Quantité d'eau maximale => après il y a de l'écoulement
*    3e-3*( max(REAL(ER2),REAL(MEA(2)))-MEA(2)) ): Écoulement, 
*     proportionnel à la hauteur du réservoir.
      ER1 = max(0.0,REAL(ER1 + DT*(
     *     PR1 + FV1 + DX - 
     *     3e-3*( max(REAL(ER1),MEA(1))-MEA(1)) ) ))
*      WRITE(*,*) "ER2", ER2
*      WRITE(*,*) "DT", DT
*      WRITE(*,*) "FV2", FV2
*      WRITE(*,*) "DX", DX
*      WRITE(*,*) "MEA(2)", MEA(1), MEA(2)
*      ER2 =  ER2+PR2*30
*      WRITE(*,*) "ER2", ER2
*      WRITE(*,*) "PR2", PR2
      ER2 = max(0.0,REAL(ER2 + DT*(
     *     PR2 + FV2 - DX 
     *     - 3e-4*( max(REAL(ER2),MEA(2))-MEA(2)) ) ))
*      WRITE(*,*) "ER2", (PR2+FV2-DX)*DT
*      if ( ER2 .gt. CH ) then
*         ER2 = 1.0
*      end if
*     Determination de la condition de la chaussee
*
*         Code: 1. -> Pave sec
*               2. -> Pave mouille
*               3. -> Glace / Neige
*               4. -> Melange eau/neige
*               5. -> Rosee (Dew)
*               6. -> Neige fondante
*               7. -> Gel (Frost)
*               8. -> Pluie verglacante
*
*     --------------------------------------------
      CUTOFF = 0.2
*      WRITE(*,*) "PRG", PRG
      if ( PRG .gt. 0.0 ) then
*     -----------------------------------------------------------
*        8. -> Pluie verglacante
*     -----------------------------------------------------------
         ETAT = 8
      else if ( PRG .lt. 0.0 ) then
*     -----------------------------------------------------------
*        6. -> Neige fondante
*     -----------------------------------------------------------
         ETAT = 6
      else if ( ER1.lt.CUTOFF .and. ER2.lt.CUTOFF ) then
         if ( FV1.gt.0.0 .and. PR1 .eq. 0.0 ) then
*            WRITE(*,*) "ER1", ER1
*            WRITE(*,*) "ER2", ER2
*            WRITE(*,*) "CUTOFF", CUTOFF

*     -----------------------------------------------------------
*           5. -> Rosee (Dew)
*     -----------------------------------------------------------
            ETAT = 5
         else if ( ( FV2 .gt. 0.0 .and. PR2 .eq. 0.0 ) .or.
     *               DX .lt. 0.0 ) then
*     -----------------------------------------------------------
*           7. -> *** Risque de glace noire ***
*                 Frost ou gel d'eau au sol 
*     -----------------------------------------------------------
*            WRITE(*,*) "FV2", FV2
*            WRITE(*,*) "PR2", PR2
*            WRITE(*,*) "DX",DX
            ETAT = 7
         else
*     -----------------------------------------------------------
*           1. -> Pave sec
*     -----------------------------------------------------------
            ETAT = 1
         end if
      else if ( ER1 .ge. CUTOFF .and. ER2 .ge. CUTOFF ) then
*     -----------------------------------------------------------
*        4. -> Transition de phase 
*     -----------------------------------------------------------
         ETAT = 4
      else if ( ER1 .ge. CUTOFF ) then
*     -----------------------------------------------------------
*        2. -> Pave mouille
*     -----------------------------------------------------------
         ETAT = 2
      else if ( ER2 .ge. CUTOFF ) then
*     -----------------------------------------------------------
*        3. -> Glace/Neige
*     -----------------------------------------------------------
         ETAT = 3
      end if
*      WRITE(*,*) "RC", ETAT
      return
      end
************************************************************************
************************************************************************
************************************************************************
***
*     Sous-routine SRFHUM: Calcul l'humidite specifique a la surface de
*                          la route
*
*     Auteur / Author: Louis-Philippe Crevier
*     Date: Decembre 1999 / December 1999
***
      SUBROUTINE SRFHUM ( QG, CL, ER1, ER2, TSK, P0, QA, FP )
      IMPLICIT NONE
***                 ***
*     DEFINITIONS     *
***                 ***
***
*     Entrees
*     -------
***
      DOUBLE PRECISION ER1, ER2, P0, QA, FP
      REAL TSK
***
*     Sorties
*     -------
***
      REAL QG, CL
***
*     Internes
*     --------
***
      REAL QGsat
***
*     Declarations des constantes physique 
*     et des fonctions thermodynamiques
*     ------------------------------------
***
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
*
      REAL CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW, EPS1
      REAL DELTA, CAPPA, TGL, CONSOL, GRAV, RAYT, STEFAN, PI
      REAL OMEGA, EPS2
      REAL KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
      COMMON/CTSPHY/  CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW,
     $                EPS1, EPS2, DELTA, CAPPA, TGL, CONSOL,
     $                GRAV, RAYT, STEFAN, PI, OMEGA,
     $                KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
*
      REAL TTT,PRS,QQQ,EEE,TVI,QST
      REAL FOEW,FODLE,FOQST,FODQS,FOEFQ,FOQFE,FOTVT,FOTTV,FOHR
      REAL FOEWA,FODLA,FOQSA,FODQA,FOHRA
*
      INTRINSIC SIGN
***
*	$Revision: 1.16 $
*
*	$Date: 2005/03/16 21:04:23 $
***
*
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
*     Humidite selon Sass (1992) + 
*     recommandation de Davies pour min(QGsat,QA)
      QGsat = FOQST ( REAL(TSK) , REAL(P0) )
*      WRITE(*,*) "ER1", ER1
*      WRITE(*,*) "ER2", ER2
*      WRITE(*,*) "min(1.0,REAL((ER1+ER2)/0.5))",
*     * min(1.0,REAL((ER1+ER2)/0.5))
*      WRITE(*,*) "QGsat", QGsat
*      WRITE(*,*) "QA", QA
*	write(*,*) "TSK", TSK
*	write(*,*) "P0", P0
*	WRITE(*,*) "TCDK", TCDK
*	write(*,*) "FP", FP
*	write(*,*) "TSK - TCDK",TSK - TCDK
      QG = min(1.0,REAL((ER1+ER2)/0.5))*QGsat +
     *     max(0.0,1.0-REAL((ER1+ER2)/0.5))
     *     *min(REAL(QGsat),REAL(QA))

*     WRITE(*,*) 'ER1', ER1
*     WRITE(*,*) 'ER2', ER2
*      WRITE(*,*) REAL((ER1+ER2)/0.5)
*      WRITE(*,*) 'QGsat', QGsat
*      WRITE(*,*) 'TSK, PO', TSK, P0
*      WRITE(*,*) min(1.0,REAL((ER1+ER2)/0.5))
*      WRITE(*,*) max(0.0,1.0-REAL((ER1+ER2)/0.5))
*      WRITE(*,*) min(REAL(QGsat),REAL(QA))
*     WRITE(*,*) QG
      if ( ER1.gt.0.0 ) then
*         WRITE(*,*) "1- CHLC", CHLC
         CL = CHLC
      else if ( ER2.gt.0.0 ) then
*         WRITE(*,*) "2- CHLC,CHLF", CHLC,CHLF
         CL = CHLC + CHLF
      else
         if ( TSK - TCDK .gt. FP ) then
*          WRITE(*,*) "3- CHLC", CHLC
*     condensation sur route seche
            CL = CHLC
         else
*     frimas sur route seche
            CL = CHLF + CHLC
         end if
      end if
      return
      end
