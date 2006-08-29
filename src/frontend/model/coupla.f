***
*	$Revision: 1.20 $
*
*	$Date: 2005/11/22 19:55:17 $
***
***
*     Sous-routine COUPLA: effectue le couplage des previsions SCRIBE avec 
*                          les observations des stations meteo-routieres
*
*     Auteur /  Author: Louis-Philippe Crevier
*     Date: Aout 1999 / August 1999
***
      SUBROUTINE COUPLA ( FS, FI, P0, TA , QA , VA , TYP, FT, PR,
     *                    PVC , iref, NTP, NTP2, CNT_IN, ITP, TSO, FLAT,
     *                    FCOR, WW , WA, ALN, ALR, FP,
     *                    FSCORR   , FICORR  , ER1, ER2, 
     *                    FAIL, EPSILON, Z0, Z0T, ZU, ZT, LCORR, ECHEC,
     *                    dpRA, dpSN, npRC, dpRT, dpIR, dpSF, dpFV,
     *                    dpFC, dpFA, dpG, dpBB, dpFP)

      IMPLICIT NONE
      INTEGER i, j
***
*	$Revision: 1.20 $
*
*	$Date: 2005/11/22 19:55:17 $
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
*     FS: Flux solaire incident (W)
*     FI: Flux infra-rouge incident (W)
*     P0: Pression de surface (Pa)
*     TA: Temperature de l'air (C) au niveau ZT
*     QA: Humidite specifique (g/kg) au niveau ZT
*     VA: Vent (m/s) au niveau ZU
*     TYP: Type probable de precipitation ( 1 -> pluie, 2 -> neige )
*     FT: "Forecast time" (heures GMT)
*     PR: Taux de precipitation (m/s)
*     PVC: Etat de la chaussee (0 [sec] ou 1 [mouille])
*     iref: Nombre de niveau dans la grille
*     NTP: Indice de debut du couplage
*     NTP2: Indice de fin du couplage
*     CNT: Constantes de la grille
*     Z0: Longueur de rugosite (m)
*     Z0T: Longueur de rugosite (m)
*     ZU: Hauteur du niveau de la prevision de vent (m)
*     ZT: Hauteur du niveau de la prevision atmospherique (m)
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
***
      LOGICAL FLAT
      INTEGER iref, NTP, NTP2
      DOUBLE PRECISION  FS(DTMAX), FI(DTMAX), TA(DTMAX)
      DOUBLE PRECISION QA(DTMAX), PR(DTMAX)
      DOUBLE PRECISION VA(DTMAX), P0(DTMAX), FT(DTMAX)
      DOUBLE PRECISION TYP(DTMAX), PVC(DTMAX)
      DOUBLE PRECISION CNT(n,2), CNT_IN(2*n)
      DOUBLE PRECISION FCOR, WW(2), WA
      DOUBLE PRECISION ALN, ALR, TSO, FP, LCORR
      DOUBLE PRECISION EPSILON, ZU, ZT, Z0, Z0T
      DOUBLE PRECISION dpSN(DTMAX), dpRA(DTMAX)
      DOUBLE PRECISION dpIR(DTMAX), dpSF(DTMAX)
      DOUBLE PRECISION dpFC(DTMAX), dpFA(DTMAX)
      DOUBLE PRECISION dpG(DTMAX), dpBB(DTMAX)
      DOUBLE PRECISION dpRT(DTMAX), dpFV(DTMAX)
      DOUBLE PRECISION dpFP(DTMAX)
      INTEGER npRC(DTMAX)
      INTEGER nCheckBefore, nCheckAfter, n30SecondsStepIn3Hours
***
*     Entrees/Sorties
*     ---------------
*     ITP: Profil de temperature de la route (C, iref niveaux != 0)
***
      LOGICAL ECHEC
      DOUBLE PRECISION ITP(n)
***
*     Sorties
*     -------
*     FAIL: Indique si le couplage a echoue
*     TS: Serie temporelle de temperature de surface
*     FSCORR: Coefficient de couplage du flux solaire
*     FICORR: Coefficient de couplage du flux infra-rouge
*     ER1: Quantite d'eau sur la route (mm)
*     ER2: Equivalent en eau de la neige/glace sur la route (mm)
***
      LOGICAL FAIL
      DOUBLE PRECISION  FSCORR, FICORR, ER1, ER2
***
*     Internes
*     --------
***
      INTEGER ier, CHKDIV
      EXTERNAL CHKDIV
      CHARACTER*250 outfmt
      INTEGER next, now, iter
      LOGICAL DOWN
      REAL T(n,2), G(0:n)
      REAL RA, QG, RHO, TSK, AL, COUDUR, M
      REAL CL, PR1, PR2, DX, PRG, FZ
      REAL coeff1, coeff2, cotemp, COFS, COFI
***
*     Variables de FLXSURFZ
*     ---------------------
***
      REAL CMU, CTU, RIB,  ILMO, FTEMP, FVAP
      REAL H, UE, LZZ0, LZZ0T, FM, FH
***
*     Initialisation du common CLELOG
*     -------------------------------
***
***
*	$Revision: 1.20 $
*
*	$Date: 2005/11/22 19:55:17 $
***
***
***
*	$Revision: 1.20 $
*
*	$Date: 2005/11/22 19:55:17 $
***
*
      REAL  TCDK
      REAL CPD, CPV, RGASD, RGASV, TRPL, RAUW, EPS1
      REAL DELTA, CAPPA, TGL, CONSOL, GRAV, RAYT, STEFAN, PI
      REAL OMEGA, EPS2
      REAL KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD

      COMMON/CTSPHY/  CPD, CPV, RGASD, RGASV, TRPL, TCDK, RAUW,
     $                EPS1, EPS2, DELTA, CAPPA, TGL, CONSOL,
     $                GRAV, RAYT, STEFAN, PI, OMEGA,
     $                KNAMS, STLO, KARMAN, RIC, CHLC, CHLF, DG2RAD
*
***
*	$Revision: 1.20 $
*
*	$Date: 2005/11/22 19:55:17 $
***
*
      REAL TTT,PRS,QQQ,EEE,TVI,QST
      REAL FOEW,FODLE,FOQST,FODQS,FOEFQ,FOQFE,FOTVT,FOTTV,FOHR
      REAL FOEWA,FODLA,FOQSA,FODQA,FOHRA
*
      LOGICAL bSilent
      COMMON /SILENT/ bSilent

      INTRINSIC SIGN
***
*	$Revision: 1.20 $
*
*	$Date: 2005/11/22 19:55:17 $
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
      if( .not. bSilent) then
         WRITE(*,*) "DEBUT COUPLA"
      end if
*****
**     Conversion du array en matrice
      CALL ARRAY2MATRIXDOUBLEPRECISION(CNT_IN, CNT , n, 2)
**     Initialisation de parametres
**     ++++++++++++++++++++++++++++
      FAIL = .FALSE.
      DOWN = .FALSE.
      FZ = 0.1
      iter = 0
      coeff1 = 0.
      coeff2 = 1.
      ILMO = 1.
**     Claculer la duree du couplage
*      WRITE (*,*) "NTP", NTP
*      WRITE (*,*) "NTP2", NTP2
*      WRITE (*,*) "DT", DT
      COUDUR = real(NTP2-NTP+1)*DT/3.6e3
*     Initialisation de la variation d'albedo en fonction de la neige
*     ---------------------------------------------------------------
*     |                                                             |
*     |  Voir balanc.F pour la description de la fonction utilisee  |
*     |                                                             |
*     ---------------------------------------------------------------
      M = (ALN - ALR) / 5.
 11   next = 1
      now = 2
      H = 300.0
      ER1 = 0.0
      ER2 = 0.0
      iter = iter + 1
      G(0) = 0.0
*      WRITE(*,*) "iref", iref
      do j=1,iref
*         WRITE(*,*) "j,now,ITP(j)", j,now, REAL(ITP(j))
         T(j,now)=REAL(ITP(j))
         T(j,next)=REAL(ITP(j))
*         WRITE(*,*) "j,now,T(j,now)", j,now, T(j,now)
         G(j) = 0.
      end do
*      WRITE(*,*) "T(1,now)" , T(1, now)
************************************************
*    Criterium: Solar flux must be non zero
*     during the 3 hours BEFORE the roadcast and 
*     1 hour AFTER the begining of the roadcast.
      n30SecondsStepIn3Hours = 3*60*2
      if (NTP2 - n30SecondsStepIn3Hours > NTP) then
         nCheckBefore = NTP2-n30SecondsStepIn3Hours 
      else
         nCheckBefore = NTP
      end if
      if (NTP2 + n30SecondsStepIn3Hours/3 < DTMAX) then
         nCheckAfter = NTP2+n30SecondsStepIn3Hours/3
      else
         nCheckAfter = DTMAX
      end if
*      WRITE(*,*) FS(nCheckAfter)
*      WRITE(*,*) NTP2, n30SecondsStepIn3Hours/3
*      WRITE(*,*) '-----'
*     * FS(NTP2),  FS(nCheckAfter)

      if(FS(nCheckBefore) > 0.0 .and.
     *   FS(NTP2) > 100.0 .and. 
     *   FS(nCheckAfter) > 0.0) then
*      if ( FT(NTP2)+LCORR.ge.  6.0 .and.
*     *     FT(NTP2)+LCORR.lt. 18.0 .or.
*     *     FT(NTP2)+LCORR.lt. -6.0 .or.
*     *     FT(NTP2)+LCORR.gt. 30.0     ) then
*        Jour
*         WRITE(*,*) "Couplage corrigeant le flux solaire"
         COFI = 1.0
         COFS = coeff2
      else
*        Nuit
*         WRITE(*,*) "Couplage corrigeant le flux infrarouge"
         COFI = coeff2
         COFS = 1.0
      end if
*      WRITE(*,*) "coeff2", coeff2, COFI
*     +++++++++++++++++++++++++++++++++
*      Entree de la boucle de couplage
*     +++++++++++++++++++++++++++++++++
*      WRITE(*,*) "NTP, NTP2", NTP, NTP2
      do i = NTP+1,NTP2
*          WRITE(*,*) "i", i
*         WRITE(*,*) "VA", i, VA(i)
*      WRITE(*,*) "Dans do", FAIL
*        Ajustement des conditions de chaussee
*        +++++++++++++++++++++++++++++++++++++
*         WRITE(*,*) "PVC(i)", PVC(i)
         ER1 = PVC(i)*ER1
         ER2 = PVC(i)*ER2
*        Preparation des differents termes du bilan
*        ++++++++++++++++++++++++++++++++++++++++++
*        road temp in Kelvins         
         TSK = T(1,now) + TCDK
*         WRITE(*,*) "T(1,now)", 1,now, T(1,now)
*         WRITE(*,*) "TCDK", TCDK
*         WRITE(*,*) "TSK", TSK
*        radiation corps noir
         RA = EPSILON*STEFAN*TSK**4
*        humidite a la suface
*        WRITE(*,*) "Avant srfhum:", FAIL
*        WRITE(*,*) "FP", FP
         call SRFHUM  ( QG, CL, ER1, ER2, TSK, P0(i), QA(i), FP )
*        densite de l'air au sol
         ier = CHKDIV ( RGASD * FOTVT ( TSK , QG ), "coupla.ftn", 209 )
         if ( ier .eq. 1 ) then
            ECHEC = .true.
            return
         end if
         RHO = P0(i) / ( RGASD * FOTVT ( TSK , QG ) )
*         WRITE(*,*) "P0(i)", P0(i)
*         WRITE(*,*) "RGASD", RGASD
*         WRITE(*,*) "TSK", TSK
*         WRITE(*,*) "QG", QG
*         WRITE(*,*) "FOTVT", FOTVT(TSK,QG)
*        modification du vent
         call VENMIN  ( WW, FT(i), VA(i) )
*        energie utilisee/liberee par neige fondante/pluie verglacante
*         WRITE(*,*) "Avant verglas:", FAIL
         call VERGLAS ( TYP(i), T(1,now), FP, FZ, PR(i),
     *                  PR1, PR2, PRG, FAIL )
*         WRITE(*,*) "Apres verglas:", FAIL
*        coefficients des flux de chaleur
*         WRITE(*,*) "va(i)", VA(i)
         call FLXSURFZ( CMU  , CTU , RIB  , FTEMP, FVAP ,
     *                  ILMO, UE   , FCOR , TA(i)+TCDK,
     *                  QA(i), ZU, ZT  , VA(i), TSK  , QG   ,
     *                  H    , Z0  , Z0T  , LZZ0 , LZZ0T,
     *                  FM   , FH  ,  1,  1     )
*        albedo efficace
         AL = 1.0 - max( ALR,min( ALN,M*ER2+ALR-M ) )
*        Calcul du bilan energetique
*        +++++++++++++++++++++++++++
*         WRITE(*,*) "FS",I, FS(i)
*         WRITE(*,*) "FI",I, FI(i)
         G(0) = max(0.0,REAL(COFS*AL*FS(i))) - RA 
     *        + COFI*EPSILON*FI(i)
     *        + RHO*CTU*( CPD*(TA(i)-T(1,now))
     *        + CL*(QA(i)-QG)) + PRG + WA
*        Output information         
         dpIR(i) = COFI*EPSILON*FI(i)
         dpSF(i) = COFS*AL*FS(i)
         dpFV(i) = RHO*CTU*(CL*(QA(i)-QG))
         dpBB(i) = - RA
         dpFC(i) =  RHO*CTU*( CPD*(TA(i)-T(1,now)))
         dpFA(i) = WA
         dpFP(i) = PRG
         dpG(i) = G(0)
*         WRITE(*,*) "---------------------------------"
*          WRITE(*,*) max(0.0,REAL(COFS*AL*FS(i)))
*          WRITE(*,*) i, COFI,EPSILON, FI(i)
*          WRITE(*,*) RHO*CTU*( CPD*(TA(i)-T(1,now)))
*         WRITE(*,*) "QA(I)", QA(i)
*         WRITE(*,*) "QG", QG
*         WRITE(*,*) "(QA(i)-QG)" , (QA(i)-QG)
*         WRITE(*,*) "CL*(QA(i)-QG)", CL*(QA(i)-QG)
*          WRITE(*,*) PRG
*          WRITE(*,*) WA
*         WRITE(*,*) "i", I
*         WRITE(*,*) "COFS", COFS
*         WRITE(*,*) "AL", AL
*         WRITE(*,*) "FS(i)", FS(i)
*         WRITE(*,*) "RA", RA
*         WRITE(*,*) "EPSILON", EPSILON
*         WRITE(*,*) "FI(i)", FI(i)
*         WRITE(*,*) "COFI", COFI
*         WRITE(*,*) "RHO", RHO
*         WRITE(*,*) "CTU", CTU
*         WRITE(*,*) "CPD", CPD
*         WRITE(*,*) "TA(i)", TA(i)
*         WRITE(*,*) "CL", CL
*         WRITE(*,*) "QA(I)", QA(i)
*         WRITE(*,*) "QG", QG
*         WRITE(*,*) "PRG", PRG
*         WRITE(*,*) "WA", WA

         G(1) = REAL(CNT(1,1) * ( T(2,now) - T(1,now) ))
         T(1,next) = REAL(T(1,now)+DT*(CNT(1,2)*( G(1) - G(0)) ))

*         WRITE(*,*) "--------------"
*         WRITE(*,*) "T(1,now)",T(1,now)
*         WRITE(*,*) "T(2,now)",T(2,now)
*         WRITE(*,*) "CNT(1,1)",CNT(1,1)
*         WRITE(*,*) "( T(2,now) - T(1,now) )", ( T(2,now) - T(1,now) )
*         WRITE(*,*) "G(1)", G(1)
*         WRITE(*,*) "--------------"
*         WRITE(*,*) "G(0)", G(0)
*         WRITE(*,*) "G(1) - G(0)", G(1)-G(0)
*         WRITE(*,*) "CNT(1,2)",CNT(1,2)
*         WRITE(*,*) "DT*(CNT(1,2)*( G(1) - G(0)) ))",

*     *     DT*(CNT(1,2)*( G(1) - G(0)) )

*         WRITE(*,*) "i, T(1,next)", i, T(1,next)
*         WRITE(*,*) "DT", DT

*         WRITE(*,*) "TA", TA(I)
*
 

*         WRITE(*,*) "i, T(1,next)", i, T(1,next)
         DX = 0.0
*        Calculer l'evolution des temperatures dans le sol         
*        +++++++++++++++++++++++++++++++++++++++++++++++++
         call TSEVOL ( T, iref, now, CNT, G, FLAT, TA(i) )
*        Bilan des accumulations au sol
*        ++++++++++++++++++++++++++++++
*         WRITE(*,*) "ER2", ER2	
         call RODCON ( ER1, ER2, RHO  , CTU, CL , FP , FZ , 
     *                 T(1,now), QA(i), QG , PR1, PR2, PRG, DX , PR,
     *                 npRC(i))

         dpRT(i) = T(1,next)
         dpRA(i) = ER1
         dpSN(i) = ER2
*         WRITE(*,*) "ER", ER1, ER2
*        Inversion des indices
*        +++++++++++++++++++++
         next = 3 - next
         now = 3 - now
      end do
*      if ( VERBOSE ) then
         FSCORR = COFS - 1.0
         FICORR = COFI - 1.0
*         WRITE(*,*) "now, T(1,now)", now, T(1,now)
*         WRITE(*,*) "TSO", TSO
*         WRITE(*,*) "COUDUR", COUDUR
         write(outfmt,223) iter, COUDUR,
     *        min(99.99,max(-99.99,T(1,now))),
     *        min(99.99,max(-99.99,REAL(TSO))),COFI,COFS
*      end if
*     Determiner la necessite d'autres iterations
*     +++++++++++++++++++++++++++++++++++++++++++
*         WRITE(*,*) "FAIL, now", FAIL, now
*         WRITE(*,*) "T(1,now)",T(1,now), TSO
*         WRITE(*,*) "iter", iter
      if ( iter .eq. 25 ) then
         FICORR = 0.0
         FSCORR = 0.0
         FAIL = .true.
         WRITE(*,*) "Plus de 25 iterations dans le couplage"
      else if ( abs(min(99.99,max(-99.99,T(1,now)))) .eq. 99.99 .or.
     *        FAIL ) then
         FICORR = 0.0
         FSCORR = 0.0
         write(outfmt,223) iter, COUDUR,
     *        min(99.99,max(-99.99,T(1,now))),
     *        min(99.99,max(-99.99,REAL(TSO))),COFI,COFS
*         WRITE(*,*) "ICI"
         FAIL = .true.
      else if ( mod(iter,15) .eq. 0 .and.
     *        min(REAL(max(T(1,now),REAL(TSO-0.1))),REAL(TSO+0.1)) 
     *    .ne. T(1,now) ) then
         coeff1 = 0.0
         coeff2 = 1.5*coeff2
         go to 11
      else if ( T(1,now) - TSO .gt. 0.1 ) then
         coeff2 = coeff2 - 0.5*(coeff2-coeff1)
         DOWN = .true.
         go to 11
      else if ( TSO - T(1,now) .gt. 0.1 ) then
        if ( DOWN ) then
           cotemp = coeff1
           coeff1 = coeff2
           coeff2 = coeff2 + 0.5*(coeff2-cotemp)
         else
            cotemp = coeff1
            coeff1 = coeff2
            coeff2 = 2.0*coeff2 - cotemp
         end if
         go to 11
      else
         FSCORR = COFS - 1.0
         FICORR = COFI - 1.0
         FAIL = .false.
         do j=1,iref
            ITP(j) = T(j,now)
         end do
      end if
 223  format(1x,i3,3x,f5.2,3x,f6.2,3x,f6.2,f9.5,f9.5)

**     Conversion de la matrice en array
      CALL MATRIX2ARRAYDOUBLEPRECISION(CNT, CNT_IN, n, 2)

      if( .not. bSilent) then
         WRITE(*,*) "FIN COUPLA"
      end if
*      CLOSE(11)
      RETURN
      END
*
*
