
************************************************************************
************************************************************************
***
*     Sous-routine MAKITP: Sous-routine pour generer un profil analytique
*                          de temperature de la route, a un instant donne
*
*     Auteur/Author: Louis-Philippe Crevier
*     Date: Decembre 1999 / December 1999
***
      SUBROUTINE MAKITP (ITP, iref, ir40, FLAT, FT, TS, 
     *                   TU, TA, DIFF, LON, GRI_IN, SWO_IN )
      IMPLICIT NONE
      INTEGER j
***
*	$Revision: 1.15 $
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
*     FLAT: switch pont / route
*     iref: indice du dernier niveau utilisee par le modele
*     ir40: indice du niveau le plus pres de 40 cm.
*     GRI: profondeur des differents niveaux utilises
*     SWO: Serie temporelle de validite des observations (0 ou 1)
*     FT: Forecast time
*     TS: Temperature de surface de la route
*     TU: Temperature a 40 cm
*     TA: Temperature de l'air
*     LON: Longitude de la station
*     DIFF: Vecteur utilise pour creer les profiles initiaux de temperature
***
      LOGICAL FLAT
      DOUBLE PRECISION FT, TS, TU, TA, DIFF, LON
      DOUBLE PRECISION GRI_IN(2*n), GRI(n,2)
      INTEGER iref, ir40, SWO_IN(Nl*4), SWO(Nl,4)

***
*     Sorties
*     -------
*     ITP: Profil de temperature
***
      DOUBLE PRECISION ITP(n)
***
*     Internes
*     --------
***
      REAL ASURF, ABOTT, B, C, K, E, Ew

***
*	$Revision: 1.15 $
*
*	$Date: 2005/03/16 21:04:23 $
***

***
*     Definition des constantes physiques
***
***
*	$Revision: 1.15 $
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

      LOGICAL bSilent
      COMMON /SILENT/ bSilent

*
**
***
*
*     Procedure
*     ---------
      if( .not. bSilent) then
         WRITE(*,*) "DEBUT MAKTIP"
      end if

*     Conversion du array en matrice
      CALL ARRAY2MATRIXINT(SWO_IN, SWO, Nl, 4)
      CALL ARRAY2MATRIXDOUBLEPRECISION(GRI_IN, GRI, n, 2)
*      WRITE(*,*) "DIFF", DIFF
*      do i=1,n/2
*         WRITE(*,*) "GRI",i, GRI(i,1), GRI(i,2)
*      end do
*      write(*,*) "LON", LON
*      write(*,*) "PI", PI
*      write(*,*) "FT", FT
      FT = FT*3600.0
      ASURF = 7.5
      ABOTT = 3.75
      C = LON*PI/180.0
      K = SQRT( OMEGA / DIFF )
*      write(*,*) "K",K
      B = TS - EXP((-K)*GRI(1,2))*ASURF*SIN(OMEGA*FT - K*GRI(1,2) + C)
*      write(*,*) "FT", FT
*      write(*,*) "LON, PI", LON, PI
*      write(*,*) "C", C
*      write(*,*) "DIFF", DIFF
*      write(*,*) "K",K
*      write(*,*) "B", B
*      write(*,*) "TS", TS      
      if ( SWO(1,1) .eq. 1 ) then
         E = TU - EXP((-K)*GRI(ir40,2))*
     *     ASURF*SIN(OMEGA*FT - K*GRI(ir40,2) + C) - B
      else
         E = 0.0
      end if
*      WRITE(*,*) "ir40, iref", ir40, iref
      do j=1,ir40
         Ew = E*(GRI(j,2)-GRI(1,2))/(GRI(ir40,2)-GRI(1,2))
         ITP(j) = REAL(B + Ew + EXP((-K)*GRI(j,2))*
     *        ASURF*SIN(OMEGA*FT -K*GRI(j,2)+C))
*         WRITE(*,*) "ITP(j)", j, REAL(ITP(j))
*         write(*,*) "B", B
*         write(*,*) "Ew", Ew
*         write(*,*) "GRI(J,2), GRI(1,2)",GRI(J,2), GRI(1,2)
*         write(*,*) "GRI(IR40,2)"
*         write(*,*) "K", K
*         write(*,*) "ASURF", ASURF
*         write(*,*) "OMEGA, FT",OMEGA,FT
*         WRITE(*,*) "C",C
      end do
      if ( FLAT .and. SWO(1,2) .eq. 1) then
         do j=ir40+1,iref
            Ew = (TA - ITP(ir40))/(GRI(iref,2)-GRI(ir40,2))
            ITP(j) = ITP(ir40) + (GRI(j,2) - GRI(ir40,2))*Ew
         end do
      else
         do j=ir40+1,iref
            ITP(j) = B + E +
     *           EXP((-K)*GRI(j,2))*ASURF*SIN(OMEGA*FT-K*GRI(j,2)+C)
         end do
      end if
*     Conversion de la matrice en array
      CALL MATRIX2ARRAYDOUBLEPRECISION(GRI, GRI_IN, n, 2)

      if( .not. bSilent) then
         WRITE(*,*) "FIN MAKTIP"
      end if

      return
      end

************************************************************************
************************************************************************
************************************************************************
***
*     Sous-routine CHKDIV: Verifier qu'un denominateur est different 
*                          de zero.
*
*     Auteur/Author: Louis-Philippe Crevier
*     Date: Octobre 2000
***
      FUNCTION CHKDIV ( DIV, FICH, LIGNE )
      IMPLICIT NONE
      INTEGER CHKDIV
      INTEGER a
      CHARACTER*(*) FICH, LIGNE
      DOUBLE PRECISION DIV
      if ( DIV .eq. 0.0 ) then
         a=index(FICH,' ')-1
         FICH=FICH(1:a)//', '//LIGNE
         CHKDIV = 1
      else
         CHKDIV = 0
      end if
      return
      end
