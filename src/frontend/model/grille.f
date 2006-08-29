***
*	$Revision: 1.6 $
*
*	$Date: 2005/03/16 21:04:23 $
***
***
*     Sous-routine GRILLE: sert a creer le maillage pour le modele de 
*                          conduction thermique du sol.
*
*     Auteur: Louis-Philippe Crevier
*     Date: Juin 1999
*     Adaptation au C et au Fortran95: Miguel Tremblay
*     Date: 26 avril 2004
***
      SUBROUTINE GRILLE ( GRI_IN, CNT_IN, iref, ir40, 
     *     FLAT, NZONE, ZONES, MAT, DIFF, ECHEC )
      IMPLICIT NONE
      INTEGER i, j, k
***
*	$Revision: 1.6 $
*
*	$Date: 2005/03/16 21:04:23 $
***
      INTEGER Nl, n
      REAL DT
      INTEGER DTMAX, NRECMAX, NCOLMAX
      COMMON /BUFFER_SIZE/ DTMAX, Nl, DT, NRECMAX, NCOLMAX, n

***               ***
*     DEFINITIONS   *
***               ***
***
*     Entrees
*     -------
*     FLAT: switch pont / route
*     MAT: Type de materiau dans chaque zone
*     NZONE: nombre de zones
*     ZONES: limites des differentes zones
***
      LOGICAL FLAT, ECHEC
      INTEGER NZONE, MAT(20)
      DOUBLE PRECISION ZONES(20), CS(20), KS(20)
***
*     Sorties
*     -------
*     iref: indice du dernier niveau utilisee par le modele
*     ir40: indice du niveau le plus pres de 40 cm.
*     GRI: profondeur des differents niveaux utilises
*     CNT: constantes utilisees pour les calculs de conduction
*     DIFF: Vecteur utilise pour creer les profiles initiaux de temperature
***
      INTEGER iref, ir40
      DOUBLE PRECISION GRI(n,2), CNT(n,2), DIFF
      DOUBLE PRECISION GRI_IN(n*2)
      DOUBLE PRECISION CNT_IN(n*2)
***
*     Internes
*     --------
*     CS: capacite du sol, par zone
*     KS: conductivite du sol, par zone
*     CC: parametre de grille ( FLAT = .f. )
*     INTER: indice du niveau present
*     YPG, YPT: derivees 1ere et 2eme de GRI
*     DY: epaisseur des niveaux ( apres transformation )
*     C: CS transpose sur GRI
*     Ko: KS transpose sur GRI
*     dd: parametre de la GRI ( FLAT = .f. )
*     ratio: ratio de transition lors transfert CS -> C et KS -> Ko
***
      INTEGER INTER, NMAX
      REAL YPG(n), YPT(n), C(n), Ko(n)
      REAL dd, ratio, CC, DY

      LOGICAL bSilent
      COMMON /SILENT/ bSilent
***
*	$Revision: 1.6 $
*
*	$Date: 2005/03/16 21:04:23 $
***

***
*     Declarations des constantes physique 
*     ------------------------------------
***
***
*	$Revision: 1.6 $
*
*	$Date: 2005/03/16 21:04:23 $
***

***
*
*     Procedure
*     =========

      if( .not. bSilent) then
         WRITE(*,*) "DEBUT DE GRILLE"
      end if

      DO k=1,2
         DO j=1,n
            CNT(j,k) = 0.
         END DO
      END DO
*     Associer les conductivites/capacites aux differentes 
*     couches de materiaux
*     ----------------------------------------------------
      NMAX = NZONE
      DO k=1,NZONE
         IF ( ZONES(NZONE + 1 - k) .ge. 0.40 ) NMAX = NZONE + 1 - k
         IF ( MAT(k) .eq. 1 ) THEN
*           asphalt(e)
*           ----------
            CS(k) = 2.10e6
            KS(k) = 0.8
         ELSE IF ( MAT(k) .eq. 2 ) THEN
*           gravier / crushed rock
*           ----------------------
            CS(k) = 2.10e6
            KS(k) = 0.95
         ELSE IF ( MAT(k) .eq. 3 ) THEN
*           beton / concrete
*           ----------------
            CS(k) = 2.10e6
            KS(k) = 2.2
         ELSE IF ( MAT(k) .eq. 4 ) THEN
*           sous-sol (sable) / deep soil (sable)
*           ------------------------------------
            CS(k) = 2.0e6
            KS(k) = 1.0
         ELSE
            ECHEC = .true.
            return
         END IF
      END DO
      DIFF = (MIN(0.4, REAL(ZONES(1)))/0.4)*KS(1)/CS(1)
*  WRITE(*,*) "ZONES(1)", ZONES(1)
*  WRITE(*,*) "DIFF", DIFF
*  WRITE(*,*) "KS(1)", KS(1)
*  WRITE(*,*) "CS(1)", CS(1)

      DO i=2,NMAX
         DIFF = ((MIN(0.4,REAL(ZONES(i)))-ZONES(i-1))/0.4)* 
     *        KS(i)/CS(i) + DIFF
      END DO
*     Creer la grille elle-meme et ses derivees
*     -----------------------------------------
      IF ( FLAT ) THEN
*        Cas FLAT = .true. => PONT
*        -------------------------
         DY = max( 0.01 , REAL(ZONES(NZONE)) / real(n) )
         iref = int( ZONES(NZONE) / DY )
*     WRITE(*,*) iref
*     WRITE(*,*) ZONES(NZONE),DY
         DO j=1,iref
*       Grille des niveaux de flux
            GRI(j,1) = j * DY
*       Grille des niveaux de temperatures
            GRI(j,2) = ( real(j)-0.5 ) * DY
*        WRITE(*,*) "GRILLE", GRI(j,1), GRI(j,2)
*       Derivee sur les niveaux de flux
            YPG(j) = 1.0
*       Derivee sur les niveaux de temperature
            YPT(j) = 1.0
            IF ( GRI(j,2) .le. 0.4 ) ir40 = j
         END DO
      ELSE
*    Cas FLAT = .false. => ROUTE
*    ---------------------------
         CC = 3.6
         dd = 20.0
         ir40=int(0.5+((1-exp(-(CC*0.4)))/(1-exp(-(CC*0.01)))))
         DY = dd * (1-exp(-(CC*0.4)))/(real(ir40)-0.5)
         j=1
 12      IF ( real(j)*DY/dd .ge. 1.0 ) THEN
            iref = j - 1
         ELSE
            j = j + 1
            go to 12
         END IF
         GRI(1,1) = - (log(( 1 - ( DY / dd ) )) / CC)
         GRI(1,2) = 0.5 * GRI(1,1)
*     WRITE(*,*) "GRILLE_init", GRI(1,1), GRI(1,2)
         j=1
         DO j=2,iref
            GRI(j,1) = - (log((1. -(real(j) * DY / dd))) / CC)
            GRI(j,2) = 0.5 * ( GRI(j,1) + GRI(j-1,1) )
*     WRITE(*,*) "GRILLE", GRI(j,1), GRI(j,2)
         END DO
         YPG(1) = dd * CC * exp( - (CC * 0.5 * GRI(1,1) ))
         YPT(1) = dd * CC * exp( -(CC * 0.5 * GRI(1,2) ))
         DO j=2,iref
            YPG(j) = dd * CC * exp( - (CC * GRI(j,1) ))
            YPT(j) = dd * CC * exp( - (CC * GRI(j,2) ))
         END DO
      END IF

*     Associer les conductivites/capacites aux niveaux de la grille
*     -------------------------------------------------------------
      INTER = 1
      DO j=1,iref
         IF ( GRI(j,1) .ge. ZONES(INTER) ) THEN
            ratio = ( ZONES(INTER) - GRI(j-1,1) ) / 
     *           ( GRI(j,1) - GRI(j-1,1) )
            C(j) = CS(INTER)*ratio + CS(INTER+1)*(1-ratio)
            INTER = INTER + 1
         ELSE
            C(j) = CS(INTER)
         END IF
      END DO
      INTER = 1
      DO j=1,iref
         IF ( GRI(j,2) .ge. ZONES(INTER) ) THEN
            ratio = ( ZONES(INTER) - GRI(j-1,2) ) / 
     *                 ( GRI(j,2) - GRI(j-1,2) )
            Ko(j) = KS(INTER)*ratio + KS(INTER+1)*(1-ratio)
            INTER = INTER + 1
         ELSE
            Ko(j) = KS(INTER)
         END IF
      END DO
*     Creation de CNT qui contient les contributions des capacites et
*     conductivites en plus des facteurs de la metrique utilisee
*     ---------------------------------------------------------------
*  WRITE(*,*) "Init de CNT"
      CNT(1,1) = - (Ko(1) * YPG(1) / DY)
      CNT(1,2) = - (YPT(1) / ( DY*C(1) ))
*  WRITE(*,*) iref
      DO j=2,iref
         CNT(j,1)= - (Ko(j) * YPG(j) / DY)
         CNT(j,2)= - (YPT(j) / ( DY*C(j) ))
      END DO
      ECHEC = .false.

* Mettre la matrice dans l'array pour l'output
      CALL MATRIX2ARRAYDOUBLEPRECISION(GRI,GRI_IN, n, 2)
      CALL MATRIX2ARRAYDOUBLEPRECISION(CNT, CNT_IN, n, 2)

      if( .not. bSilent) then
         WRITE(*,*) "FIN DE GRILLE"      
      end if
      

      RETURN
      END SUBROUTINE GRILLE
