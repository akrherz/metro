***
*	$Revision: 1.10 $
*
*	$Date: 2005/03/16 21:04:23 $
***
***
*     Sous-routine INITIAL: effectue l'initialisation du profile de 
*                           temperature de la route a partir des donnees
*                           de temperature a la surface, a 40 cm et la 
*                           temperature de l'air.
*
*     Auteur / Author: Louis-Philippe Crevier
*     Date: Juillet 1999 / July 1999
***
      SUBROUTINE INITIAL ( ITP, TSO, TUO, TAO, DEB, FIN, CNT_IN,
     *                       iref, ir40, FLAT, SWO_IN)

      IMPLICIT NONE
      INTEGER i, j
***
*	$Revision: 1.10 $
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
*     iref: nombre de niveau de la grille
*     ir40: niveau situe a 40 cm de profondeur
*     DEB: Indice de debut de l'initialisation
*     FIN: Indice de fin de l'initialisation
*     TSO, TUO, TAO: forcage ( surface, 40 cm, air [sous le pont] )
*     SWO: serie temporelle indiquant les trous dans les observations
***
      LOGICAL FLAT
      INTEGER iref, ir40, DEB, FIN
      INTEGER SWO(Nl,4), SWO_IN(4*Nl)
      DOUBLE PRECISION CNT(n,2), CNT_IN(2*n)
      DOUBLE PRECISION TSO(Nl)
      DOUBLE PRECISION TUO(Nl), TAO(Nl)
***
*     Sorties
*     -------
*     ITP: profil de temperature initial
***
      DOUBLE PRECISION ITP(n)
***
*     Internes
*     --------
*     now, next: reference au pas de temp
*     T: profil de temperature
*     G: profil des flux
*     DTDZ: gradient de temperature initial
***
      CHARACTER*80 outfmt
      INTEGER next, now
      REAL G(0:n), T(n,2)

***
*	$Revision: 1.10 $
*
*	$Date: 2005/03/16 21:04:23 $
***  
      LOGICAL bSilent
      COMMON /SILENT/ bSilent


***
*
*     Procedure
*     =========
      if( .not. bSilent) then
         WRITE(*,*) "DEBUT INITIAL"
      end if


*      WRITE(*,*) "SWO", SWO_IN(1)
*      WRITE(*,*) "ir40", ir40
*      WRITE(*,*) "FLAT", FLAT
      CALL ARRAY2MATRIXINT(SWO_IN, SWO, Nl, 4)
*      WRITE(*,*) "1"
      CALL ARRAY2MATRIXDOUBLEPRECISION(CNT_IN, CNT, n, 2)      
      next = 1
      now = 2
*      WRITE(*,*) "AVANT "
      do j=1,n
         T(j,now) = ITP(j)
      end do
*      WRITE(*,*) "DEB, FIN", DEB, FIN
      do i = DEB,FIN
         G(0) = 0.0
         G(1) = CNT(1,1) * ( T(2,now) - T(1,now) )
         T(1,next) = TSO(i)
*         WRITE(*,*) "TSO(i)", i, TSO(I)
         do j=2,iref-1
            G(j) = CNT(j,1) * ( T(j+1,now) - T(j,now) )
         end do
         do j=2,iref-1
            T(j,next) = T(j,now)+DT*(CNT(j,2)*( G(j) - G(j-1 )))
         end do
         if ( SWO(i,1) .eq. 1 ) then
*            WRITE(*,*) 'tuo', TUO(i)
            T(ir40,next) = TUO(i)
         end if
         if ( FLAT .and. SWO(i,2) .eq. 1 ) then
*        BC: underside temp. is air temp
            T(iref,next) = TAO(i)
         else
*        BC: no flux ( G(iref) = 0.0 )
            T(iref,next) = T(j,now) - DT*CNT(j,2)*G(iref-1)
         end if
         next = 3 - next
         now = 3 - now
      end do
*      WRITE(*,*) "APRES"
      do i=1,iref
         ITP(i) = T(i,now)
      end do
*      WRITE(*,*) "ITP(1)", ITP(1)
*     Ecriture au journal
*     -------------------
      write(outfmt,667) min(99.99,max(-99.99,REAL(TSO(FIN)))),' / ',
     *                  real(FIN-DEB)*DT/3.6e3
 667  format(f6.2,a,f6.2)

      if( .not. bSilent) then
         WRITE(*,*) "FIN INITIAL"
      end if

      return
      end
