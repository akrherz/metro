********************************************************************************
* Author: Miguel Tremblay
* Date: April 15 2004
* Description: Program who test the creation of a matrix with an array.
*  integer, double precision and char are made.
*  Tools to transpose a matrix are also made, because C write information in 
*  a line fashion and fortran in column.
********************************************************************************



***************************************************************************
* Create a matrix with an array.  The arguments are the number of lines
*  and columns that must be created in the matrix.
*************************************************************************
      SUBROUTINE ARRAY2MATRIXINT(npInput, nppOutput, nLine, nColumn )
      IMPLICIT NONE
      INTEGER nLine, nColumn
      INTEGER npInput(0:nLine*nColumn-1) 
      INTEGER nppOutput(0:nLine-1,0:nColumn-1) 
      INTEGER  i,j


*      WRITE(*,*) "line", nLine
*      WRITE(*,*) "column", nColumn
*      nppOutput(0,3) = 1
      DO i=0, nLine-1
         DO j=0, nColumn-1
*            WRITE(*,*) i,j,i*nColumn + j, npInput(i*nColumn + j)
            nppOutput(i,j) = npInput(i*nColumn + j)
*            WRITE(*,*)  nppOutput(i,j) 
*            WRITE(*,*) "-------"
         END DO
      END DO

      RETURN
      END SUBROUTINE ARRAY2MATRIXINT

***************************************************************************
* Transforme une matrice de double en un array de double.
*************************************************************************
      SUBROUTINE MATRIX2ARRAYDOUBLEPRECISION(dppInput, dpOutput, nLine, 
     *     nColumn)
      IMPLICIT NONE
      INTEGER nLine, nColumn
      DOUBLE PRECISION dppInput(0:nLine-1, 0:nColumn-1) 
      DOUBLE PRECISION dpOutput(0:nLine*nColumn-1) 

      INTEGER i,j

      DO i=0, nLine-1
         DO j=0, nColumn-1
            dpOutput(i*nColumn+j)=dppInput(i,j)
         END DO
      END DO

      RETURN
      END SUBROUTINE MATRIX2ARRAYDOUBLEPRECISION


***************************************************************************
* Transforme une matrice de integer en un array de double.
*************************************************************************
      SUBROUTINE MATRIX2ARRAYINT(nppInput, npOutput, nLine, nColumn)
      IMPLICIT NONE
      INTEGER  nLine, nColumn
      INTEGER nppInput(0:nLine-1, 0:nColumn-1) 
      INTEGER npOutput(0:nLine*nColumn-1) 
      INTEGER i,j

      DO i=0, nLine-1
         DO j=0, nColumn-1
            npOutput(i*nColumn+j)=nppInput(i,j)
         END DO
      END DO

      RETURN
      END SUBROUTINE MATRIX2ARRAYINT

***************************************************************************
* Create a matrix with an array.  The arguments are the number of lines
*  and columns that must be created in the matrix.
*************************************************************************
      SUBROUTINE ARRAY2MATRIXDOUBLEPRECISION(dpInput, dppOutput, nLine,
     *     nColumn)
      IMPLICIT NONE
      INTEGER nLine, nColumn
      DOUBLE PRECISION dpInput(0:nLine*nColumn-1)
      DOUBLE PRECISION dppOutput(0:nLine-1,0:nColumn-1) 

      INTEGER i,j

      DO i=0, nLine-1
         DO j=0, nColumn-1
            dppOutput(i,j) = dpInput(i*nColumn + j)
         END DO
      END DO

      RETURN
      END SUBROUTINE ARRAY2MATRIXDOUBLEPRECISION

***************************************************************************
* Create a matrix with an array.  The arguments are the number of lines
*  and columns that must be created in the matrix. The first line is printed.
*************************************************************************
      SUBROUTINE MIGARRAY2MATRIXDOUBLEPRECISION(dpInput, dppOutput, 
     *     nLine, nColumn)
      IMPLICIT NONE
      INTEGER nLine, nColumn
      DOUBLE PRECISION dpInput(0:nLine*nColumn-1) 
      DOUBLE PRECISION dppOutput(0:nLine-1,0:nColumn-1) 

      INTEGER  i,j

      DO i=0, nLine-1
         DO j=0, nColumn-1
            dppOutput(i,j) = dpInput(i*nColumn + j)
         END DO
      END DO

      RETURN
      END SUBROUTINE MIGARRAY2MATRIXDOUBLEPRECISION

*************************************************************************
* Transpose the column and line of the matrix.
*************************************************************************
      SUBROUTINE REVERTINT(nppInput, nLine, nColumn, nppOutput)
      IMPLICIT NONE
      INTEGER nLine, nColumn
      INTEGER nppInput(0:nLine-1,0:nColumn-1) 
      INTEGER nppOutput(0:nLine-1,0:nColumn-1) 
      INTEGER i,j
      INTEGER  nNewLine, nNewColumn
      INTEGER nTotalElement

      nTotalElement = 1
      DO i=0, nColumn-1
         DO j=0, nLine-1      
            nNewLine = (nTotalElement-1)/nColumn
            nNewColumn = MOD(nTotalElement, nColumn)-1
            IF(nNewColumn == -1) THEN
               nNewColumn = nColumn-1;
            END IF
            nppOutput(nNewLine, nNewColumn) = nppInput(j,i)
            nTotalElement = nTotalElement + 1
         END DO
      END DO

      RETURN
      END SUBROUTINE REVERTINT
