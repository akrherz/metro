/*************************************************************************
* METRo : Model of the Environment and Temperature of Roads
* METRo is Free and is proudly provided by the Government of Canada
* Copyright (C) 2006 Environment Canada

*  Questions or bugs report: metro@ec.gc.ca 
*  METRo repository: https://gna.org/projects/metro/ 
*  Documentation: http://documentation.wikia.com/wiki/METRo 
* 
* 
* Code contributed by:
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
*****************************************************************************/ 

/***************************************************************************
**
** Nom:         macadam.c
**
** Auteur:      Miguel Tremblay
**
** Date:        April 16, 2004
**
** Description: Fichier qui gère le modèle de METRo lui-même.
**  Toutes les routines fortran doivent être appelé par ce fichier.  Le 
**  séquençage du modèle se fait par ce fichier.
**
** 
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "macadam.h"
#define f77name(x) x##_


/* Constante pour la taille des string pour la compatibilite avec fortran */
#define nNBROFSTRING 50
#define nNAMELENGTH 100
#define nNAMELENGTHLONG 150
#define nNBRARGS 27


/* Those variables are declared here because we have to get them in python */
/* This cannot be perform unless you retrieve one pointer as a return value */
static struct doubleStruct stRA; /* Liquid accumlation */
static struct doubleStruct stSN; /* Snow/ice acculation */
static struct longStruct stRC; /* Road condition */
static struct doubleStruct stRT; /* Road temperature */
static struct doubleStruct stIR; /* Infra-red flux */
static struct doubleStruct stSF; /* Solar flux */
static struct doubleStruct stFV; /* Vapor flux */
static struct doubleStruct stFC; /* Sensible head */
static struct doubleStruct stFA; /* Anthropogenic flux */
static struct doubleStruct stG; /* Ground flux */
static struct doubleStruct stBB; /* Black body radiation */
static struct doubleStruct stFP; /* Phase change energy */

/****************************************************************************
 Name: Do_Metro
 
 Parameters:  
[I BOOL bFlat : road (FALSE) or bridge (TRUE)]
[I double dMLat : Latitude de la station meteo]
[I double dMLong : Longitude de la station meteo]
[I double dLCorr : Nombre d'heure entre le fuseau horaire et UTC]
[I double* dpZones : profondeur en metre de chacune des couches de la route]
[I long nNbrOfZone : nombre de couches composant la route]
[I long* npMateriau : code indiquant la composition de la couche de la route.
   voir http://wikid.cmc.ec.gc.ca/tiki-index.php?page=Type+de+Surface]
[I double* dpTA : interpolated air temperature]
[I double* dpQP : interpolated quantity of precipitation]
[I double* dpFF : interpolated wind velocity]
[I double* dpPS : interpolated surface pressure]
[I double* dpFS : interpolated solar flux]
[I double* dpFI : interpolated visible flux]
[I double* TYP : Type of precipitation: 0 = nada, 1= liquid, 2=solid]
[I double* dpRC : interpolated road condition. 0 = dry, 1=wet]
[I double dpTAO : interpolated observed air temperature]
[I double* dpRTO : interpolated observed road temperature]
[I double* dpDTO : interpolated observed deep road temperature]
[I double* dpTimeO : steps of 30 seconds for the observation]
[I long* npSWO1 : Boolean field to check if the deep road temperature 
    passed the QA/QC]
[I long* npSWO2 : Boolean field to check if the air temperature 
    passed the QA/QC]
[I long* npSWO3 : Boolean field to check if the dew point 
    passed the QA/QC]
[I long* npSWO4 : Boolean field to check if the wind speed
    passed the QA/QC]
[I BOOL* bpNoObs : boolean field to tell the number of observation used]
[I double dDeltaT : Time diffence, in hours, between the first observation 
  and the start of METRo.]
[I long nLenObservation : Number of valid observations.  30 seconds steps.]
[I long nNbrTimeSteps : number of 30 seconds steps in the forecast]

Returns: None

Functions Called: 
Those are fortran functions called with f77name.  Only the function
balanc and grille should remains here at the release of macadam.
grille : Création de la grille pour le modèle.
makitp : Création d'une température analytique pour la température de la route.
initial : Initilisation du profil pour la température de la route.
coupla : Couplage.
balanc : Prévision du modèle.

<function description>

Description:
This is part of the module "metro_model.py".  This C function make the forecast
 for the METRo software

Notes:

Revision History:

Author		Date		Reason
Miguel Tremblay  mai 2004     
 
***************************************************************************/

void Do_Metro( BOOL bFlat, double dMLat, double dMLon,  double dLCorr, double* dpZones, long nNbrOfZone,  long* npMateriau, double* dpTA, double* dpQP, double* dpFF,  double* dpPS, double* dpFS, double* dpFI, double* dpFT, double* dpTYP, double* dpRC, double* dpTAO,  double* dpRTO, double* dpDTO, double* dpAH, double* dpTimeO, long* npSwo, BOOL* bpNoObs, double dDeltaT, long nLenObservation, long nNbrTimeSteps, BOOL bSilent)
{
  /* Path des fichiers utilises */
  char* cpNRep; 

  /* Argument de la ligne de commande. Donne par python  */

/**     Un "O" a la fin du nom de la variable indique qu'elle provient 
  **     des observations locales/ An "O" at the end of a variable name 
  **     indicates it comes from the local observation
  **
  **     Ex: TA, TAO (observations)
  **
  **     TA: temperature de l'air / air temperature
  **     TD: point de rosee / dew point temperature
  **     VA: vitesse du vent / wind speed
  **     DD: direction du vent / wind direction
  **     FS : solar flux solaire
  **     FI : infra-red flux infra-rouge
  **     AC: accumulations
  **     TYP: type de precipitation type
  **     P0 : pression a la surface / surface pressure
  **     GMT: Variable contenant l'heure "reelle"
  **     DT: Temperature sous la surface
  ******/
  BOOL bFail = FALSE;
  BOOL bSucces = TRUE;
  BOOL bEchec;
  long nNtp;
  long nNtp2;
  long nNRec;
  long nNtdcl;
  double* dpItp;
  double dDiff;
  double* dpWw;
  double dWa = 10.0;
  double dAln = 0.5;
  double dAlr = 0.1;
  double dEpsilon = 0.92;
  double dZ0 = 0.001;
  double dZ0t = 0.0005;
  double dZt = 1.5;
  double dZu = 10;
  double dFCorr; 
  double dFsCorr=0;
  double dFiCorr=0;
  double dEr1=0;
  double dEr2=0;
  double dFp=0.0;
  /* Valeur pour la grille */
  long nIRef=0;
  long nIR40;
  double* dpCnt;
  double* dpGri;
  long i;
  long nDeltaTIndice=0;

  /* Allocate memory for all structures */
  init_structure(nDTMAX);


  for (i=0; i<nNbrTimeSteps; i++){
    stIR.pdArray[i] = dpFI[i];
  }

  /* double */ 
  dpItp = (double*)malloc((nNGRILLEMAX)*sizeof(double));
  dpWw = (double*)calloc((2),sizeof(double));
  dpCnt = (double*)calloc((2*nNGRILLEMAX),sizeof(double));
  dpGri = (double*)malloc((2*nNGRILLEMAX)*sizeof(double));
  
   /* Initilisation des constantes physique dans le code fortran */ 
  f77name(setconstphys)(&bSilent);

  /******************************* Station ********************************/

  bEchec = FALSE;
  dFCorr = 2.0*dOMEGA*sin(dPI*dMLat/180.0); 

  if(bFlat){
    nNbrOfZone = 1;
  }
  else{/* Faire attention ici.  Dans le cas d'une route, on ajoute une couche 
           de 20 mètres de type 4 (sable).*/
    dpZones[nNbrOfZone] = 20.0;
    npMateriau[nNbrOfZone]= 4;
    nNbrOfZone = nNbrOfZone +1;
  }

  /* Creation de la grille */
  f77name(grille)(dpGri, dpCnt, &nIRef, &nIR40, &bFlat, &nNbrOfZone, dpZones, npMateriau, &dDiff, &bEchec); 
  if(FALSE){
    bSucces = FALSE;
  }

  /* Extraction des observations */
  /*  TODO MT: those -1 is because it is use in fortran */
  nDeltaTIndice = (dDeltaT)*3600/30.-1;

  nLenObservation = nLenObservation -1;

   
  /***********************************************************************/
  /*           Couplage de la prevision et des observations.
  /*           Differentes possibilites selon la quantite d'observations
  /*           presentes.
  /**********************************************************************/
  bFail = FALSE;
  if(bpNoObs[2]){
    goto liberation;
  }
  else if(bpNoObs[3]){
    BOOL bFalse = FALSE;
    if(!bSilent)
      printf(" Une seule observation valide: Pas d'init et couplage.\n");
    f77name(makitp)(dpItp, &nIRef, &nIR40, &bFlat, &(dpTimeO[0]), &(dpRTO[0]), &(dpDTO[0]), &(dpTAO[0]), &dDiff, &dMLon, dpGri, npSwo);
    nNtp2 = nLenObservation - nDeltaTIndice;
   }
  else if(bpNoObs[1]){
    /* moins de trois heures d'observation dans le couplage */
    BOOL bFalse = FALSE;
    long nOne =1;
    if(!bSilent)
      printf(" Pas assez de donnees pour le couplag)e.\n");
    f77name(makitp)(dpItp, &nIRef, &nIR40, &bFlat, &(dpTimeO[0]), &(dpRTO[0]), &(dpDTO[0]), &(dpTAO[0]), &dDiff, &dMLon, dpGri, npSwo); 
    f77name(initial)(dpItp , (dpRTO+1), (dpDTO+1), (dpTAO+1), &nOne, &nLenObservation, dpCnt, &nIRef, &nIR40, &bFlat, npSwo); 
    nNtp2 = nLenObservation - nDeltaTIndice;
  }
  else if(bpNoObs[0]){
    BOOL bTmp = FALSE;    
    if(!bSilent)
      printf(" Pas de donnees pour initialisation.\n");
    nNtdcl  = nLenObservation - ((nLenObservation < 28800.0/dDT) ? nLenObservation : 28800.0/dDT);
    /*     printf("nNtdcl:%d\n",nNtdcl); */
    if(nNtdcl == 0) /* Patch parce que nNtdcl prend la valeur 0 en fortran!*/
      nNtdcl =1;
    f77name(makitp)(dpItp, &nIRef, &nIR40, &bFlat, &(dpTimeO[nNtdcl]), &(dpRTO[nNtdcl]), &(dpDTO[nNtdcl]), &(dpTAO[nNtdcl]), &dDiff, &dMLon, dpGri, npSwo); 
    nNtp = - nDeltaTIndice + nNtdcl;
    nNtp2 = nLenObservation - nDeltaTIndice;
    f77name(coupla)(dpFS, dpFI, dpPS, dpTA, dpAH, dpFF, dpTYP, dpFT, dpQP, dpRC, &nIRef, &nNtp, &nNtp2, dpCnt, dpItp, &(dpRTO[nLenObservation]), &bFlat, &dFCorr, dpWw, &dWa, &dAln, &dAlr, &dFp, &dFsCorr, &dFiCorr, &dEr1, &dEr2,  &bFail, &dEpsilon, &dZ0, &dZ0t, &dZu, &dZt, &dLCorr, &bEchec, stRA.pdArray, stSN.pdArray, stRC.plArray, stRT.pdArray, stIR.pdArray, stSF.pdArray, stFV.pdArray, stFC.pdArray, stFA.pdArray, stG.pdArray, stBB.pdArray, stFP.pdArray);  
    if(!bSilent)
      printf("coupla 1 \n");
    if(bEchec){
      bSucces = FALSE;       
      goto liberation;
    }
    if(bFail){
      long nOne = 1;
      if(!bSilent)
	printf("fail\n");      
      f77name(initial)(dpItp, (dpRTO+1), (dpDTO+1), (dpTAO+1), &nOne, &nLenObservation, dpCnt, &nIRef, &nIR40, &bFlat, npSwo); 
     }
  }
  else{/* observation complete */
    long nOne =1;
    if(!bSilent)
      printf("Observation complete\n");
    f77name(makitp)(dpItp, &nIRef, &nIR40, &bFlat, &(dpTimeO[nDeltaTIndice]), &(dpRTO[nDeltaTIndice]), &(dpDTO[nDeltaTIndice]), &(dpTAO[nDeltaTIndice]), &dDiff, &dMLon, dpGri, npSwo); 
    nNtdcl  = nLenObservation - nDeltaTIndice - ((nLenObservation-nDeltaTIndice < 28800.0/dDT) ? nLenObservation-nDeltaTIndice : 28800.0/dDT);
    f77name(initial)(dpItp , (dpRTO+1), (dpDTO+1), (dpTAO+1), &nOne, &nLenObservation, dpCnt, &nIRef, &nIR40, &bFlat, npSwo); 
    nNtp = 0 + nNtdcl;
    nNtp2 = nLenObservation - nDeltaTIndice;
    f77name(coupla)(dpFS, dpFI, dpPS, dpTA, dpAH, dpFF, dpTYP, dpFT, dpQP, dpRC, &nIRef, &nNtp, &nNtp2, dpCnt, dpItp, &(dpRTO[nLenObservation]), &bFlat, &dFCorr, dpWw, &dWa, &dAln, &dAlr, &dFp, &dFsCorr, &dFiCorr, &dEr1, &dEr2,  &bFail, &dEpsilon, &dZ0, &dZ0t, &dZu, &dZt, &dLCorr, &bEchec, stRA.pdArray, stSN.pdArray, stRC.plArray, stRT.pdArray, stIR.pdArray, stSF.pdArray, stFV.pdArray, stFC.pdArray, stFA.pdArray, stG.pdArray, stBB.pdArray, stFP.pdArray);
    if(!bSilent)
      printf("coupla 2\n");
     if(bEchec){
       bSucces = FALSE;       
       goto liberation;
     }
     if(bFail){
       long nOne = 1;
       if(!bSilent)
	 printf("fail\n");
       f77name(initial)(dpItp, (dpRTO+1), (dpDTO+1), (dpTAO+1), &nOne, &nLenObservation, dpCnt, &nIRef, &nIR40, &bFlat, npSwo);
     }
  }/* End else observation complete */

  
  /************Prevision**************************************************/
  f77name(balanc)(dpFS, dpFI, dpPS, dpTA, dpAH, dpFF, dpTYP, dpFT, dpQP, &nIRef, &nNtp2, &nNbrTimeSteps, dpCnt, dpItp, &bFlat, &dFCorr, dpWw, &dWa, &dAlr, &dAlr, &dFp, &dFsCorr, &dFiCorr, &dEr1, &dEr2,  &dEpsilon, &dZ0, &dZ0t, &dZu, &dZt, &bEchec, stRT.pdArray, stRA.pdArray ,stSN.pdArray, stRC.plArray, stIR.pdArray, stSF.pdArray, stFV.pdArray, stFC.pdArray, stFA.pdArray, stG.pdArray, stBB.pdArray, stFP.pdArray); 

  if(bEchec){
    if(!bSilent)
      printf("Echec in balanc\n");
    bSucces = FALSE;       
    goto liberation;
  }
  /* Preparation du fichier de sortie */
  if(!bSilent)
    printf("Free memory\n");

 liberation:
/* Free everybody */
/* String */
  /* double */
  nNbrOfZone= 1234;
  free(dpItp);
  dpItp = NULL;
  free(dpWw);
  dpWw = NULL;
  free(dpCnt);
  dpCnt = NULL;
  free(dpGri);
  dpGri = NULL;
}/* End Do_Metro */

int main(argc, argv) 
     int argc; 
     char *argv[];      
{ 

  
 return 0; 
} 


/****************************************************************************
 Name: Echo
 
 Parameters: None

Returns: None

Functions Called: printf

Description:
Function that only does a printf of "here".  Used to deploy this code
 in python.

Notes:

Revision History:

Author		Date		Reason
Miguel Tremblay 28 mai 2004     Put the C code in python.

 ***************************************************************************/
void Echo()
{
  printf("here\n");
}

/****************************************************************************
 Name: Echo
 
 Parameters: None

Returns: None

Functions Called: printf

Description: Initialize all the static structure

Notes:

Revision History:

Author		Date		Reason
Miguel Tremblay 9 mars 2005     

***************************************************************************/
void init_structure(long nSize)
{
  /* Allocation de mémoire */

  /* Size */

  stRC.nSize = nSize;
  stRA.nSize = nSize;
  stRT.nSize = nSize;
  stIR.nSize = nSize;
  stSF.nSize = nSize;
  stSN.nSize = nSize;
  stFV.nSize = nSize;
  stFC.nSize = nSize;
  stFA.nSize = nSize;
  stG.nSize = nSize;
  stBB.nSize = nSize;
  stFP.nSize = nSize;
  /* Memory alloc */
  stRC.plArray = (long*)calloc((nSize),sizeof(long));
  stRA.pdArray = (double*)calloc((nSize),sizeof(double));
  stIR.pdArray = (double*)calloc((nSize),sizeof(double));
  stSF.pdArray = (double*)calloc((nSize),sizeof(double));
  stRT.pdArray = (double*)calloc((nSize),sizeof(double));
  stSN.pdArray = (double*)calloc((nSize),sizeof(double));
  stFV.pdArray = (double*)calloc((nSize),sizeof(double));
  stFC.pdArray = (double*)calloc((nSize),sizeof(double));
  stFA.pdArray = (double*)calloc((nSize),sizeof(double));
  stG.pdArray = (double*)calloc((nSize),sizeof(double));
  stBB.pdArray = (double*)calloc((nSize),sizeof(double));
  stFP.pdArray = (double*)calloc((nSize),sizeof(double));

}

void mydebug(double* list1, long* list2){

  printf("In mydebug\n");

}

struct doubleStruct get_ra(void){

  return stRA;

}

struct doubleStruct get_sn(void){

  return stSN;

}

struct longStruct get_rc(void){

  return stRC;
}

struct doubleStruct get_rt(void){

  return stRT;
}

struct doubleStruct get_ir(void){

  return stIR;
}

struct doubleStruct get_sf(void){

  return stSF;
}

struct doubleStruct get_fv(void){

  return stFV;
}

struct doubleStruct get_fc(void){

  return stFC;
}

struct doubleStruct get_fa(void){

  return stFA;
}

struct doubleStruct get_g(void){

  return stG;
}

struct doubleStruct get_bb(void){

  return stBB;
}

struct doubleStruct get_fp(void){

  return stFP;
}
