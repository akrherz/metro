
/***************************************************************************
**
** Nom:         macadam.h
**
** Auteur:      Miguel Tremblay
**
** Date:        April 16, 2004
**
** Description: .h qui contient les constantes qui �taient autrefois dans 
**  params.cdk.  L'allocation dynamique �tant impossible � faire avec un 
**  pointeur cr�� en C et pass� au Fortran, l'utilisation de constante comme
**  DTMAX doit �tre maintenu.
**
** TODO: Enlever les constantes � mesure que le code fortran les utilisant
**  est remplac� par du code en C.
**
****************************************************************************/

#include "global.h"
#include "number.h"

/* Nombre de pas de temps maximal apr�s le d�but de la matrice SCRIBE */
/*  12000 -> 100 h */
#define nDTMAX 12000

/* Pas de temps du mod�le de bilan �nerg�tique */
#define dDT 30.0

/* Nombre de temps (ligne) permis dans le infile/matrice SCRIBE */
#define nNRECMAX 73

/* Nombre de colonnes permis dans les matrices SCRIBE */
#define nNCOLMAX 73

/* Nombre maximal de niveaux des grilles dans le sol */
#define nNGRILLEMAX 200

/* constantes physiques pr */
#define dPI  3.141592653590e0  
#define dOMEGA  0.7292e-4 

void Do_Metro(BOOL bFlat, double dMLat, double dMLon, double* dpZones, long nNbrOfZone, long* npMateriau, double* dpTA, double* dpQP, double* dpFF, double* dpPS, double* dpFsPy, double* dpFI, double* dpFT, double* dpTYP, double* dpRc, double* dpTAO,  double* dpRTO, double* dpDTO, double* dpAH, double* dpTimeO, long* npSWO,  BOOL* bpNoObs, double dDeltaT, long nLenObservation, long nNbrTimeSteps, BOOL bSilent);
	      
void init_structure(long nSize);

struct doubleStruct get_ra(void);
struct doubleStruct get_sn(void);
struct longStruct get_rc(void);
struct doubleStruct get_rt(void);
struct doubleStruct get_ir(void);
struct doubleStruct get_sf(void);
struct doubleStruct get_fv(void);
struct doubleStruct get_fc(void);
struct doubleStruct get_fa(void);
struct doubleStruct get_g(void);
struct doubleStruct get_bb(void);
struct doubleStruct get_fp(void);
struct longStruct get_echec(void);
struct doubleStruct get_sst(void);
