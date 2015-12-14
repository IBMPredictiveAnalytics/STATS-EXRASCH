﻿file handle data /name="%er%/stats_exrasch/tests".
get file="data/raschdat1.sav".
dataset name rasch.
***show locale.

STATS EXRASCH MODEL=RM ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1.

STATS EXRASCH MODEL=RM ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/SAVE PERSONDATA=persondata

*Wald test.
STATS EXRASCH MODEL=RM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=yes SE=YES
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO WALDTEST=MEDIAN ETA=YES PERSONITEMPLOT=YES.

STATS EXRASCH MODEL=RM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=yes SE=YES ITEMDISPLAY=easiness
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO WALDTEST=MEDIAN.

STATS EXRASCH MODEL=RM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=YES SE=YES ITEMDISPLAY=DIFFICULTY
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO WALDTEST=MEDIAN.

* Unavailable Wald.
STATS EXRASCH MODEL=LLTM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=NO SE=YES ITEMDISPLAY=DIFFICULTY
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO WALDTEST=MEDIAN.

 
STATS EXRASCH MODEL=RM ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/SAVE PERSONDATA=persondata
/OUTPUT PERSONPLOT=no iteminfoplot=no iccplot=yes.


DATASET ACTIVATE rasch.
STATS EXRASCH MODEL=ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=NO.

STATS EXRASCH ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SE=YES.

STATS EXRASCH MODEL=RSM ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 V20 V21 V22 
    V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SE=YES.

file handle data /name="%er%/stats_exrasch/tests".
get file="data/rsmdat.sav".
dataset name rsmdat.
dataset activate rsmdat.

STATS EXRASCH MODEL=RSM ITEMS=V1 V2 V3 V4 V5 V6  REPS=1
/OPTIONS SE=YES.

file handle data /name="%er%/stats_exrasch/tests".
get file="data/pcmdat.sav".
dataset name pcmdat.
dataset activate pcmdat.
STATS EXRASCH MODEL=PCM ITEMS=V1 V2 V3 V4 V5 V6 V7 REPS=1
/OUTPUT ICCPLOT=YES thresh=yes.

STATS EXRASCH MODEL=PCM ITEMS=V1 V2 V3 V4 V5 V6 V7 REPS=1
/OPTIONS SE=YES
/OUTPUT ICCPLOT=YES thresh=yes.


STATS EXRASCH MODEL=PCM ITEMS=V1 V2 V3 V4 V5 V6 REPS=1
/OPTIONS SUM0=NO
/OUTPUT personplot=yes
/SAVE  PERSONDATA=PCMPERSON.


file handle data /name="%er%/stats_exrasch/tests".
get file="data/lltmdat1.sav".
dataset name lltmdat1.
dataset activate lltmdat1.

STATS EXRASCH MODEL=LLTM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 
    V20 V21 V22 V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=NO SE=YES
/SAVE PERSONDATA=lltmperson /OUTPUT PERSONPLOT=YES thresh=yes.

* small version to test plot.
STATS EXRASCH MODEL=LLTM  ITEMS=V1 V2 V3 V4 V5   REPS=1
/OPTIONS SUM0=NO SE=YES
/OUTPUT PERSONPLOT=YES iteminfoplot=yes.

STATS EXRASCH MODEL=LLTM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 
    V20 V21 V22 V23 V24 V25 V26 V27 V28 V29 V30  REPS=1
/OPTIONS SUM0=NO SE=YES
/SAVE PERSONDATA=lltmperson /OUTPUT PERSONPLOT=YES ITEMINFOPLOT=YES.


DATASET ACTIVATE lltmdat1.
STATS EXRASCH MODEL=LLTM  ITEMS=V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19 
    V20 V21 V22 V23 V24 V25 V26 V27 V28 V29 V30  REPS=2
/OPTIONS SUM0=NO SE=YES 
Y
/SAVE PERSONDATA=lltmperson 
/OUTPUT PERSONPLOT=YES ITEMINFOPLOT=YES DESIGNMATRIX=YES.


file handle data /name="%er%/stats_exrasch/tests".
get file="data/lpcmdat.sav".
dataset name lpcmdat.
dataset activate lpcmdat.

STATS EXRASCH MODEL=LPCM  ITEMS=X1 X2 X3 X4 X5 X6  GROUP=G REPS=2
/OPTIONS SUM0=NO SE=YES 
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO thresh=yes.

* error.
STATS EXRASCH MODEL=LPCM  ITEMS=X1 X2 X3 X4 X5 X6  GROUP=G REPS=1
/OPTIONS SUM0=NO SE=YES
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO.



STATS EXRASCH MODEL=LPCM  ITEMS=X1 X2 X3 X4 X5 X6  GROUP=G REPS=2
/OPTIONS SUM0=NO SE=YES
/OUTPUT PERSONPLOT=YES ITEMINFOPLOT=YES DESIGNMATRIX=YES.

STATS EXRASCH MODEL=LPCM  ITEMS=X1 X2 X3 X4 X5 X6  GROUP=G REPS=2
/OPTIONS SUM0=NO SE=YESS
/OUTPUT PERSONPLOT=no ITEMINFOPLOT=yes DESIGNMATRIX=no.

STATS EXRASCH MODEL=RM  ITEMS=X1 X2 X3 X4 X5 X6  REPS=1
/OPTIONS SUM0=NO SE=YES 
/OUTPUT ITEMINFOPLOT=NO DESIGNMATRIX=NO WALDTEST=MEDIAN.

DATASET ACTIVATE lpcmdat.
* fails with eRm error because of V1.
STATS EXRASCH MODEL=LPCM  ITEMS=V1 X1 X2 X3 X4 X5 X6  GROUP=G REPS=2
/OPTIONS SUM0=YES SE=YES
/OUTPUT ITEMINFOPLOT=YES DESIGNMATRIX=YES WALDTEST=MEDIAN THRESH=YES 
ITEMPERSONPLOT=YES ETA=NO.

STATS EXRASCH MODEL=LPCM  ITEMS=X1 X2 X3 X4 X5 X6  GROUP=G REPS=2
/OPTIONS SUM0=YES SE=YES
/OUTPUT ITEMINFOPLOT=YES DESIGNMATRIX=YES WALDTEST=MEDIAN THRESH=YES 
ITEMPERSONPLOT=YES ETA=NO.

