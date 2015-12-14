file handle data /name="c:/cc/misc2/extensions/r/stats_survreg/tests".
get file="data/ovarian.sav".
dataset name ovarian.
variable level fustat (scale).
compute ID = 2 * ID.
exec.

stats survreg CENSORTYPE  = right event=fustat time1=futime indep=ecog.ps rx id=ID
/options distribution=weibull scale=1 missing=include /output survspecs=yes.

stats survreg CENSORTYPE  = right event=fustat time1=futime indep=ecog.ps rx id=ID
/options distribution=weibull scale=1 missing=include robust=no /output survspecs=yes.


* dataset tests.
* error: no specs.
stats survreg CENSORTYPE  = right event=fustat time1=futime indep=ecog.ps rx
/options distribution=weibull scale=1 /output survspecs=no
/save caseresults=results.

stats survreg CENSORTYPE  = right event=fustat time1=futime indep=ecog.ps rx id=ID
/options distribution=weibull scale=1 /output survspecs=no
/save caseresults=results residuals=response deviance ldcase ldresp shape.

stats survreg CENSORTYPE  = right event=fustat time1=futime indep=ecog.ps rx
/options distribution=weibull scale=1 /output survspecs=no
/save caseresults=results residuals=response deviance dfbeta 
PREDICT =response linear quantile uquantile quantiles=.2 .5 .8.


get file="data/tobin.sav".
dataset name tobin.
compute event  = durable > 0.
variable level event(scale).
exec.
stats survreg censortype=left event=event time1=durable
indep = age quant id=ID
/options distribution = gaussian scale=0
/save caseresults=tobinout residuals=dfbeta
/output survspecs=yes.

* with ID.
stats survreg censortype=left event=event time1=durable
indep = age quant
/options distribution = gaussian scale=0
/save caseresults=tobinout residuals=dfbeta
/output survspecs=yes.


* Visual Binning.
*age.
RECODE  age (MISSING=COPY) (LO THRU 45.1=1) (LO THRU 50.9=2) (LO THRU HI=3) (ELSE=SYSMIS) INTO 
    agebinned.
VARIABLE LABELS  agebinned 'age (Binned)'.
FORMATS  agebinned (F5.0).
VALUE LABELS  agebinned 1 '<= 45.1' 2 '45.2 - 50.9' 3 '51.0+'.
VARIABLE LEVEL  agebinned (ORDINAL).
EXECUTE.


STATS SURVREG CENSORTYPE=LEFT EVENT=event TIME1=durable ID=ID INDEP=agebinned quant
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=GAUSSIAN
/OUTPUT SURVSPECS=YES
/SAVE CASERESULTS=caseresults RESIDUALS=WORKING  PREDICT=LINEAR.

get file="data/lung.sav".
dataset name lung.
compute V1 = 2 * V1.
exec.
* no strata.
STATS SURVREG CENSORTYPE=RIGHT EVENT=status TIME1=time INDEP=ph.ecog sex age 
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=WEIBULL robust=yes
/OUTPUT SURVSPECS=NO.
* strata equivalent.
STATS SURVREG CENSORTYPE=RIGHT EVENT=status TIME1=time INDEP=ph.ecog age STRATA=sex
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=WEIBULL 
/OUTPUT SURVSPECS=NO.

get file="data/heart.sav".
dataset name heart.
* fails with invalid times.
STATS SURVREG CENSORTYPE=INTERVAL EVENT=event TIME1=start TIME2=stop INDEP=age transplant year 
    surgery 
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=WEIBULL ROBUST=NO
/OUTPUT SURVSPECS=YES.

DATASET ACTIVATE heart.
STATS SURVREG CENSORTYPE=INTERVAL EVENT=event TIME1=start TIME2=stop INDEP=age transplant year 
    surgery 
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=GAUSSIAN ROBUST=NO
/OUTPUT SURVSPECS=YES.

DATASET ACTIVATE tobin.
* error - no quantiles specified.
STATS SURVREG CENSORTYPE=LEFT EVENT=event TIME1=durable ID=ID INDEP=age quant 
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=LOGNORMAL ROBUST=NO
/OUTPUT SURVSPECS=NO
/SAVE CASERESULTS=cases RESIDUALS=RESPONSE DEVIANCE WORKING DFBETA DFBETAS LDCASE LDRESP LDSHAPE 
    PREDICT=RESPONSE LINEAR QUANTILE UQUANTILE.

STATS SURVREG CENSORTYPE=LEFT EVENT=event TIME1=durable ID=ID INDEP=age quant 
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=GAUSSIAN ROBUST=NO
/OUTPUT SURVSPECS=NO
/SAVE CASERESULTS=cases RESIDUALS=RESPONSE DEVIANCE WORKING DFBETA DFBETAS LDCASE LDRESP LDSHAPE 
    PREDICT=RESPONSE LINEAR QUANTILE UQUANTILE  QUANTILES=.5 .8.

STATS SURVREG CENSORTYPE=LEFT EVENT=event TIME1=durable ID=ID INDEP=age quant 
/OPTIONS SCALE=0 MAXITER=30 PCONVERGE=.000000001 SINGULAR=.0000000001 MISSING=LISTWISE 
    DISTRIBUTION=T DISTPARAM=4 ROBUST=NO
/OUTPUT SURVSPECS=NO
/SAVE CASERESULTS=cases RESIDUALS=RESPONSE DEVIANCE WORKING DFBETA DFBETAS LDCASE LDRESP LDSHAPE 
    PREDICT=RESPONSE LINEAR QUANTILE UQUANTILE  QUANTILES=.5 .8.

get file="c:/data/cars.sav".
dataset name cars.
compute bound=0.
exec.


