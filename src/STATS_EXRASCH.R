#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 2015
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# author__ = "SPSS, JKP"
# version__ = "1.1.0"

# History
# 22-apr-2013 Original Version
# 19-mar-2014 Convert missing values.  Add other missing value handling code.
# 11-mar-2015 Add controls for ICC plot

helptext="STATS EXRASCH MODEL= RM* OR LLTM OR RSM or LRSM or PCM or LPCM
ITEMS=itemvariables
GROUP=group variable
REPS = number of mesurements
/OPTIONS SE=YES or NO* SUM0 = YES* or NO
/SAVE PERSONDATA = datasetname
/OUTPUT PERSONPLOT = YES or NO* ITEMINFOPLOT = YES or NO* ICCPLOT = YES or NO*
PERSONITEMPLOT= YES or NO*
DESIGNMATRIX = YES or NO* WALDTEST = MEDIAN* or MEAN THRESH=YES or NO*
ETA = YES or NO*

Example:
STATS EXRASCH MODEL=RM ITEMVARIABLES=i1 i2 i3.

* indicates default value.
MODEL and ITEMS are required.  All other specifications are optional.

MODEL specifies the type of model to be estimated.
RM=Binary Rasch, 0/1 item values
LLTM=Linear Logistic Test, 0/1 item values
RSM=Polytomous Rating Scale, more than two values
LRSM=Linear Rating Scale, more than two values
PCM=Polytomous Partial Credit, more than two values
LPCM=Polytomous Linear Partial Credit, more than two values

ITEMS lists the item variables.  For RM and LLTM, the values must be 0 or 1.
For the other models, values must be integers starting with 0.
With multiple measures, the repeated measures appear as additional
variables in the same case (wide format), e.g., I1a I2a I3a I1b I2b I3b.

GROUP indicates the group membership of each case.  Values must be integers
starting from 1.  AUTORECODE is a convenient way to convert a group indicator
to this form.  GROUP can only be used with models LLTM, LRSM, and LPCM.

REPS specifies the number of repeated measures, defaulting to 1, i.e.,
no repeats.  If GROUP is used, REPS must be greater than 1
for the parameters to be identified.

SE specifies whether standard errors should be computed.

SUM0 specifies the parameter normalization.  If YES, the
parameters are  normalized to sum to zero.  If NO, the first
parameter is set to zero.

PERSONDATA specifies a dataset to be created holding person statistics.
The specified name must not already be in use.  If PERSONDATA is
specified a number of person-related statistics are shown in the Viewer.
PERSONDATA cannot be used if there is a GROUPS specification.

Missing data are accepted by the procedure, but certain statistics
for person data are not available in this case, and the structure
of the persondata dataset is a little different.

PERSONPLOT, ITEMINFOPLOT, ICCPLOT, and PERSONITEMPLOT specify whether or not
these plots should be produced.

DESIGNMATRIX specifies whether the design matrix for the model
is displayed.  This can be helpful in ensuring that the data
structure is appropriate for the model specifications.

WALDTEST specifies whether a table of tests for item elimination
is displayed.

THRESH specifies whether or not to display a table of item threshold.
This applies only when there are more than two values for items.

ETA specifies whether to display a table of the estimated etas
from which the beta parameters are computed.

STATS EXRASCH /HELP.  prints this information and does nothing else.

This extension command requires the R programmability plug-in and 
the R eRm package.
"

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}
gtxt <- function(...) {
    return(gettext(...,domain="STATS_EXRASCH"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_EXRASCH"))
}

models=list(RM=gtxt("Binary Rasch"),
    LLTM=gtxt("Linear Logistic Test"),
    RSM=gtxt("Polytomous Rating Scale"),
    LRSM=gtxt("Linear Rating Scale"),
    PCM=gtxt("Polytomous Partial Credit"),
    LPCM=gtxt("Polytomous Linear Partial Credit"))
    
conv = list(gtxt("relative gradient is close to zero, current iterate is probably solution."),
    gtxt("successive iterates within tolerance, current iterate is probably solution."),
    gtxt("last global step failed to locate a point lower than estimate. Either estimate is an approximate local minimum of the function or steptol is too small."),
    gtxt("iteration limit exceeded."),
    gtxt("maximum step size stepmax exceeded five consecutive times. Either the function is unbounded below, becomes asymptotic to a finite value from above in some direction or stepmax is too small.")
)
    
    
dorasch = function(model="rm", items, group=NULL, reps=1, 
    se=FALSE, sum0=TRUE, ci=.95,
    persondata=NULL, personplot=FALSE, iteminfoplot=FALSE, personitemplot=FALSE,
    iccplot=FALSE, mplot=FALSE, legpos="left", designmatrix=FALSE, 
    waldtest=NULL, thresh=FALSE, eta=FALSE) {
    #estimate Rasch models
    
    setuplocalization("STATS_EXRASCH")

    tryCatch(library(eRm), error=function(e){
        stop(gtxtf("The R %s package is required but could not be loaded.","eRm"),call.=FALSE)
        }
    )
    # ensure that output dataset names are not in use
    alldsspecs = c(persondata)
    if (!is.null(alldsspecs)) {
        alldatasets = spssdata.GetDataSetList()
        if (length(intersect(alldsspecs, alldatasets) > 0)) {
            stop(gtxt("One or more specified output dataset names are already in use"), call.=FALSE)
        }
    }
    model = toupper(model)
    rmargs = list(se=se, sum0=sum0)
    if (model %in% list("RM", "LLTM")) {
        values = "dichot"
        rsets = list("RM", "LLTM")
    } else {
        values = "poly"
        rsets = list("RSM", "LRSM", "PCM", "LPCM")
    }

    if (!is.null(group) || reps > 1) {
        rsets = intersect(rsets, list("LLTM", "LRSM", "LPCM"))
        if (reps > 1) {
            rmargs["mpoints"] = reps
        }
    }

    if (!model %in% rsets) {
        stop(gtxt("The specified model type is inconsistent with group and repetition specifications"),
            call.=FALSE)
    }
    duplicates = length(items) -length(union(items, NULL))
    if (duplicates > 0) {
        stop(gtxtf("There are %s duplicates in the item list", duplicates), call.=FALSE)
    }
    if (thresh && !se) {
        print(gtxt("Thresholds require that standard errors be computed.  Changing standard error setting"))
        se = TRUE
        rmargs$se=TRUE
    }
    dta = spssdata.GetDataFromSPSS(c(items, group), missingValueToNA=TRUE)
    if (!is.null(group)) {
        if (!is.null(persondata)) {
            stop(gtxt("Person parameters cannot be estimated with groups"), call.=FALSE)
        }
        # split off the last column
        groupdta = dta[ncol(dta)]
        dta = dta[-ncol(dta)]
        rmargs["groupvec"] = groupdta
    }

    rmargs$X = dta
    # make sure data is at least plausible for the function
    datarange = range(dta, na.rm=TRUE)
    if (datarange[1] != 0) {
        stop(gtxt("The item values must start at zero"), call.=FALSE)
    }
    if (model %in% list("RSM","LRSM","PCM","LPCM") && datarange[2] - datarange[1] <=2) {
        stop(gtxt("The items must have more than two values"), call.=FALSE)
    }
    if (model %in% list("RM", "LLTM") && datarange[2] != 1){
        stop(gtxt("The items must be coded 0/1 for this model"), call.=FALSE)
    }
    # model variable holds a string, but do.call calls the function by that name
    # arguments are quoted to avoid duplicating the data sent to the function

    res = do.call(model, quote=TRUE, rmargs)

    StartProcedure("Extended Rasch", "STATSEXRASCH")
    summarylabels=list(gtxt("Model"),
        gtxt("Group Variable"),
        gtxt("Repetitions"),
        gtxt("Log-Likelihood"),
        gtxt("Number of Parameters"),
        gtxt("Parameter Normalization"),
        gtxt("Number of Iterations"),
        gtxt("Convergence"),
        gtxt("Person Parameters Dataset")
    )
    summaryvalues = list(models[[model]],
        ifelse(is.null(group), gtxt("--None--"), group),
        reps,
        res$loglik,
        res$npar,
        ifelse(sum0 == TRUE, gtxt("Sum to zero"), gtxt("First set to zero")),
        res$iter,
        conv[[res$convergence]],
        ifelse(is.null(persondata), gtxt("--None--"), persondata)
    )
    names(summaryvalues) = summarylabels
    summarydf = data.frame(cbind(summaryvalues))
    colnames(summarydf) = gtxt("Values")
    spsspivottable.Display(summarydf, gtxt("Rasch Model Summary"), "RASCHSUMMARY",
        caption=gtxt("Results computed by R eRm package"),
        isSplit=FALSE
    )
    if (res$conv > 3) {
        stop(gtxt("Converge not achieved"), call.=FALSE)
    }
    rsumm(res, se, ci, sum0, eta)
    if (thresh && values == "dichot") {
        print(gtxt("Threshold parameters are not available for dichotomous items"))
    }
    if (thresh && values != "dichot") {
        threshf(res)
    }
    if (iteminfoplot) {
    # the combined plot becomes impossible to read when there are many items
        tryCatch({
            if (length(items) <=5) {
                plotINFO(res, type="both")
            } else {
                tryCatch(
                    plotINFO(res, type="item"), 
                    error=function(e) {
                        print(gtxt("The item information plot is not available for this model"))
                    }
                )
                tryCatch(
                    plotINFO(res, type="test"),
                    error = function(e) {
                        print(gtxt("The item information test plot is not available for this model"))
                    }
                )
            }
        },
        error=function(e) {
            print(gtxt("The item information plot is not available for this model"))
        }
        )
    }

    if (iccplot && ("dRm" %in% class(res) || "Rm" %in% class(res))) {
        arglist = list(object=res, ask=FALSE,
           xlab=gtxt("Latent Dimension"), ylab=gtxt("Probability to Solve"))
            # mplot = TRUE produces some wrong behavior
            # in the output (duplicate plots etc), so it has been removed
#         if (!is.null(mplot) && mplot != "auto") {
#             mplot = mplot != "grouped"
#             ###arglist["mplot"] = mplot
#             if (!mplot) {
        arglist["lwd"] = 3   # fatten up lines
        arglist['mplot'] = FALSE
            }
#        }
        if (legpos == "none") {
            legpos = FALSE
        }
        arglist["legpos"] = legpos
        do.call(plotICC, arglist)
#         plotICC(res, mplot=TRUE, ask=FALSE,
#             xlab=gtxt("Latent Dimension"), ylab=gtxt("Probability to Solve"))
#    }
    
    if (designmatrix) {
        spsspivottable.Display(as.data.frame(model.matrix(res)), title=gtxt("Design Matrix"),
            templateName="RASCHDESIGNMATRIX", isSplit=FALSE, format=formatSpec.Count)
    }
    if (!is.null(waldtest)) {
        if ("Rm" %in% class(res)) {
            ww = Waldtest(res, splitcr=waldtest)
            spsspivottable.Display(data.frame(ww$coef.table), 
                title=gtxt("Wald Test for Item Elimination"),
                templateName="RASCHWALD"
            )
        } else {
            print(gtxt("The Wald test is not available for this model."))
        }
    }
    if (personitemplot) {
        tryCatch(plotPImap(res, sorted=TRUE, main=gtxt("Person-Item Map"), latdim=gtxt("Latent Dimension"),
            pplabel = gtxt("Person\nParameter\nDistribution")),
            error = function(e) {
                print(gtxt("The item-person plot is not available for this model"))
            }
        )
    }
    if (!is.null(persondata)) {
        processperson(res, persondata, model, personplot)  # This function ends the procedure state
    } else {
        spsspkg.EndProcedure()
    }
}

rsumm = function(res, se, ci, sum0, eta) {
    # Display summary table with or without se and conf interval
    mult = abs(qnorm((1-ci)/2))
    caption = ""
        df = data.frame(-res$betapar)
        title = gtxt("Item (Category) Difficulty Parameters (Beta)")
        if (se) {
            caption = gtxtf("CI: %s", ci)
            df = data.frame(df, res$se.beta, -res$betapar - mult*res$se.beta, 
                -res$betapar + mult*res$se.beta)
            names(df) = list(gtxt("Estimate"), gtxt("Std. Error"), gtxt("Lower CI"), gtxt("Upper CI"))
        } else {
            names(df) = list(gtxt("Estimate"))
        }
    spsspivottable.Display(df, title=title, "RASCHITEMSUMMARY",
        isSplit=FALSE
    )
    # optional display of eta parameters
    if (eta) {
        df = data.frame(res$etapar)
        title = gtxt("Item (Eta) Parameters")
        if (se) {
            caption = gtxtf("CI: %s", ci)
            df = data.frame(df, res$se.eta, res$etapar - mult*res$se.eta, 
                res$etapar + mult*res$se.eta)
            names(df) = list(gtxt("Estimate"), gtxt("Std. Error"), gtxt("Lower CI"), gtxt("Upper CI"))
        } else {   
            names(df) = list(gtxt("Estimate"))
        }
    spsspivottable.Display(df, title=title, "RASCHITEMETAS",
        isSplit=FALSE
    )
    }
}

threshf = function(res) {
    # display threshold parameters
    # SE's are required to use this
    th = tryCatch(thresholds(res), 
            error=function(e) {
                print(gtxt("Thresholds could not be computed"))
                return
            }
        )
    tnames = names(th$threshpar)
    index = 1
    # Compute table of location parameters and thresholds
    # The threshtable structure is a list of tables - sometimes just 1
    # Each becomes a separate pivot table
    # Standard errors are displayed separately in order to keep the tables
    # of manageable width.
    # The threshold parameter names are indexed to create table titles
    
    for (t in 1:length(th$threshtable)) {
        df = data.frame(th$threshtable[t])
        names(df) = c(gtxt("Location"), paste(gtxt("Threshold"), 1:(ncol(df)-1)))
        if (length(th$threshtable) == 1) {
            title = gtxt("Item Thresholds")
        } else {
            title = gtxtf("Item Thresholds. %s", 
                sub("thresh.*c\\d+",gtxt("Design Matrix Block"),tnames[index]))
        }
        spsspivottable.Display(df, 
            title = title, templateName="RASCHTHRESH")
        index = index + nrow(df) * (ncol(df)-1)
    }
    # Std error and c.i table
    seci = data.frame(data.frame(th$se.thresh), data.frame(confint(th)))
    names(seci) = c(gtxt("Std. Error"), gtxtf("Lower %s%% C.I.", 95), gtxtf("Upper %s%% C.I.", 95))
    row.names(seci) = sub("thresh ", "", row.names(seci))
    spsspivottable.Display(seci, title=gtxt("Threshold Std. Errors and Confidence Intervals"),
        templateName="RASCHTHRESHSECI")
}

processperson = function(res, persondata, model, personplot) {
    # produce person dataset and related statistics and plot
    # tests are only available for dichotomous items
    pdata = person.parameter(res)
    
    if (model %in% list("RM", "LLTM")) {
        gof = tryCatch(gofIRT(pdata),
                       error = function(e) {print(e$message);print(gtxt("Some tables are unavailable")); return(NULL)}
        )
        values = "dichot"
    } else {
        gof = NULL
        values = "poly"
    }
    if (!is.null(gof)) {
        plabels = list(
            gtxt("Model"),
            gtxt("Unconditional (Joint) Log Likelihood"),
            gtxt("D.F"),
            gtxt("Number of Iterations")
        )
        pvalues = list(
            models[[model]],
            pdata$loglik,
            pdata$npar,
            pdata$iter
        )
        if (values == "dichot") {
            plabels[5:10] = c(
                gtxt("Pearson R2"),
                gtxt("Sum-of-Squares R2"),
                gtxt("McFadden R2"),
                gtxt("Accuracy"),
                gtxt("Sensitivity"),
                gtxt("Specificity")
            )
            pvalues[5:10] = c(
                gof$R2$R2.P,
                gof$R2$R2.SS,
                gof$R2$R2.MF,
                gof$classifier$accuracy,
                gof$classifier$sensitivity,
                gof$classifier$specificity
            )
        }
        names(pvalues) = plabels
        pdatadf = data.frame(cbind(pvalues))
        colnames(pdatadf) = gtxt("Statistics")
        spsspivottable.Display(pdatadf, title=gtxt("Person Ability Parameters"),
                               templateName="RASCHPERSON", isSplit=FALSE, format=formatSpec.Coefficient
        )
    }
    # Item fit statistics
    ifit = itemfit(pdata)
    # p value df adjustment from on eRm print.ifit.R
    ifitdf = data.frame(ifit$i.fit, ifit$i.df-1, 1 - pchisq(ifit$i.fit, ifit$i.df-1), ifit$i.outfitMSQ, 
                        ifit$i.infitMSQ, ifit$i.outfitZ, ifit$i.infitZ)
    names(ifitdf) = c(gtxt("Chi Squared"), gtxt("D.F."), gtxt("P Value"), gtxt("Outfit MSQ"),
                      gtxt("Infit MSQ"), gtxt("Outfit t"), gtxt("Infit t"))
    spsspivottable.Display(ifitdf, title=gtxt("Item Fit Statistics"), templateName="RASCHITEMFIT")
    
    # Confusion Matrix
    if (values == "dichot" && !is.null(gof)) {
        cm = gof$classifier$confmat
        cmdf = data.frame(matrix(cm, nrow(cm), ncol(cm)))
        row.names(cmdf) = 0:(nrow(cm) - 1)
        names(cmdf) = 0: (ncol(cm) - 1)
        spsspivottable.Display(cmdf, title=gtxt("Confusion Matrix"),
                               templateName="RASCHCONFUSION", isSplit=FALSE, 
                               rowdim = gtxt("Predicted"), coldim=gtxt("Observed"),
                               hiderowdimtitle=FALSE, hidecoldimtitle=FALSE,
                               format=formatSpec.Count
        )
        
        # Goodness of fit
        spsspivottable.Display(gof$test.table, title=gtxt("Person Goodness-of-Fit Tests"),
                               templateName="RASCHPGOF")
    }
    # Information criteria, if available
    icpdata = tryCatch(data.frame(IC(pdata)$ICtable), error = function(e) return(NULL))
    if (!is.null(icpdata)) {
        row.names(icpdata) = c(gtxt("Joint Log Likelihood"), gtxt("Marginal Log Likelihood"),
                               gtxt("Conditional Log Likelihood"))
        names(icpdata)[1:2] = c(gtxt("Value"), gtxt("Number of Parameters"))
        spsspivottable.Display(icpdata, title=gtxt("Person Fit Information Criteria"), templateName="RASCHINFCRIT")
    }
    if (personplot) {
        plot(pdata)
    }
    
    spsspkg.EndProcedure()
    
    # Create person parameters dataset
    
    tryCatch({
        pdict = list(c("Person", gtxt("Case ID"), 20, "A20", "nominal"),
                     c("Parameter", gtxt("Parameter"), 0, "F12.6", "scale"),
                     c("Missinggroup", gtxt("Missing Data Group"), 0, "F5.0", "nominal"),
                     c("Interpolated", gtxt("Interpolated"), 0, "F3.0", "nominal"),
                     c("SE", gtxt("Standard Error"), 0, "F12.6", "scale")
        )
        nsetheta = length(pdata$se.theta)
        # If the data set contains missing values, person parameters are 
        # estimated for each missing value subgroup.
        if (nsetheta > 1) { # multiple NA groups
            for (i in 1:nsetheta) {
                pdict[[4 + i]] = c(paste("SE", i, sep=""), paste(gtxt("Standard Error, NA Group"), i),
                                   0, "F12.6", "scale")
            }
        }
        dict = spssdictionary.CreateSPSSDictionary(pdict)
        spssdictionary.SetDictionaryToSPSS(persondata, dict)
        
        # if a person was interpolated, there will be no entry in pdata$se.theta
        # for that case, so we have to do a merge on row.names to line things
        # up.  In the resulting data frame, the cases where the se was missing
        # appear at the end.
        # Note: the summary function in eRm just leaves these rows out, but we
        # still want the person estimates to appear.
        # We can't resort into the original order, because the generated row names
        # are alphabetical.
        pse = domerge(pdata$se.theta)
        #df = merge(pdata$theta.table, pdata$se.theta, by="row.names", all.x=TRUE, sort=FALSE)
        df = merge(pdata$theta.table, pse, by="row.names", all.x=TRUE, sort=FALSE)
        spssdata.SetDataToSPSS(persondata, df)
        spssdictionary.EndDataStep()},
        error=function(e) {print(e)
                           print(gtxt("Failed to create person parameters dataset."))}
    )
}
domerge = function(pp) {
    # merge all the pdata$se.theta columns allowing for missing
    ngroups = length(pp)
    pm = data.frame()
    for (i in 1:ngroups) {
        pd = data.frame(pp[i])
        pm = merge(pm, pd, all.x=TRUE, all.y=TRUE, sort=FALSE, by=0)
        row.names(pm) = pm[["Row.names"]]
        pm['Row.names'] = NULL
    }
    return(pm)
}

# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

Run = function(args) {
    
    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("MODEL", subc="", ktype="str", var="model",
            vallist=list("rm", "lltm", "rsm", "lrsm", "pcm", "lpcm")),
        spsspkg.Template("ITEMS", subc="",  ktype="existingvarlist", var="items",
            islist=TRUE),
        spsspkg.Template("GROUP", subc="", ktype="existingvarlist", var="group",
            islist=FALSE),
        spsspkg.Template("REPS", subc="",  ktype="int", var="reps",
            vallist=list(1)),
        
        spsspkg.Template("SE", subc="OPTIONS", ktype="bool", var="se"),
        spsspkg.Template("SUM0", subc="OPTIONS", ktype="bool", var="sum0"),
        
        spsspkg.Template("PERSONDATA", subc="SAVE", ktype="varname", var="persondata"),
        
        spsspkg.Template("PERSONPLOT", subc="OUTPUT", ktype="bool", var="personplot"),
        spsspkg.Template("ITEMINFOPLOT", subc="OUTPUT", ktype="bool", var="iteminfoplot"),
        spsspkg.Template("ICCPLOT", subc="OUTPUT", ktype="bool", var="iccplot"),
        spsspkg.Template("ICCPLOTTYPE", subc="OUTPUT", ktype="str", var="mplot",
            vallist=list("grouped", "separate", "auto")),  # ignored due to plotICC problems
        spsspkg.Template("ICCPLOTLEGENDPOS", subc="OUTPUT", ktype="str", "legpos",
            vallist=list("bottom", "bottomleft", "left", "topleft", "top",
                "topright", "right", "center", "none")),
        spsspkg.Template("ITEMPERSONPLOT", subc="OUTPUT", ktype="bool", var="personitemplot"),
        spsspkg.Template("WALDTEST", subc="OUTPUT", ktype="str", var="waldtest",
            vallist=list("median", "mean")),
        spsspkg.Template("THRESH", subc="OUTPUT", ktype="bool", var="thresh"),
        spsspkg.Template("DESIGNMATRIX", subc="OUTPUT", ktype="bool", var="designmatrix"),
        spsspkg.Template("ETA", subc="OUTPUT", ktype="bool", var="eta"),
        spsspkg.Template("HELP", subc="", ktype="bool")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "dorasch")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
