## Specify state-space model (RI-CLPM) ----

opmxL <- list()
manif <- c("MHI", "NFC")
latent <- c("y1", "y2", "RIy1", "RIy2")

# Autoregressive dynamics 
opmxL$amat <- mxMatrix(name="A", nrow=4, ncol=4, byrow = T,
                       free = c(T,T,F,F,
                                T,T,F,F,
                                F,F,F,F,
                                F,F,F,F),
                       values = c(-.2, 0,0,0,
                                  0,.5,0,0,
                                  0, 0,0,0,
                                  0, 0,0,0),
                       labels = c("a11", "c12",NA,NA,
                                  "c21","a22",NA,NA,
                                  NA,NA,NA,NA,
                                  NA,NA,NA,NA) )

# Input effects on the latent variables
opmxL$bmat <- mxMatrix(name = "B", "Zero", 4, 1)

#  Factor loadings in the measurement model
opmxL$cmat <- mxMatrix("Full", 2, 4, name = "C", free = F,
                       dimnames = list(manif, latent), 
                       values = matrix(c(1, 0, 1, 0,
                                         0, 1, 0, 1), nrow=2, byrow=T))

# Input effects on the observed variables
opmxL$dmat <- mxMatrix("Zero", 2, 1, name = "D")

# Dynamic error (i.e., innovations)
opmxL$qmat <- mxMatrix("Full", 4, 4, name = "Q",
                       free = matrix(c(T,T,F,F,
                                       T,T,F,F,
                                       F,F,F,F,
                                       F,F,F,F), nrow=4),
                       values = matrix(c(.5, 0, 0, 0,
                                         0, .5, 0, 0,
                                         0, 0,  0, 0,
                                         0, 0,  0, 0), nrow=4),
                       labels = matrix(c("derx","derxy",NA,NA,
                                         "derxy","dery",NA,NA,
                                         NA,NA,NA,NA,
                                         NA,NA,NA,NA), nrow=4))

# Measurement error
opmxL$rmat <- mxMatrix("Zero", 2, 2, F, name = "R")

## Mean vector
opmxL$xmat <- mxMatrix("Full", name='x0', nrow=4, ncol=1,
                       free = c(T,T,T,T),
                       values = c(0,0,2.21,4.32), # he puesto la media de todos y todos los dias NFC
                       labels = c("x0mn", "y0mn", "xmn","ymn"))

# Covariance matrix
opmxL$pmat <- mxMatrix("Full", 4, 4, name = "P0", 
                       free = c(T,T,F,F,
                                T,T,F,F,
                                F,F,T,T,
                                F,F,T,T),
                       values = c(1,.1, 0, 0,
                                  .1, 1, 0, 0,
                                  0, 0, 1, .1,
                                  0, 0, .1, 1),
                       labels = c("x0v", "x0y0cv" , NA, NA,
                                  "x0y0cv" , "y0v", NA, NA,
                                  NA     , NA     , "RIxv", "RIcv",
                                  NA     , NA     , "RIcv", "RIyv"), 
                       lbound = c(0,NA,NA,NA,
                                  NA,0,NA,NA,
                                  NA,NA,0,NA,
                                  NA,NA,NA,0))

# covariates
opmxL$umat <- mxMatrix("Zero", 1, 1, name = "u")

# Specification of the time index
opmxL$tmat <- mxMatrix('Full', 1, 1, name='time', labels='data.sem')

# Store matrices and algebra
opmxL$modataL_ct <- with(opmxL, list(opmxL$amat, opmxL$bmat, opmxL$cmat, opmxL$dmat,
                                     opmxL$qmat, opmxL$rmat, opmxL$umat, opmxL$tmat,
                                     opmxL$xmat, opmxL$pmat))
# Select expectation
opmxL$expSSCT <- mxExpectationStateSpaceContinuousTime(A = "A", B = "B",
                                                       C = "C", D = "D",
                                                       Q = "Q", R = "R",
                                                       x0 = "x0", P0 = "P0",
                                                       u = "u", t = "time")

## Create SSM-CT multisubject model
n <- length(bd)
indivmodels <- list()
modNames <- paste0("indiv", 1:n)

for(k in 1:n){
  DataSetForSubjectK <- bd[[k]] # poner aquí la base de datos
  indivmodels[[k]] <- mxModel(name=modNames[k],
                              opmxL$modataL_ct,
                              opmxL$expSSCT,
                              mxFitFunctionML(),
                              mxData(DataSetForSubjectK, type='raw')) 
}

model <- mxModel(name="RICLPM", indivmodels,
                 mxFitFunctionMultigroup(modNames))


## Run model
# fit <- mxTryHard(model, extraTries = 10)
#t1 <- proc.time()
bfitT <- mxRun(model)
#t2 <- proc.time()
#t2-t1
summary(bfitT)


