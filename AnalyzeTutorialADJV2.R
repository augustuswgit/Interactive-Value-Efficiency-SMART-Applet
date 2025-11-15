##########################################################################
# (EDITED by Augustus Williams for applet development) Code for analyzing a simulated dataset to illustrate the weighted and 
# replicated longitudinal analysis methods for SMART trials proposed by 
# Lu, Nahum-Shani, Kasari, Lynch, Oslin, Pelham, Fabiano & Almirall (2016).
# Code by John J. Dziak and Jamie Yap, based on sample code by Xi Lu.
##########################################################################
# Choose options:
WorkingStructureOption <- "independence";  # Try choosing "independence", "AR-1", or "unstructured"
# to get a covariance matrix that treats observations as independent, autoregressive of order
# one, or having a freely estimated structure within each replication of a subject.; 
WeightsEstimationOption <- "known";        # Try choosing "known" or "estimated";
##########################################################################
# Load the data and prepare to start the analysis;
library(geepack);  # Load a package by S. Hojsgaard, U. Halekoh, & J. Yan for doing  
library(tidyverse)
#  generalized estimating equations (Liang & Zeger, 1986);
DataWideFormat <- read_csv("sim_engage_data_wide3.csv")
DataWideFormat$A1 <- DataWideFormat$a1
DataWideFormat$A2 <- DataWideFormat$a2
nTimes <- 6; 
##########################################################################
# Create the known weights based on the design.  
DataWideFormat$DesignWeight <- NA;
DataWideFormat$DesignWeight[which(DataWideFormat$R==0)] <- 4;
DataWideFormat$DesignWeight[which(DataWideFormat$R==1)] <- 2;
# Recall that DataWideFormat$R tells whether a subject is considered a responder or not. 
###########################################################################
# One option is to use the known weights as the weights for the analysis.  The other
# option is to estimate weights which might provide better performance.
if (WeightsEstimationOption=="known") {
  DataWideFormat$FinalWeight <- DataWideFormat$DesignWeight;
}
if (WeightsEstimationOption=="estimated") {
  DataWideFormat$A1DummyCoded <- 1*(DataWideFormat$A1==+1);
  logisticModel1 <- glm(formula=A1DummyCoded ~ pretest_centered + gender_centered + heavy_drinking_days + Y1, 
                        # It is not yet known whether Y1 is needed here.  
                        # The choice of other covariates is up to the user.;
                        family=binomial,
                        data=DataWideFormat);
  # This model predicts stage 1 treatment assignment from baseline covariates,
  # potentially including baseline outcome Y1.
  DataWideFormat$p1 <- fitted(logisticModel1);
  DataWideFormat$EstimatedWeight1 <- DataWideFormat$A1DummyCoded/DataWideFormat$p1 +
    (1-DataWideFormat$A1DummyCoded)/(1-DataWideFormat$p1);
  # The estimated weight at stage 1 is the inverse estimated probability
  # of receiving the treatment which the participant did in fact receive 
  # at stage 1.  This is analogous to an inverse propensity weight.
  DataWideFormat$A2DummyCoded <- 1*(DataWideFormat$A2==+1);
  DataWideFormat$A2DummyCoded[which(DataWideFormat$R==1)] <- NA;
  # responders were not re-randomized, so A2 is neither -1 nor +1;
  logisticModel2 <- glm(formula=A2DummyCoded ~ pretest_centered + gender_centered + heavy_drinking_days + Y1 + Y2, 
                        # It is not yet known whether Y1 is needed here.  
                        # The choice of other covariates is up to the user.;
                        family=binomial,
                        na.action=na.exclude,
                        data=DataWideFormat); 
  # Now we predict stage 2 treatment assignment.
  DataWideFormat$p2 <- fitted(logisticModel2);
  DataWideFormat$EstimatedWeight2 <- 1;     
  who.responded <- which(DataWideFormat$R==0);
  DataWideFormat$EstimatedWeight2[who.responded] <- 
    DataWideFormat$A2DummyCoded[who.responded]/DataWideFormat$p2[who.responded] +
    (1-DataWideFormat$A2DummyCoded[who.responded])/(1-DataWideFormat$p2[who.responded]);
  # Responders have a stage 2 weight of 1;  nonresponders have a stage 2 weight which is their
  # estimated probability of getting the treatment they did in fact get.
  DataWideFormat$FinalWeight <- DataWideFormat$EstimatedWeight1 * DataWideFormat$EstimatedWeight2;
  # The overall estimated weight is the product of the estimated weights from the two randomizations.; 
}
###########################################################################
# Reorganize the data from "wide" form to "long" form.
Time1 <- data.frame(cbind(DataWideFormat[,c("id","A1","R","A2","DesignWeight","FinalWeight")], Time=1, Y=DataWideFormat$Y1))
Time2 <- data.frame(cbind(DataWideFormat[,c("id","A1","R","A2","DesignWeight","FinalWeight")], Time=2, Y=DataWideFormat$Y2))
Time3 <- data.frame(cbind(DataWideFormat[,c("id","A1","R","A2","DesignWeight","FinalWeight")], Time=3, Y=DataWideFormat$Y3))
Time4 <- data.frame(cbind(DataWideFormat[,c("id","A1","R","A2","DesignWeight","FinalWeight")], Time=4, Y=DataWideFormat$Y4))
Time5 <- data.frame(cbind(DataWideFormat[,c("id","A1","R","A2","DesignWeight","FinalWeight")], Time=5, Y=DataWideFormat$Y5))
Time6 <- data.frame(cbind(DataWideFormat[,c("id","A1","R","A2","DesignWeight","FinalWeight")], Time=6, Y=DataWideFormat$Y6))
DataLongFormat <- rbind(Time1, Time2, Time3, Time4, Time5, Time6)
DataLongFormat <- DataLongFormat[order(DataLongFormat$id, DataLongFormat$Time),]
###########################################################################
# Recode time by creating the variables that tell how much time has been spent in  
# Stage One (before Time 2) or in Stage Two (after Time 2).
DataLongFormat$S1 <- ifelse(DataLongFormat$Time <= 2, DataLongFormat$Time - 1, 1)
DataLongFormat$S2 <- ifelse(DataLongFormat$Time <= 2, 0, DataLongFormat$Time - 2)
stopifnot(all(DataLongFormat$S1+DataLongFormat$S2==DataLongFormat$Time-1)); 
# If S1 and S2 are coded correctly, S1 + S2 should equal Time - 1, that is, 
# the total time since the beginning of treatment.
##############################################################
# Create "replications." 
# Replicate people who got A1=+1 and responded with R=1.  They were not
# re-randomized, so we have to replicate their data to inform both of
# the dynamic treatment regimens which they could have received had they
# been re-randomized.  It is somewhat as if we are creating two clones of
# each of them and counting each in a different treatment, because in
# reality it is indistinguishable which one they received.
##############################################################
DataLongFormat$wave <- DataLongFormat$Time;
RowsToReplicate <- DataLongFormat[which(DataLongFormat$R==1),];
RowsNotToReplicate <- DataLongFormat[which(DataLongFormat$R==0),];
PlusOnePseudodata <- RowsToReplicate;
PlusOnePseudodata$A2 <- 1;
MinusOnePseudodata <- RowsToReplicate;
MinusOnePseudodata$A2 <- -1;
MinusOnePseudodata$wave <- MinusOnePseudodata$Time + nTimes;  
# We keep the same subject ID to show that we don't really have all those
# new participants.  So we have to distinguish the new observations somehow,
# and so we treat them as new waves of data on the same person.  Although
# it seems very ad-hoc, this method has been shown to be valid.
##############################################################
# Create the final analysis dataset including replicates.
##############################################################
DataForAnalysis <- rbind(PlusOnePseudodata, MinusOnePseudodata, RowsNotToReplicate);
DataForAnalysis <- DataForAnalysis[order(DataForAnalysis$id,DataForAnalysis$wave),] 
# This sorts by the variables id and wave;
##############################################################
#  Do analysis with GEE and the weighted and replicated data, 
# using (at least initially) the working independence structure.
############################################################## 
GEEIndependent <- geeglm(formula = Y ~   S1 +
                           S2 +
                           S1:A1 + 
                           S2:A1 + 
                           S2:A2 +
                           S2:A1:A2,  
                         id=id,
                         weights = FinalWeight,    
                         data=DataForAnalysis,
                         corstr = "independence");
##############################################################
#  Do analysis with the non-independence structure, if desired.
############################################################## 
if (WorkingStructureOption=="independence") {
  FinalGEEOutput <- GEEIndependent;
  BlockWorkCorr <- diag(rep(1,nTimes)); # identity matrix;
}
if (WorkingStructureOption=="AR-1") {
  if(sum(is.na(DataLongFormat$Y))>0) {
    stop("This version of the analysis code assumes there is no missing data.")
  }
  # Estimate the AR-1 correlation parameter using the method of moments, by
  # finding the average cross-product of residuals of adjacent observations
  # and then dividing it by the average squared residuals of observations.
  # That is, cor(Y[t],Y[t+1]) = cov(Y[t],Y[t+1]) / var(Y).  This assumes
  # homoskedasticity (equal variance across time) for Y.  However, more 
  # elaborate code could work around this assumption.
  residualsByWave <- list(GEEIndependent$residuals[which(DataForAnalysis$wave==1)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==2)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==3)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==4)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==5)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==6)]);
  weightsByWave <- list(GEEIndependent$weights[which(DataForAnalysis$wave==1)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==2)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==3)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==4)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==5)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==6)]);
  idsByWave <- list(GEEIndependent$id[which(DataForAnalysis$wave==1)],
                    GEEIndependent$id[which(DataForAnalysis$wave==2)],
                    GEEIndependent$id[which(DataForAnalysis$wave==3)],
                    GEEIndependent$id[which(DataForAnalysis$wave==4)],
                    GEEIndependent$id[which(DataForAnalysis$wave==5)],
                    GEEIndependent$id[which(DataForAnalysis$wave==6)]);
  stopifnot(all(idsByWave[[1]]==idsByWave[[2]]) &
              all(idsByWave[[1]]==idsByWave[[3]]) &
              all(idsByWave[[4]]==idsByWave[[5]]) &
              all(idsByWave[[4]]==idsByWave[[6]]));  # double check that residuals are not being mismatched;
  stopifnot(all(weightsByWave[[1]]==weightsByWave[[2]]) &
              all(weightsByWave[[1]]==weightsByWave[[3]]) &
              all(weightsByWave[[4]]==weightsByWave[[5]]) &
              all(weightsByWave[[4]]==weightsByWave[[6]]));  # double check that weights are equal across wave within replicant as we assume;
  averageSquaredResidual <- weighted.mean(x=c(residualsByWave[[1]]^2,
                                              residualsByWave[[2]]^2,
                                              residualsByWave[[3]]^2,
                                              residualsByWave[[4]]^2,
                                              residualsByWave[[5]]^2,
                                              residualsByWave[[6]]^2),
                                          w=c(weightsByWave[[1]],
                                              weightsByWave[[2]],
                                              weightsByWave[[3]],
                                              weightsByWave[[4]],
                                              weightsByWave[[5]],
                                              weightsByWave[[6]]));
  averageCrossProductResidual <- weighted.mean(x=c(residualsByWave[[1]]*residualsByWave[[2]],
                                                   residualsByWave[[2]]*residualsByWave[[3]],
                                                   residualsByWave[[4]]*residualsByWave[[5]],
                                                   residualsByWave[[5]]*residualsByWave[[6]]),
                                               w=c(weightsByWave[[1]],
                                                   weightsByWave[[2]],
                                                   weightsByWave[[4]],
                                                   weightsByWave[[5]]));
  correlationEstimate <- averageCrossProductResidual / averageSquaredResidual;
  BlockWorkCorr <- matrix(0,nTimes,nTimes);
  for (thisRow in 1:nrow(BlockWorkCorr)) {
    for (thisColumn in 1:ncol(BlockWorkCorr)) {
      BlockWorkCorr[thisRow,thisColumn] <- correlationEstimate^abs(thisRow-thisColumn);
    }
  } 
  WorkCorr <- rbind(cbind(BlockWorkCorr,0*BlockWorkCorr),cbind(0*BlockWorkCorr,BlockWorkCorr)); 
  WorkCorrAsZCor <- fixed2Zcor(cor.fixed=WorkCorr, 
                               id=DataForAnalysis$id,  
                               waves=DataForAnalysis$wave); 
  FinalGEEOutput <- geeglm(formula = Y ~   S1 +
                             S2 +
                             S1:A1 + 
                             S2:A1 + 
                             S2:A2 +
                             S2:A1:A2,  
                           id=id,
                           weights = FinalWeight,    
                           data=DataForAnalysis,
                           corstr = "fixed",
                           zcor=WorkCorrAsZCor);   
}
if (WorkingStructureOption=="unstructured") {
  if(sum(is.na(DataLongFormat$Y))>0) {
    stop("This version of the analysis code assumes there is no missing data.")
  }
  # We have three correlation parameters to estimate using the method of 
  # moments here.  Specifically, they are cor(Y[1],Y[2]), cor(Y[1],Y[3]),
  # and cor(Y[2],Y[3])
  # As in the AR-1 case, we assume var(Y[1])=var(Y[2])=var(Y[3]).
  # 
  residualsByWave <- list(GEEIndependent$residuals[which(DataForAnalysis$wave==1)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==2)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==3)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==4)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==5)],
                          GEEIndependent$residuals[which(DataForAnalysis$wave==6)]);
  weightsByWave <- list(GEEIndependent$weights[which(DataForAnalysis$wave==1)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==2)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==3)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==4)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==5)],
                        GEEIndependent$weights[which(DataForAnalysis$wave==6)]);
  idsByWave <- list(GEEIndependent$id[which(DataForAnalysis$wave==1)],
                    GEEIndependent$id[which(DataForAnalysis$wave==2)],
                    GEEIndependent$id[which(DataForAnalysis$wave==3)],
                    GEEIndependent$id[which(DataForAnalysis$wave==4)],
                    GEEIndependent$id[which(DataForAnalysis$wave==5)],
                    GEEIndependent$id[which(DataForAnalysis$wave==6)]);
  stopifnot(all(idsByWave[[1]]==idsByWave[[2]]) &
              all(idsByWave[[1]]==idsByWave[[3]]) &
              all(idsByWave[[4]]==idsByWave[[5]]) &
              all(idsByWave[[4]]==idsByWave[[6]]));  # double check that residuals are not being mismatched;
  stopifnot(all(weightsByWave[[1]]==weightsByWave[[2]]) &
              all(weightsByWave[[1]]==weightsByWave[[3]]) &
              all(weightsByWave[[4]]==weightsByWave[[5]]) &
              all(weightsByWave[[4]]==weightsByWave[[6]]));  # double check that weights are equal across wave within replicant as we assume;
  averageSquaredResidual <- weighted.mean(x=c(residualsByWave[[1]]^2,
                                              residualsByWave[[2]]^2,
                                              residualsByWave[[3]]^2,
                                              residualsByWave[[4]]^2,
                                              residualsByWave[[5]]^2,
                                              residualsByWave[[6]]^2),
                                          w=c(weightsByWave[[1]],
                                              weightsByWave[[2]],
                                              weightsByWave[[3]],
                                              weightsByWave[[4]],
                                              weightsByWave[[5]],
                                              weightsByWave[[6]]));
  averageCrossProductResidual12 <- weighted.mean(x=c(residualsByWave[[1]]*residualsByWave[[2]],
                                                     residualsByWave[[4]]*residualsByWave[[5]]),
                                                 w=c(weightsByWave[[1]],
                                                     weightsByWave[[4]]));
  averageCrossProductResidual13 <- weighted.mean(x=c(residualsByWave[[1]]*residualsByWave[[3]],
                                                     residualsByWave[[4]]*residualsByWave[[6]]),
                                                 w=c(weightsByWave[[1]],
                                                     weightsByWave[[4]]));
  averageCrossProductResidual23 <- weighted.mean(x=c(residualsByWave[[2]]*residualsByWave[[3]],
                                                     residualsByWave[[5]]*residualsByWave[[6]]),
                                                 w=c(weightsByWave[[2]],
                                                     weightsByWave[[5]]));
  correlationEstimate12 <- averageCrossProductResidual12 / averageSquaredResidual;
  correlationEstimate13 <- averageCrossProductResidual13 / averageSquaredResidual;
  correlationEstimate23 <- averageCrossProductResidual23 / averageSquaredResidual;
  BlockWorkCorr <- matrix(c(1, correlationEstimate12, correlationEstimate13,
                            correlationEstimate12, 1, correlationEstimate23,
                            correlationEstimate13, correlationEstimate23, 1),
                          nrow=3,ncol=3,
                          byrow = TRUE);
  WorkCorr <- rbind(cbind(BlockWorkCorr,0*BlockWorkCorr),cbind(0*BlockWorkCorr,BlockWorkCorr)); 
  WorkCorrAsZCor <- fixed2Zcor(cor.fixed=WorkCorr, 
                               id=DataForAnalysis$id,  
                               waves=DataForAnalysis$wave); 
  FinalGEEOutput <- geeglm(formula = Y ~   S1 +
                             S2 +
                             S1:A1 + 
                             S2:A1 + 
                             S2:A2 +
                             S2:A1:A2,  
                           id=id,
                           weights = FinalWeight,    
                           data=DataForAnalysis,
                           corstr = "fixed",
                           zcor=WorkCorrAsZCor);   
  GEECoefficients <- coef(FinalGEEOutput);
  GEECovarianceMatrix <- FinalGEEOutput$geese$vbeta;
}
GEECoefficients <- coef(FinalGEEOutput);
GEECovarianceMatrix <- FinalGEEOutput$geese$vbeta;
##############################################################
# Convert the regression coefficients estimates into the desired linear contrast estimates. 
############################################################## 
ContrastCoefficients <- matrix(c(
  1.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,   # Time 1 Mean,   Any Regimen
  1.00,  1.00,  0.00,  1.00,  0.00,  0.00,  0.00,   # Time 2 Mean,   ++ or +-
  1.00,  1.00,  0.00, -1.00,  0.00,  0.00,  0.00,   # Time 2 Mean,  -+ or --
  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,  1.00,   # Time 3 Mean,   ++
  1.00,  1.00,  1.00,  1.00,  1.00, -1.00, -1.00,   # Time 3 Mean,   +-
  1.00,  1.00,  1.00, -1.00, -1.00,  1.00, -1.00,   # Time 3 Mean,  -+
  1.00,  1.00,  1.00, -1.00, -1.00, -1.00,  1.00,   # Time 3 Mean,  --
  0.00,  0.00,  0.00,  2.00,  0.00,  0.00,  0.00,   # Time 2 Mean,   + vs. -
  1.00,  1.00,  2.00,  1.00,  2.00,  2.00,  2.00,   # Time 4 Mean,   ++
  1.00,  1.00,  2.00,  1.00,  2.00, -2.00, -2.00,   # Time 4 Mean,   +-
  1.00,  1.00,  2.00, -1.00, -2.00,  2.00, -2.00,   # Time 4 Mean,  -+
  1.00,  1.00,  2.00, -1.00, -2.00, -2.00,  2.00,   # Time 4 Mean,  --
  1.00,  1.00,  3.00,  1.00,  3.00,  3.00,  3.00,   # Time 5 Mean,   ++
  1.00,  1.00,  3.00,  1.00,  3.00, -3.00, -3.00,   # Time 5 Mean,   +-
  1.00,  1.00,  3.00, -1.00, -3.00,  3.00, -3.00,   # Time 5 Mean,  -+
  1.00,  1.00,  3.00, -1.00, -3.00, -3.00,  3.00,   # Time 5 Mean,  --
  1.00,  1.00,  4.00,  1.00,  4.00,  4.00,  4.00,   # Time 6 Mean,   ++
  1.00,  1.00,  4.00,  1.00,  4.00, -4.00, -4.00,   # Time 6 Mean,   +-
  1.00,  1.00,  4.00, -1.00, -4.00,  4.00, -4.00,   # Time 6 Mean,  -+
  1.00,  1.00,  4.00, -1.00, -4.00, -4.00,  4.00,   # Time 6 Mean,  --
  0.00,  0.00,  0.00,  0.00,  0.00,  2.00,  2.00,   # Time 3 Mean,   ++ vs. +-
  0.00,  0.00,  0.00,  2.00,  2.00,  0.00,  2.00,   # Time 3 Mean,   ++ vs. -+
  0.00,  0.00,  0.00,  2.00,  2.00,  2.00,  0.00,   # Time 3 Mean,   ++ vs. --
  0.00,  0.00,  0.00,  2.00,  2.00, -2.00,  0.00,   # Time 3 Mean,   +- vs. -+
  0.00,  0.00,  0.00,  2.00,  2.00,  0.00, -2.00,   # Time 3 Mean,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  2.00, -2.00,   # Time 3 Mean,  -+ vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  4.00,  4.00,   # Time 4 Mean,   ++ vs. +-
  0.00,  0.00,  0.00,  2.00,  4.00,  0.00,  4.00,   # Time 4 Mean,   ++ vs. -+
  0.00,  0.00,  0.00,  2.00,  4.00,  4.00,  0.00,   # Time 4 Mean,   ++ vs. --
  0.00,  0.00,  0.00,  2.00,  4.00, -4.00,  0.00,   # Time 4 Mean,   +- vs. -+
  0.00,  0.00,  0.00,  2.00,  4.00,  0.00, -4.00,   # Time 4 Mean,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  4.00, -4.00,   # Time 4 Mean,  -+ vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  6.00,  6.00,   # Time 5 Mean,   ++ vs. +-
  0.00,  0.00,  0.00,  2.00,  6.00,  0.00,  6.00,   # Time 5 Mean,   ++ vs. -+
  0.00,  0.00,  0.00,  2.00,  6.00,  6.00,  0.00,   # Time 5 Mean,   ++ vs. --
  0.00,  0.00,  0.00,  2.00,  6.00, -6.00,  0.00,   # Time 5 Mean,   +- vs. -+
  0.00,  0.00,  0.00,  2.00,  6.00,  0.00, -6.00,   # Time 5 Mean,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  6.00, -6.00,   # Time 5 Mean,  -+ vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  8.00,  8.00,   # Time 6 Mean,   ++ vs. +-
  0.00,  0.00,  0.00,  2.00,  8.00,  0.00,  8.00,   # Time 6 Mean,   ++ vs. -+
  0.00,  0.00,  0.00,  2.00,  8.00,  8.00,  0.00,   # Time 6 Mean,   ++ vs. --
  0.00,  0.00,  0.00,  2.00,  8.00, -8.00,  0.00,   # Time 6 Mean,   +- vs. -+
  0.00,  0.00,  0.00,  2.00,  8.00,  0.00, -8.00,   # Time 6 Mean,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  8.00, -8.00,   # Time 6 Mean,  -+ vs. --
  0.00,  1.00,  0.00,  1.00,  0.00,  0.00,  0.00,   # Stage 1 Slope,   ++ or +-
  0.00,  1.00,  0.00, -1.00,  0.00,  0.00,  0.00,   # Stage 1 Slope,  -+ or --
  0.00,  0.00,  1.00,  0.00,  1.00,  1.00,  1.00,   # Stage 2 Slope,   ++
  0.00,  0.00,  1.00,  0.00,  1.00, -1.00, -1.00,   # Stage 2 Slope,   +-
  0.00,  0.00,  1.00,  0.00, -1.00,  1.00, -1.00,   # Stage 2 Slope,  -+
  0.00,  0.00,  1.00,  0.00, -1.00, -1.00,  1.00,   # Stage 2 Slope,  --
  0.00,  0.00,  0.00,  0.00,  0.00,  2.00,  2.00,   # Stage 2 Slope,   ++ vs. +-
  0.00,  0.00,  0.00,  0.00,  2.00,  0.00,  2.00,   # Stage 2 Slope,   ++ vs. -+
  0.00,  0.00,  0.00,  0.00,  2.00,  2.00,  0.00,   # Stage 2 Slope,   ++ vs. --
  0.00,  0.00,  0.00,  0.00,  2.00, -2.00,  0.00,   # Stage 2 Slope,   +- vs. -+
  0.00,  0.00,  0.00,  0.00,  2.00,  0.00, -2.00,   # Stage 2 Slope,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  0.00,  2.00, -2.00,   # Stage 2 Slope,  -+ vs. --
  2.00,  1.50,  0.50,  1.50,  0.50,  0.50,  0.50,   # Area under Curve,   ++
  2.00,  1.50,  0.50,  1.50,  0.50, -0.50, -0.50,   # Area under Curve,   +-
  2.00,  1.50,  0.50, -1.50, -0.50,  0.50, -0.50,   # Area under Curve,  -+
  2.00,  1.50,  0.50, -1.50, -0.50, -0.50,  0.50,   # Area under Curve,  --
  0.00,  0.00,  0.00,  0.00,  0.00, -1.00,  1.00,   # Area,   ++ vs. +-
  0.00,  0.00,  0.00,  3.00,  1.00,  0.00,  1.00,   # Area,   ++ vs. -+
  0.00,  0.00,  0.00,  3.00,  1.00,  1.00,  0.00,   # Area,   ++ vs. --
  0.00,  0.00,  0.00,  3.00,  1.00, -1.00,  0.00,   # Area,   +- vs. -+
  0.00,  0.00,  0.00,  3.00,  1.00,  0.00, -1.00,   # Area,   +- vs. --
  0.00,  0.00,  0.00,  3.00,  1.00,  0.00, -1.00,   # Area,   +- vs. --
  1.00,  0.75,  0.25,  0.75,  0.25,  0.25,  0.25,   # Average value,   ++
  1.00,  0.75,  0.25,  0.75,  0.25, -0.25, -0.25,   # Average value,   +-
  1.00,  0.75,  0.25, -0.75, -0.25,  0.25, -0.25,   # Average value,  -+
  1.00,  0.75,  0.25, -0.75, -0.25, -0.25,  0.25,   # Average value,  --
  0.00,  0.00,  0.00,  0.00,  0.00, -0.50,  0.50,   # Average,   ++ vs. +-
  0.00,  0.00,  0.00,  1.50,  0.50,  0.00,  0.50,   # Average,   ++ vs. -+
  0.00,  0.00,  0.00,  1.50,  0.50,  0.50,  0.00,   # Average,   ++ vs. --
  0.00,  0.00,  0.00,  1.50,  0.50, -0.50,  0.00,   # Average,   +- vs. -+
  0.00,  0.00,  0.00,  1.50,  0.50,  0.00, -0.50,   # Average,   +- vs. --
  0.00,  0.00,  0.00,  1.50,  0.50,  0.00, -0.50,   # Average,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  2.00,  0.00,  2.00,   # Delayed Effect,   ++ vs. -+
  0.00,  0.00,  0.00,  0.00,  2.00,  2.00,  0.00,   # Delayed Effect,   ++ vs. --
  0.00,  0.00,  0.00,  0.00,  2.00, -2.00,  0.00,   # Delayed Effect,   +- vs. -+
  0.00,  0.00,  0.00,  0.00,  2.00,  0.00, -2.00,   # Delayed Effect,   +- vs. --
  0.00,  0.00,  0.00,  0.00,  2.00,  0.00,  0.00    # Ave. Delayed Eff.,  + vs -
), byrow=TRUE, ncol=7)

rownames(ContrastCoefficients) <- c(
  "Time 1 Mean,  Any Regimen",
  "Time 2 Mean,  ++ or +-",
  "Time 2 Mean,  -+ or --",
  "Time 3 Mean,  ++",
  "Time 3 Mean,  +-",
  "Time 3 Mean,  -+",
  "Time 3 Mean,  --",
  "Time 2 Mean,  + vs. -",
  "Time 4 Mean,  ++",
  "Time 4 Mean,  +-",
  "Time 4 Mean,  -+",
  "Time 4 Mean,  --",
  "Time 5 Mean,  ++",
  "Time 5 Mean,  +-",
  "Time 5 Mean,  -+",
  "Time 5 Mean,  --",
  "Time 6 Mean,  ++",
  "Time 6 Mean,  +-",
  "Time 6 Mean,  -+",
  "Time 6 Mean,  --",
  "Time 3 Mean,  ++ vs. +-",
  "Time 3 Mean,  ++ vs. -+",
  "Time 3 Mean,  ++ vs. --",
  "Time 3 Mean,  +- vs. -+",
  "Time 3 Mean,  +- vs. --",
  "Time 3 Mean,  -+ vs. --",
  "Time 4 Mean,  ++ vs. +-",
  "Time 4 Mean,  ++ vs. -+",
  "Time 4 Mean,  ++ vs. --",
  "Time 4 Mean,  +- vs. -+",
  "Time 4 Mean,  +- vs. --",
  "Time 4 Mean,  -+ vs. --",
  "Time 5 Mean,  ++ vs. +-",
  "Time 5 Mean,  ++ vs. -+",
  "Time 5 Mean,  ++ vs. --",
  "Time 5 Mean,  +- vs. -+",
  "Time 5 Mean,  +- vs. --",
  "Time 5 Mean,  -+ vs. --",
  "Time 6 Mean,  ++ vs. +-",
  "Time 6 Mean,  ++ vs. -+",
  "Time 6 Mean,  ++ vs. --",
  "Time 6 Mean,  +- vs. -+",
  "Time 6 Mean,  +- vs. --",
  "Time 6 Mean,  -+ vs. --",
  "Stage 1 Slope,  ++ or +-",
  "Stage 1 Slope,  -+ or --",
  "Stage 2 Slope,  ++",
  "Stage 2 Slope,  +-",
  "Stage 2 Slope,  -+",
  "Stage 2 Slope,  --",
  "Stage 2 Slope,  ++ vs. +-",
  "Stage 2 Slope,  ++ vs. -+",
  "Stage 2 Slope,  ++ vs. --",
  "Stage 2 Slope,  +- vs. -+",
  "Stage 2 Slope,  +- vs. --",
  "Stage 2 Slope,  -+ vs. --",
  "Area under Curve,  ++",
  "Area under Curve,  +-",
  "Area under Curve,  -+",
  "Area under Curve,  --",
  "Area,  ++ vs. +-",
  "Area,  ++ vs. -+",
  "Area,  ++ vs. --",
  "Area,  +- vs. -+",
  "Area,  +- vs. --",
  "Area,  +- vs. --",
  "Average value,  ++",
  "Average value,  +-",
  "Average value,  -+",
  "Average value,  --",
  "Average,  ++ vs. +-",
  "Average,  ++ vs. -+",
  "Average,  ++ vs. --",
  "Average,  +- vs. -+",
  "Average,  +- vs. --",
  "Average,  +- vs. --",
  "Delayed Effect,  ++ vs. -+",
  "Delayed Effect,  ++ vs. --",
  "Delayed Effect,  +- vs. -+",
  "Delayed Effect,  +- vs. --",
  "Ave. Delayed Eff.,  + vs -"
)

#EDIT DUE TO SPECIAL CHARACTER TROUBLES
rownames(ContrastCoefficients) <- c(
  "Time 1 Mean Any Regimen",
  "Time 2 Mean PP or PM",
  "Time 2 Mean MP or MM",
  "Time 3 Mean PP",
  "Time 3 Mean PM",
  "Time 3 Mean MP",
  "Time 3 Mean MM",
  "Time 2 Mean P vs M",
  "Time 4 Mean PP",
  "Time 4 Mean PM",
  "Time 4 Mean MP",
  "Time 4 Mean MM",
  "Time 5 Mean PP",
  "Time 5 Mean PM",
  "Time 5 Mean MP",
  "Time 5 Mean MM",
  "Time 6 Mean PP",
  "Time 6 Mean PM",
  "Time 6 Mean MP",
  "Time 6 Mean MM",
  "Time 3 Mean PP vs PM",
  "Time 3 Mean PP vs MP",
  "Time 3 Mean PP vs MM",
  "Time 3 Mean PM vs MP",
  "Time 3 Mean PM vs MM",
  "Time 3 Mean MP vs MM",
  "Time 4 Mean PP vs PM",
  "Time 4 Mean PP vs MP",
  "Time 4 Mean PP vs MM",
  "Time 4 Mean PM vs MP",
  "Time 4 Mean PM vs MM",
  "Time 4 Mean MP vs MM",
  "Time 5 Mean PP vs PM",
  "Time 5 Mean PP vs MP",
  "Time 5 Mean PP vs MM",
  "Time 5 Mean PM vs MP",
  "Time 5 Mean PM vs MM",
  "Time 5 Mean MP vs MM",
  "Time 6 Mean PP vs PM",
  "Time 6 Mean PP vs MP",
  "Time 6 Mean PP vs MM",
  "Time 6 Mean PM vs MP",
  "Time 6 Mean PM vs MM",
  "Time 6 Mean MP vs MM",
  "Stage 1 Slope PP or PM",
  "Stage 1 Slope MP or MM",
  "Stage 2 Slope PP",
  "Stage 2 Slope PM",
  "Stage 2 Slope MP",
  "Stage 2 Slope MM",
  "Stage 2 Slope PP vs PM",
  "Stage 2 Slope PP vs MP",
  "Stage 2 Slope PP vs MM",
  "Stage 2 Slope PM vs MP",
  "Stage 2 Slope PM vs MM",
  "Stage 2 Slope MP vs MM",
  "Area under Curve PP",
  "Area under Curve PM",
  "Area under Curve MP",
  "Area under Curve MM",
  "Area PP vs PM",
  "Area PP vs MP",
  "Area PP vs MM",
  "Area PM vs MP",
  "Area PM vs MM",
  "Area PM vs MM",
  "Average value PP",
  "Average value PM",
  "Average value MP",
  "Average value MM",
  "Average PP vs PM",
  "Average PP vs MP",
  "Average PP vs MM",
  "Average PM vs MP",
  "Average PM vs MM",
  "Average PM vs MM",
  "Delayed Effect PP vs MP",
  "Delayed Effect PP vs MM",
  "Delayed Effect PM vs MP",
  "Delayed Effect PM vs MM",
  "Ave Delayed Eff P vs M"
)
colnames(ContrastCoefficients) <- names(coef(FinalGEEOutput))
ContrastEstimates <- as.vector(ContrastCoefficients%*%GEECoefficients);  # Note the matrix multiplication;
if (WeightsEstimationOption=="estimated") {
  # Calculate the score functions for the logistic regressions, using the original data:
  nsub <- nrow(DataWideFormat);
  Stage1LogisticResiduals <- DataWideFormat$A1DummyCoded - DataWideFormat$p1;
  scoreLogistic1 <- model.matrix(logisticModel1) * as.vector(Stage1LogisticResiduals);
  Stage2LogisticResiduals <- DataWideFormat$A2DummyCoded - DataWideFormat$p2;
  scoreLogistic2 <- matrix(0,nsub,ncol(model.matrix(logisticModel2)));
  scoreLogistic2[which(!is.na(Stage2LogisticResiduals)),] <- model.matrix(logisticModel2) * as.vector(Stage2LogisticResiduals[which(!is.na(Stage2LogisticResiduals))]); 
  # Finish calculating the standard errors, using the replicated data. 
  predictors <- model.matrix(FinalGEEOutput);
  resid <- FinalGEEOutput$residuals;
  nobs <- nrow(predictors);
  fitted <- FinalGEEOutput$fitted.values;
  fitted[which(fitted<.0000000000001)] <- .0000000000001; 
  fitted[which(fitted>.9999999999999)] <- .9999999999999; 
  stopifnot(length(resid)==nobs); # try to make sure nothing has gone wrong;
  meat <- matrix(0,ncol(predictors),ncol(predictors));
  bread <- matrix(0,ncol(predictors),ncol(predictors));
  # "I" and "J" are the notation used  in Xi Lu's code for the two matrices
  # we call "meat" and "bread," respectively. 
  # They don't represent the usual I and J matrices in matrix algebra which 
  #	are the identity (diagonal) and all-ones matrix respectively.  
  # Instead, they are the two matrices in Theorem 1.2 of the supplemental
  # material of Lu (2016).  The matrix "meat" will be an empirical covariance
  # estimator of the score function of the GEE equation, minus a correction 
  # for uncertainty in estimating the weights.  The matrix "bread" will be
  # the naive information matrix, that is, its inverse would be the GEE 
  # model-based covariance estimate of the GEE coefficients.  The sandwich
  # matrix t(bread) * meat * bread will be a better covariance estimate of 
  # the GEE coefficients.
  U <- matrix(0,nsub,ncol(predictors));
  # The rows of U will be set to equal the score function for each subject (derivative of that subject's
  # log-pseudo-likelihood contribution with respect to the GEE coefficients) in the FinalGEEOutput model,
  # with each subject in the original data set having one row. */   
  indicesNextSubject <- 1:nTimes;  
  for (i in 1:nsub) {
    indicesThisSubject <- indicesNextSubject;
    # The code as written assumes that no data are missing, and that the data are sorted by
    # person and then time, with nTimes rows for nonresponders and 2*nTimes rows for responders.
    weightThisSubject <- DataForAnalysis$DesignWeight[indicesThisSubject[1]];
    # The weights are the same for all observations within the 
    # subject, so we just read the first one.
    residualsThisSubject <- resid[indicesThisSubject];
    predictorsThisSubject <- predictors[indicesThisSubject,] ;  
    fittedValuesThisSubject <- fitted[indicesThisSubject];
    GlmWeightThisSubject <- fittedValuesThisSubject*(1-fittedValuesThisSubject); 
    errorThisSubject <- weightThisSubject * t(predictorsThisSubject) %*% 
      diag(as.vector(sqrt(GlmWeightThisSubject)))%*% 
      solve(BlockWorkCorr) %*% 
      diag(as.vector(1/sqrt(GlmWeightThisSubject)))%*%
      residualsThisSubject;
    # errorThisSubject is a k by 1 vector representing this subject's score function.
    # where k=ncol(predictors). ; 
    if (indicesThisSubject[nTimes] == nobs) {
      # This is the case where the current observations are for the last individual,
      # so no more observations are forthcoming. 
      U[i,] <- t(errorThisSubject);
      thisBread <- weightThisSubject * t(predictorsThisSubject)%*%
        diag(as.vector(sqrt(GlmWeightThisSubject)))%*% 
        solve(BlockWorkCorr) %*% 
        diag(as.vector(1/sqrt(GlmWeightThisSubject)))%*%
        predictorsThisSubject; 
      bread <- bread + thisBread;
    } else {
      if (DataForAnalysis$wave[[indicesThisSubject[nTimes]+1]] == 1) {
        # This is the case where the next observations are from an actual new individual. */
        U[i,] <- t(errorThisSubject);
        thisBread <- weightThisSubject * t(predictorsThisSubject)%*%
          diag(as.vector(sqrt(GlmWeightThisSubject)))%*% 
          solve(BlockWorkCorr) %*% 
          diag(as.vector(1/sqrt(GlmWeightThisSubject)))%*%
          predictorsThisSubject; 
        bread <- bread + thisBread;
        indicesNextSubject <- (indicesThisSubject[nTimes]+1):(indicesThisSubject[nTimes]+nTimes); 
      }
      if (DataForAnalysis$wave[[indicesThisSubject[nTimes]+1]] == nTimes+1) {
        # This is the case where the next observations are a replicate of this individual.;
        indicesReplicate <- (indicesThisSubject[nTimes]+1):(indicesThisSubject[nTimes]+nTimes);
        residReplicate <- resid[indicesReplicate];
        predictorsReplicate <- predictors[indicesReplicate,];
        errorReplicate <- weightThisSubject * t(predictorsReplicate)%*%
          diag(as.vector(sqrt(GlmWeightThisSubject)))%*% 
          solve(BlockWorkCorr) %*% 
          diag(as.vector(1/sqrt(GlmWeightThisSubject)))%*%
          residReplicate;
        U[i,] <- t(errorThisSubject + errorReplicate);
        thisBread <- weightThisSubject * t(predictorsThisSubject)%*%
          diag(as.vector(sqrt(GlmWeightThisSubject)))%*% 
          solve(BlockWorkCorr) %*% 
          diag(as.vector(1/sqrt(GlmWeightThisSubject)))%*%
          predictorsThisSubject + 
          weightThisSubject * t(predictorsReplicate)%*%
          diag(as.vector(sqrt(GlmWeightThisSubject)))%*% 
          solve(BlockWorkCorr) %*% 
          diag(as.vector(1/sqrt(GlmWeightThisSubject)))%*%
          predictorsReplicate;
        bread <- bread + thisBread;
        indicesNextSubject <- (indicesReplicate[nTimes]+1):(indicesReplicate[nTimes]+nTimes);
      }
      if ((DataForAnalysis$wave[indicesThisSubject[nTimes]+1] != 1) & 
          (DataForAnalysis$wave[indicesThisSubject[nTimes]+1] != nTimes+1) ) {
        print("Unexpected error in code or dataset;");
        print(indicesThisSubject); 
      }
    }
  }
  bread <- bread/nsub;  # re-express as an average, not a sum, of contributions from participants;
  AdjustedSampleSizeForEmpiricalCovariance <- nsub-ncol(predictors); 
  # some analysts would just use 1/nsub here, 
  # and they are asymptotically equivalent, but this is slightly more conservative in adjusting
  # somewhat for overfitting;
  # If weights were known, then meat would be the empirical covariance matrix 
  # (1/AdjustedSampleSizeForEmpiricalCovariance)*t(U)%*%U.  However, using estimated
  # weights can reduce the covariates, especially if the covariates used to estimate the weights 
  # are good predictors.  To reflect this reduction in error variance, Lu et al (2016, Theorem 1.2) 
  # shows that the covariance estimator should be corrected.  U will be replaced by Uproj, the
  # projection of U onto the space defined by the "hat" or projection matrix of the regression  
  # of the weights on the predictors of the weights.
  scores <- cbind(scoreLogistic1, scoreLogistic2);
  hat <- scores%*%solve(t(scores)%*%scores)%*%t(scores)
  Uproj <- U - hat%*%U;
  meat <- (1/AdjustedSampleSizeForEmpiricalCovariance)*t(Uproj)%*%Uproj;
  invBread <- solve(bread);   
  GEECovarianceMatrix <- (1/nsub)* invBread %*% meat %*% invBread; 
  # This matrix is analogous to the sandwich covariance matrix in standard GEE.
  # See Theorem I.2 in the supplemental material for Lu et al (2016).
}
ContrastCovarianceMatrix <- ContrastCoefficients%*%GEECovarianceMatrix%*%t(ContrastCoefficients);
# This is Cramer's delta method, a simple application of Taylor linearization.
ContrastStdErrors <- sqrt(diag(ContrastCovarianceMatrix)); 
ContrastZRatios <- ContrastEstimates / ContrastStdErrors;
ContrastPValues <- 2*(1-pnorm(abs(ContrastZRatios)));
ContrastUpper95 <- ContrastEstimates + 1.96*ContrastStdErrors;
ContrastLower95 <- ContrastEstimates - 1.96*ContrastStdErrors;
print(cbind(ContrastEstimates, ContrastStdErrors,ContrastZRatios,ContrastPValues,ContrastLower95,ContrastUpper95));

#Individual y3 prediction
# Define indices for Time 3 means
y3_indices <- 4:7

# Extract predictions and inference statistics
y3_means <- ContrastEstimates[y3_indices]
y3_se <- ContrastStdErrors[y3_indices]
y3_z <- ContrastZRatios[y3_indices]
y3_p <- ContrastPValues[y3_indices]
y3_lower <- ContrastLower95[y3_indices]
y3_upper <- ContrastUpper95[y3_indices]

# Organize into a data frame
y3_results <- data.frame(
  Estimate = y3_means,
  StdError = y3_se,
  ZRatio = y3_z,
  PValue = y3_p,
  Lower95 = y3_lower,
  Upper95 = y3_upper
)

# Display results
print(y3_results)
# all estimates into a data frame
binded_data <- data.frame(
  Estimate = ContrastEstimates,
  StdError = ContrastStdErrors,
  ZRatio = ContrastZRatios,
  PValue = ContrastPValues,
  Lower95 = ContrastLower95,
  Upper95 = ContrastUpper95
)
# Experimental code for applet that is not necessary is below, along with some code that was useful. The only super important line is the binded_data2.
# Begin making plots
binded_data2 <- data.frame(cbind(ContrastEstimates, ContrastStdErrors,ContrastZRatios,ContrastPValues,ContrastLower95,ContrastUpper95)) #Line used to create data for analysis in applet
#adding costs
# Named vector of costs
cost_lookup <- c(PP = 450, MP = 550, PM = 100, MM = 200)

# Extract relevant acronym from row names
extract_first_acronym <- function(name) {
  # Check if the string contains "vs" (case-insensitive)
  if (grepl("vs", name, ignore.case = TRUE)) {
    return(NA)
  }
  # Extract the first occurrence of PP, PM, MP, or MM
  matches <- unlist(regmatches(name, gregexpr("PP|PM|MP|MM", name)))
  if (length(matches) > 0) return(matches[1]) else return(NA)
}

# Apply to row names
first_acronyms <- sapply(rownames(binded_data2), extract_first_acronym)

# Assign costs
binded_data2$Cost <- cost_lookup[first_acronyms]

#official plots
binded_data2$ID <- rownames(binded_data2)

# Filter data for Time.3
time3_data <- binded_data2[grepl("^Time\\.3", binded_data2$ID), ]

# Create dot plot for Time.3 with error bars
time3_plot <- ggplot(time3_data, aes(x = ContrastEstimates, y = Cost, color = ID)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ContrastEstimates - ContrastStdErrors, 
                     xmax = ContrastEstimates + ContrastStdErrors), 
                 height = 10) +  # Error bars for standard error
  labs(title = "Time 3: Cost vs Contrast Estimates with Standard Errors",
       x = "Contrast Estimates",
       y = "Cost") +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +  # Y-axis for Cost
  scale_color_hue(name = "ID") +  # Color by ID with default hue palette
  theme_minimal() +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8))  # Adjust legend for readability

# Filter data for Time.6
time6_data <- binded_data2[grepl("^Time\\.6", binded_data2$ID), ]

# Create dot plot for Time.6 with error bars
time6_plot <- ggplot(time6_data, aes(x = ContrastEstimates, y = Cost, color = ID)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ContrastEstimates - ContrastStdErrors, 
                     xmax = ContrastEstimates + ContrastStdErrors), 
                 height = 10) +  # Error bars for standard error
  labs(title = "Time 6: Cost vs Contrast Estimates with Standard Errors",
       x = "Contrast Estimates",
       y = "Cost") +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +  # Y-axis for Cost
  scale_color_hue(name = "ID") +  # Color by ID with default hue palette
  theme_minimal() +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8))  # Adjust legend for readability

# Filter data for Area.under.Curve
area_data <- binded_data2[grepl("^Area\\.under\\.Curve", binded_data2$ID), ]

# Create dot plot for Area.under.Curve with error bars
area_plot <- ggplot(area_data, aes(x = ContrastEstimates, y = Cost, color = ID)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = ContrastEstimates - ContrastStdErrors, 
                     xmax = ContrastEstimates + ContrastStdErrors), 
                 height = 10) +  # Error bars for standard error
  labs(title = "Area under Curve: Cost vs Contrast Estimates with Standard Errors",
       x = "Contrast Estimates",
       y = "Cost") +
  scale_y_continuous(breaks = seq(0, 600, by = 100)) +  # Y-axis for Cost
  scale_x_continuous(breaks = seq(62, 66, by = 0.5)) +  # X-axis for ContrastEstimates
  scale_color_hue(name = "ID") +  # Color by ID with default hue palette
  theme_minimal() +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8))  # Adjust legend for readability
