rm(list = ls())
library(tidyverse)
library(remotes)
library(renv)
library(reshape2)

if( !("pgBatch" %in% installed.packages())) renv::restore()

 # or use remotes::install_github("rdewijn/pgbatch", ref = "1.6")

library(pgBatch)

input.mat = function(df, vcol = "H_Identity", bcol = "RunID"){
  X = acast(df, ID ~ Barcode + Array, value.var = vcol)
  bx = acast(df, ID ~ Barcode + Array, value.var = bcol)
  result = list(X = X, bx = factor(bx))
}

# edit path to load data
h = read.csv("C:/PamSoft/PamCloud/jcrLocalRoot/PamCloud/Biolizard/CC_CPE/cc_cpe_normalized.txt")
f = read.csv("C:/PamSoft/PamCloud/jcrLocalRoot/PamCloud/Biolizard/CC_CPE/cc_cpe_corrected.txt")

hcc.ref = h %>%
  filter(Study == "METHOD-CC") %>%
  filter(Grouping == "REF")

hcc.das = h %>%
  filter(Study == "METHOD-CC") %>%
  filter(Grouping == "DAS")

# fit a model for run-run in the cc set on the REF data
#Xref = acast(hcc.ref, ID ~ Barcode + Array, value.var = "H_Identity")
#bx.ref = acast(hcc.ref, ID ~ Barcode + Array, value.var = "RunID")[1,]

cc.ref = input.mat(hcc.ref)
cc.model  = pgCombat$new()
cc.model = cc.model$fit(cc.ref$X, cc.ref$bx, mean.only = TRUE)

# Apply to the DAS set
Xcc = acast(hcc.das, ID ~ Barcode + Array, value.var = "H_Identity")
bx = acast(hcc.das, ID ~ Barcode + Array, value.var = "RunID")[1,]
Xcc.corrected = cc.model$apply(Xcc, factor(bx))

# model correction of the CPE set towards the CC set
hcpe.ref = h %>%
  filter(Study == "METHOD-CPE") %>%
  filter(Grouping == "REF")

hcpe.das = h %>%
  filter(Study == "METHOD-CPE") %>%
  filter(Grouping == "DAS")

Xcpe.ref = acast(hcpe.ref, ID ~ Barcode + Array, value.var = "H_Identity")
bx.cpe = acast(hcpe.ref, ID ~ Barcode + Array, value.var = "RunID")[1,]

M = cbind(cc.model$Xc, Xcpe.ref)
bxm = c(rep("cc-ref", dim(cc.model$Xc)[2]), bx.cpe)

cpe.model = pgCombat$new()
cpe.model = cpe.model$fit(M, factor(bxm), mean.only = TRUE, ref.batch = "cc-ref")

Xcpe.das = acast(hcpe.das, ID ~ Barcode + Array, value.var = "H_Identity")
bx = acast(hcpe.das, ID ~ Barcode + Array, value.var = "RunID")[1,]

Xcpe.corrected = cpe.model$apply(Xcpe.das, factor(bx) )


