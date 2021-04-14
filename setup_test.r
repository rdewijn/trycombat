rm(list = ls())
library(tidyverse)
library(remotes)
library(renv)
library(reshape2)

if( !("pgBatch" %in% installed.packages())) renv::restore()

 # or use remotes::install_github("rdewijn/pgbatch", ref = "1.6")

library(pgBatch)

# helper to create input matrix for combat
input.mat = function(df, vcol = "H_Identity", bcol = "RunID"){
  X = acast(df, ID ~ Barcode + Array, value.var = vcol)
  bx = acast(df, ID ~ Barcode + Array, value.var = bcol)[1,]
  result = list(X = X, bx = factor(bx))
}

# edit path to load data
h = read.csv("C:/PamSoft/PamCloud/jcrLocalRoot/PamCloud/Biolizard/CC_CPE/cc_cpe_normalized.txt") %>%
  filter( !(Barcode == "650055110" & Array == "A2"))
f = read.csv("C:/PamSoft/PamCloud/jcrLocalRoot/PamCloud/Biolizard/CC_CPE/cc_cpe_corrected.txt")

# fit a model for run-run in the cc set on the REF data
cc.ref = h %>%
  filter(Study == "METHOD-CC") %>%
  filter(Grouping == "REF") %>%
  input.mat()

cc.model  = pgCombat$new()
cc.model = cc.model$fit(cc.ref$X, cc.ref$bx, mean.only = TRUE)

# Apply to the DAS set
cc.das = h %>%
  filter(Study == "METHOD-CC") %>%
  filter(Grouping == "DAS") %>%
  input.mat()

Xcc.corrected = cc.model$apply(cc.das$X, cc.das$bx)

# CPE towards CC
cpe.ref = h %>%
  filter(Study == "METHOD-CPE") %>%
  filter(Grouping == "REF") %>%
  input.mat()

R = cc.model$apply(cc.ref$X, cc.ref$bx) # ref data for CPE
br = rep("cc-ref", dim(R)[2])
M = cbind(R, cpe.ref$X)
bxm = c(br, as.character(cpe.ref$bx)) %>% as.factor()

cpe.model = pgCombat$new()
cpe.model = cpe.model$fit(M, bxm, mean.only = TRUE, ref = "cc-ref")

cpe.das = h %>%
  filter(Study == "METHOD-CPE") %>%
  filter(Grouping == "DAS") %>%
  input.mat()

Xcpe.corrected = cpe.model$apply(cpe.das$X, cpe.das$bx)

# check workflow
# Note: some difference between the exported result and the result created here ...

cpe.corrected = f %>%
  filter(Study == "METHOD-CPE") %>%
  filter(Grouping == "DAS") %>%
  input.mat(vcol =   "CPECCCor_Identity")

MC = cpe.corrected$X
delta = Xcpe.corrected - MC
hist(delta)







