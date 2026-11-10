library(fs)
library(here)
library(gt)
library(gtsummary)
library(checkmate)
library(dplyr)
#-------------------------------------------------------------------------------
source(path(here(), "R", "utils", "utils_io.R"))
#-------------------------------------------------------------------------------
fit_cDiff <- readFile("fit_tmax_cDiff_spat_1_temp_k30d1_2011.rds")
fit_iDiff <- readFile("fit_tmax_iDiff_spat_1_temp_k30d1_2011")
fit_kron <- readFile("fit_tmax_kron_spat_1_temp_k30d1_2011")

fits <- list(fit_kron, fit_cDiff, fit_iDiff)
names(fits) <- c("Kronecker", "critical Diffusion", "iterative Diffusion")

tblSmryComp <- function(fits) {
  
  checkmate::assertList(fits)
  
  runtimes <- lapply(fits, function(model){model$cpu.used})
  calls <- lapply(fits, function(model){model$misc$nfunc})
  
  #bind_rows(lapply(fits, function(mdl) mdl$cpu.used), .id = "Model")
}


