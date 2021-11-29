runwithout_sild <- function (fit, year = NULL, fleet = NULL, map = fit$obj$env$map, 
          ...) 
{
  data <- stockassessment::reduce(fit$data, year = year, fleet = fleet, conf = fit$conf)
  conf <- attr(data, "conf")
  fakefile <- file()
  sink(fakefile)
  saveConf(conf, file = "")
  sink()
  conf <- loadConf(data, fakefile, patch = TRUE)
  close(fakefile)
  par <- defpar(data, conf)
  par$logSdLogN = c(-0.35, -5)
  par[!names(par) %in% c("logN", "logF", "logSW", "logCW", 
                         "logitMO", "logNM")] <- fit$pl[!names(fit$pl) %in% c("missing", 
                                                                              "logN", "logF", "logSW", "logCW", "logitMO", "logNM")]
  
  map <- list(logSdLogN = as.factor(c(0,NA)))
  ret <- sam.fit(data, conf, par,map = map)
  return(ret)
}
leaveout_sild <- function (fit, fleet = as.list(2:fit$data$noFleets), ncores = detectCores(), 
          ...) 
{
  if (ncores > 1) {
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl))
    clusterExport(cl, varlist = "fit", envir = environment())
    lib.ver <- dirname(path.package("stockassessment"))
    clusterExport(cl, varlist = "lib.ver", envir = environment())
    clusterEvalQ(cl, {
      library(stockassessment, lib.loc = lib.ver)
    })
    runs <- parLapply(cl, fleet, function(f) runwithout_sild(fit, 
                                                        fleet = f, ...))
  }
  else {
    runs <- lapply(fleet, function(f) runwithout_sild(fit, fleet = f, 
                                                 ...))
  }
  converg <- unlist(lapply(runs, function(x) x$opt$conv))
  if (any(converg != 0)) 
    warning(paste0("leavout run(s) ", paste0(which(converg != 
                                                     0), collapse = ","), " did not converge."))
  names(runs) <- paste0("w.o. ", lapply(fleet, function(x) paste(attr(fit$data, 
                                                                      "fleetNames")[x], collapse = " and ")))
  attr(runs, "fit") <- fit
  class(runs) <- "samset"
  runs
}
