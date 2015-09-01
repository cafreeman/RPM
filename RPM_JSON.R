library("rjson")
library("miniCRAN")

loadPkgJSON <- function(path) {
  fromJSON(file = normalizePath(path))
}

writePkgJSON <- function(obj, configPath) {
  pkgJSON <- toJSON(obj)
  write(pkgJSON, configPath)
}

listDeps <- function(pkgList = NULL, rVersion, configPath) {
  obj <- loadPkgJSON(configPath)
  # Get full list of packages from CRAN
  pkgInfo <- pkgAvail(repos = obj$cranRepo, type = obj$pkgType)
  # Partially apply all of the pkgDep function args except pkgList
  partialListDeps <- function(pkgList) {
    pkgDep(pkgList,
           availPkgs = pkgInfo,
           repos = obj$cranRepo,
           type = obj$pkgType,
           suggests = FALSE,
           Rversion = rVersion)
  }
  # Call partialListDeps based on state of pkgList argument
  if (is.null(pkgList)) {
    return(partialListDeps(obj$masterPkgList))
  } else {
    return(partialListDeps(pkgList))
  }
}

listLocalPkgs <- function(version, configPath) {
  obj <- loadPkgJSON(configPath)
  return(pkgAvail(repos = obj$localRepoPath, type = obj$pkgType, Rversion = version)[,1])
}

buildRepo <- function(pkgList, configPath) {
  obj <- loadPkgJSON(configPath)
  # Get list of rVersions
  versions <- names(obj$rVersion)
  # Create local repo dir if it doesn't exist
  if (!file.exists(obj$localRepoPath)) {
    dir.create(obj$localRepoPath)
  }
  partialMakeRepo <- function(pkgs, Rversion) {
    makeRepo(pkgs = pkgs,
             path = obj$localRepoPath,
             repos = obj$cranRepo,
             type = obj$pkgType,
             Rversion = Rversion,
             download = TRUE,
             writePACKAGES = TRUE)
  }
  for (version in versions) {
    deps <- listDeps(pkgList, version, configPath)
    partialMakeRepo(pkgs = deps, Rversion = version)
    # Add dependencies list to package.json object
    obj$rVersion[[version]]$depList <- unique(deps)
  }
  writePkgJSON(obj, configPath)
  cat("Created repos for the following versions:\n")
  for (version in versions) {
    cat(paste0(version, "\n"))
  }
}

rpmInstall <- function(newPkgs, configPath) {
  obj <- loadPkgJSON(configPath)
  versions <- names(obj$rVersion)
  partialAddPackage <- function(Rversion) {
    addPackage(pkgs = newPkgs,
               path = obj$localRepoPath,
               repos = obj$cranRepo,
               type = obj$pkgType,
               Rversion = Rversion)
  }
  pkgDelta <- list()
  for (version in versions) {
    oldPkgList <- listLocalPkgs(version, configPath)
    partialAddPackage(version)
    # Get new package list and compare to original package list
    newPkgList <- listLocalPkgs(version, configPath)
    pkgDelta[[version]] <- setdiff(newPkgList, oldPkgList)
    obj$rVersion[[version]]$depList <- newPkgList
  }
  obj$masterPkgList <- unique(c(obj$masterPkgList, newPkgs))
  writePkgJSON(obj, configPath)
  return(pkgDelta)
}

rpmOutdated <- function(configPath) {
  obj <- loadPkgJSON(configPath)
  outdated <- oldPackages(path = obj$localRepoPath,
                          repos = obj$cranRepo,
                          type = obj$pkgType,
                          Rversion = obj$rVersion)
  cat("The following packages can be updated\n")
  for (i in outdated[,"Packages"]) {
    cat(paste0(i, "\n"))
  }
  return(outdated)
}

rpmUpdate <- function(updatePkgs, configPath) {
  obj <- loadPkgJSON(configPath)
  versions <- names(obj$rVersion)
  for (version in versions) {
    currentPkgList <- listLocalPkgs(version, configPath)
    updatePackages(path = obj$localRepoPath,
                   repos = obj$cranRepo,
                   ask = FALSE,
                   oldPkgs = updatePkgs,
                   type = obj$pkgType,
                   Rversion = version)
    newPkgList <- listLocalPkgs(version, configPath)
    pkgDelta[[version]] <- setdiff(newPkgList, currentPkgList)
  }
  cat("The following packages have been updated:\n")
  for (i in names(pkgDelta)) {
    cat(paste0("R version ", i, "\n"))
    for (j in pkgDelta[[i]]) {
      cat(paste0(j, "\n"))
    }
  }
}
