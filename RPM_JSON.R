library("rjson")
library("miniCRAN")

loadPkgJSON <- function(path) {
  fromJSON(file = normalizePath(path))
}

writePkgJSON <- function(obj) {
  pkgJSON <- toJSON(obj)
  write(pkgJSON, "package.json")
}

listDeps <- function(pkgList = NULL, rVersion, obj) {
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

listLocalPkgs <- function(obj, version) {
  return(pkgAvail(repos = obj$localRepoPath, type = obj$pkgType, Rversion = version)[,1])
}

buildRepo <- function(pkgList, obj) {
  localObj <- obj
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
    # version <- as.character(version)
    deps <- listDeps(pkgList, version, obj)
    partialMakeRepo(pkgs = deps, Rversion = version)
    # Add dependencies list to package.json object
    obj$rVersion[[version]]$depList <- unique(deps)
  }
  writePkgJSON(obj)
  eval.parent(substitute(obj <- ))
  obj -> eval(as.list(match.call())$obj)
  cat("Created repos for the following versions:\n")
  for (version in versions) {
    cat(paste0(version, "\n"))
  }
}

rpmInstall <- function(newPkgs, obj) {
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
    oldPkgList <- listLocalPkgs(obj, version)
    partialAddPackage(version)
    # Get new package list and compare to original package list
    newPkgList <- listLocalPkgs(obj, version)
    pkgDelta[[version]] <- setdiff(newPkgList, oldPkgList)
    obj$rVersion[[version]]$depList <- newPkgList
  }
  obj$masterPkgList <- unique(c(obj$masterPkgList, newPkgs))
  return(pkgDelta)
}
