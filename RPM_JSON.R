library("rjson")
library("miniCRAN")
library("stringr")

# convert a new-line separated list to a character vector
newLinesToChar <- function(list, fixed = FALSE) {
  str_trim(as.character(strsplit(list, "\\n", fixed = fixed)[[1]]), "both")
}

# load JSON config from file into an R object
loadPkgJSON <- function(path) {
  fromJSON(file = normalizePath(path))
}

# convert config object to JSON and save to file
writePkgJSON <- function(obj, configPath) {
  pkgJSON <- toJSON(obj)
  dirPath <- dirname(configPath)
  if (!dir.exists(dirPath)) {
    dir.create(dirPath, recursive = TRUE)
  }
  write(pkgJSON, configPath)
}

}

# Find all dependencies for a given list of packages. If no list is provided, find dependencies for
# all local packages
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

# List all packages in the local repopsitory
listLocalPkgs <- function(version, configPath) {
  obj <- loadPkgJSON(configPath)
  return(as.character(pkgAvail(repos = obj$localRepoPath, type = obj$pkgType, Rversion = version)[,1]))
}

# initialize config object and save as JSON
rpmInit <- function(localRepoPath,
                    cranRepo = "http://cran.us.r-project.org",
                    masterPkgList,
                    pkgType,
                    versions,
                    savePath) {
  obj <- list()
  obj$localRepoPath <- localRepoPath
  obj$cranRepo <- c(CRAN = cranRepo)
  obj$masterPkgList <- as.character(masterPkgList)
  obj$pkgType <- pkgType
  obj$rVersion <- list()
  for (version in versions) {
    obj$rVersion[[version]] <- list(depList = character())
  }
  writePkgJSON(obj, savePath)
}

# Construct a local repo from a list of packages. buildRepo() finds all package dependencies,
# downloads the appropriate files from CRAN, and creates a CRAN-like repo in the directory specified
# in the config object
buildRepo <- function(pkgList = NULL, configPath) {
  obj <- loadPkgJSON(configPath)
  # Get list of rVersions
  if (is.null(pkgList)) {
    pkgList <- obj$masterPkgList
  }
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
    deps <- listDeps(pkgList, version, obj)
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

# Add a new package to the local repo, find all dependencies, and download everything.
# Updates package manifest and config object
rpmInstall <- function(newPkgs, configPath) {
  obj <- loadPkgJSON(configPath)
  alreadyInstalled <- Filter(function(pkg) {
    pkg %in% obj$masterPkgList
  }, newPkgs)
  newPkgs <- newPkgs[!newPkgs %in% alreadyInstalled]
  if (length(newPkgs) == 0) {
    stop("All of the provided packages have already been installed.")
  }
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
  if (length(alreadyInstalled) > 0) {
    cat("The following packages were already installed and therefore were not downloaded:\n")
    for (i in alreadyInstalled) {
      cat(paste0(i, "\n"))
    }
  }
  cat("The following packages have been installed:")
  for (i in names(pkgDelta)) {
    cat(paste0(i, ":\n"))
    for (j in pkgDelta[[i]]) {
      cat(paste0(j, "\n"))
    }
  }
  return(pkgDelta)
}

# Check for outdated packages in the local repo
rpmOutdated <- function(configPath) {
  obj <- loadPkgJSON(configPath)
  versions <- names(obj$rVersion)
  outdated <- list()
  for (version in versions) {
    outdated[[version]] <- oldPackages(path = obj$localRepoPath,
                            repos = obj$cranRepo,
                            type = obj$pkgType,
                            Rversion = version)
  }
  cat("The following packages can be updated\n")
  for (i in names(outdated)) {
    cat(paste0(i, ":\n"))
    for (j in outdated[[version]]) {
      cat(paste0(j, "\n"))
    }
  }
  return(outdated)
}

# Update a given set of packages to their latest version
rpmUpdate <- function(updatePkgs, configPath) {
  obj <- loadPkgJSON(configPath)
  versions <- names(obj$rVersion)
  pkgDelta <- list()
  partialUpdatePackages <- function(Rversion) {
    updatePackages(path = obj$localRepoPath,
                   repos = obj$cranRepo,
                   ask = FALSE,
                   oldPkgs = updatePkgs,
                   type = obj$pkgType,
                   Rversion = Rversion)
  }
  for (version in versions) {
    currentPkgList <- listLocalPkgs(version, configPath)
    partialUpdatePackages(version)
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
