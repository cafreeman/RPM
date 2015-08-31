library("rjson")
library("miniCRAN")

# Constants
setwd("/Users/cfreeman/Google Drive/CF_Work/Projects/Apps and Macros/RPM")
localRepoPath <- "/Users/cfreeman/Desktop/testRepo"
rVersion <- "3.2"
cranRepo <- c(CRAN = "http://cran.us.r-project.org")
pkgType <- "source"

# Read original package list from .txt
pkgs <- readLines(file(file.path(getwd(), "AlteryxPackageList.txt"))

# Create original package.json
# pkgObj <- list(version = rVersion,
#                pkgType = pkgType,
#                pkgList = pkgs
#                )
# pkgJSON <- toJSON(pkgObj)
# write(pkgJSON, "package.json")

# loadPkgJSON <- function(path = pkgJSON) {
#   fromJSON(path)
# }
# Generate list of all non-base/recommended installed packages
installedList <- function() {
  installed <- data.frame(installed.packages(), stringsAsFactors = FALSE)
  installed[!installed$Priority %in% c("base", "recommended"), c("Package", "Version", "Depends", "Imports", "LinkingTo", "Suggests")]
}

# Build/Rebuild Repo
buildRepo <- function(pkgList = NULL) {
  # Get full list of packages from CRAN
  pkgInfo <- pkgAvail(repos = cranRepo, type = pkgType)
  if (!file.exists(localRepoPath)) {
    dir.create(localRepoPath)
  }
  deps <- pkgDep(pkgList, pkgInfo, repos = cranRepo, type = pkgType, suggests = FALSE, Rversion = rVersion)
  makeRepo(deps, localRepoPath, cranRepo, type = pkgType, Rversion = rVersion, download = TRUE, writePACKAGES = TRUE)
}

# Add package to Repo
rpmInstall <- function(newPkgs, path = localRepoPath, repos = cranRepo, type = pkgType, Rversion = rVersion) {
  oldPkgList <- pkgAvail(repos = path, type = pkgType)[,1]
  addPackage(newPkgs, path, repos, type, Rversion)
  pkgDelta <- setdiff(pkgAvail(repos = path, type = pkgType)[,1], oldPkgList)
  # Print out list of newly installed dependencies
  print("The following packages have been installed:\n")
  for (i in pkgDelta) {
    print(pkgDelta[i])
  }
}

# Check for outdated packages in the local miniCRAN repo
rpmOutdated <- function(path = localRepoPath, repos = cranRepo, type = pkgType, Rversion = rVersion, returnOutput = FALSE) {
  outdated <- oldPackages(path = path, repos = repos, method = "auto", type = type, Rversion = rVersion)
  print("The following packages can be updated:\n")
  for (i in outdated[,"Packages"]) {
    print(i)
  }
  if (returnOutput == TRUE) {
    return outdated[,"Packages"]
  }
}

rpmUpdate <- function(updatePkgs, path = localRepoPath, repos = cranRepo, type = pkgType, Rversion = rVersion) {
  currentPkgList <- pkgAvail(path, pkgType, Rversion)[,1]
  updatePackages(path = path, repos = repos, ask = FALSE, oldPkgs = updatePkgs, type = type, Rversion = Rversion)
  newPkgList <- pkgAvail(path, pkgType, Rversion)[,1]
  pkgDelta <- setdiff(newPkgList, currentPkgList)
  print("The following packages have been updated:\n")
  for (i in pkgDelta) {
    print(i)
  }
}
