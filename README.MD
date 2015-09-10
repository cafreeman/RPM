## R Package Manager

An R package management tool, built on MiniCRAN and heavily influenced by the Node.js (NPM package manager)[https://github.com/npm/npm].

RPM is (currently) a set of functions built on top of (miniCRAN)[https://github.com/RevolutionAnalytics/miniCRAN] which allows you create and manipulate a local, CRAN-like repository of R packages. This local repository can track dependencies across different versions of R as well as maintain repositories of different package types (e.g. source code, binaries).

RPM builds on miniCRAN by tracking the state of your repository in a JSON file, similar to NPM's `package.json` convention. This allows the user to perform RPM actions in different R sessions and ensure that the state of RPM repository is maintained across runs. As a result, once a user creates the initial config file (with `rpmInit`), they can then perform other actions on the existing repo solely by providing the path to the config file itself when calling a new RPM function. One of the major benefits of this is that the JSON config object can be version-controlled without having to VC the rest of your repository, since the JSON file will always reflect the most recent state of all your different repositories (across versions of R, for instance).

An initial JSON config file would look like this:
```
{
  "localRepoPath": "~/Desktop/testRepo",
  "cranRepo": {
    "CRAN": "http://cran.us.r-project.org"
  },
  "masterPkgList": ["magrittr", "ggplot2"],
  "pkgType": "source",
  "rVersion": {
    "3.1": {
      "depList": []
    },
    "3.2": {
      "depList": []
    }
  }
}
```

RPM has 4 main actions:

  - `buildRepo`: Create a miniCRAN repo based on the contents of the JSON config file.
  - `rpmInstall`: Add a new package to the miniCRAN repo and download all necessary dependencies
  - `rpmUpdate`: Update packages in the miniCRAN repo to their most recent versions. You can either update all packages, or specify a subset of our current repo.
  - 'rpmUninstall`: Remove a set of packages from the repo. The user need only specify the package they want remove and `rpmUninstall` will find any unique dependencies of that package and delete them as well.

Since the user's local repository will (most likely) change any time one of these actions is performed, the changes are reflected in the JSON config file so that the user can always pick up where they left off.
