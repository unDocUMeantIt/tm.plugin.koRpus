# tm.plugin.koRpus

This package enhances the ['koRpus'](https://reaktanz.de/?c=hacking&s=koRpus) text object classes and methods to also
support large corpora. Hierarchical ordering of corpus texts into arbitrary categories will be preserved.
The pProvided classes and methods also improve the ability of using the 'koRpus' package together with the
['tm'](https://CRAN.R-project.org/package=tm) package.
Not all planned features are fully implemented, but it is already a recommended addition to 'koRpus' of you want to analyze
full text corpora instead of single texts.

More information on tm.plugin.koRpus is available on the [project homepage](https://reaktanz.de/?c=hacking&s=koRpus).

## Installation

### Development releases via the project repository

The latest stable release is available from the project's own repository:

```r
install.packages("tm.plugin.koRpus", repo=c(getOption("repos"), "https://reaktanz.de/R"))
```

To automatically get updates, consider adding the repository to your R configuration. You might also
want to subscribe to the package's [RSS feed](https://reaktanz.de/R/pckg/tm.plugin.koRpus/RSS.xml) to get notified of new releases.

If you're running a Debian based operating system, you might be interested in the
[precompiled *.deb packages](https://reaktanz.de/R/pckg/tm.plugin.koRpus/deb_repo.html).

### Installation via GitHub

To install the package directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/r-lib/devtools) package:

```r
devtools::install_github("unDocUMeantIt/tm.plugin.koRpus") # stable release
devtools::install_github("unDocUMeantIt/tm.plugin.koRpus", ref="develop") # development release
```

## Contributing

To ask for help, report bugs, suggest feature improvements, or discuss the global
development of the package, please either subscribe to the
[koRpus-dev mailing list](https://korpusml.reaktanz.de), or
use the [issue tracker](https://github.com/unDocUMeantIt/tm.plugin.koRpus/issues) on GitHub.

### Branches

Please note that all development happens in the `develop` branch. Pull requests against the `master`
branch will be rejected, as it is reserved for the current stable release.

## License

Copyright 2015-2020 Meik Michalke <meik.michalke@hhu.de>

tm.plugin.koRpus is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

tm.plugin.koRpus is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with tm.plugin.koRpus.  If not, see <https://www.gnu.org/licenses/>.
