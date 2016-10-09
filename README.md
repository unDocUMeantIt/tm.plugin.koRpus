# tm.plugin.koRpus

[![Flattr this git repo](https://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=m.eik&url=https://github.com/unDocUMeantIt/tm.plugin.koRpus&title=tm.plugin.koRpus&language=en_GB&tags=github&category=software)

This package provides classes and methods to enhance the ability to use the 'koRpus'
package together with the 'tm' package. It is in its early stages. To ask for help, report
bugs, suggest feature improvements, or discuss the global development of the package, please
subscribe to the koRpus-dev mailing list:
https://ml06.ispgateway.de/mailman/listinfo/korpus-dev_r.reaktanz.de

More information on tm.plugin.koRpus is available on the [project homepage](https://reaktanz.de/?c=hacking&s=koRpus).

## Installation

### Development releases via the project repository

Installation of tha latest stable release is fairly easy, it's available from the project's own repository:

```
install.packages("tm.plugin.koRpus", repo="https://reaktanz.de/R")
```

To automatically get updates, consider adding the repository to your R configuration. You might also
want to subscribe to the package's [RSS feed](https://reaktanz.de/R/pckg/tm.plugin.koRpus/RSS.xml) to get notified of new releases.

If you're running a Debian based operating system, you might be interested in the
[precompiled *.deb packages](https://reaktanz.de/R/pckg/tm.plugin.koRpus/deb_repo.html).

### Installation via GitHub

To install the package directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```
library(devtools)
install_github("unDocUMeantIt/tm.plugin.koRpus")
```

## License

tm.plugin.koRpus Copyright (C) 2016 m.eik michalke, released under the
GNU General Public License (GPL) version 3 or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the license with the
source package as the file COPYING or LICENSE.
