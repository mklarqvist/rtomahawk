[![Release](https://img.shields.io/badge/Release-beta_0.1.0-blue.svg?logo=R&logoColor=white)](https://github.com/mklarqvist/rtomahawk/releases)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/mklarqvist/rtomahawk/blob/master/LICENSE)
[![Docs](https://img.shields.io/badge/Docs-Available-green.svg)](https://mklarqvist.github.io/tomahawk/r-tutorial/)

# rtomahawk

This package provides native R-bindings for interfacing with
[Tomahawk](https://github.com/mklarqvist/tomahawk/) libraries and provides
additional graphical functionality. This means exposing most of the features and
flexibility of the C++ API while not sacrificing the usability that R provides.

| GWAS plots | Dense LD data | Square LD data |
|---|---|---|
|![screenshot](twk_locuszoom_combine_genes.jpeg)|![screenshot](twk_plotLD_triangular_truncate.jpeg)|![screenshot](twk_plotLD_viridis_quad.jpeg)|



## Get started

* Read the [documentation](https://mklarqvist.github.io/tomahawk/r-tutorial/)

## Installation
For Ubuntu, Debian, and Mac systems, installation is easy: just run
```bash
git clone --recursive https://github.com/mklarqvist/rtomahawk
R CMD INSTALL --with-keep.source rtomahawk
```

Check that the package can be loaded and list the built libraries (in `R`):
```R
library(rtomahawk)
tomahawkVersion()
```
This will print a version string to the console:
```text
rtomahawk: 0.1.0
Libraries: tomahawk-0.7.0; ZSTD-1.3.1; htslib 1.9
```


### Contributing

Interested in contributing? Fork and submit a pull request and it will be
reviewed.

### Support
We are actively developing Tomahawk and are always interested in improving its
quality. If you run into an issue, please report the problem on our Issue
tracker. Be sure to add enough detail to your report that we can reproduce the
problem and address it. We have not reached version 1.0 and as such the function
names may change.

### Version
This is rtomahawk 0.1.0. rtomahawk follows [semantic
versioning](https://semver.org/).

### Author
Marcus D. R. Klarqvist (<mk819@cam.ac.uk>)  
Department of Genetics, University of Cambridge  
Wellcome Sanger Institute


### License
rtomahawk is licensed under [MIT](LICENSE)  
tomahawk is licensed under
[MIT](https://github.com/mklarqvist/tomahawk/blob/master/LICENSE)