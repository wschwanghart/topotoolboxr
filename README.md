# topotoolboxr

`topotoolboxr` is an R-package for the analysis of
digital elevation models (DEMs). It is intended to
provide similar functionalities as the MATLAB version
(TopoToolbox 3)[http://github.com/TopoToolbox/topotoolbox3] and the Python version (pytopotoolbox)[http://github.com/TopoToolbox/pytopotoolbox].

# Install as user

Currently, the package is only available via its github repository.
To install, use `devtools`.

```
#install.packages("devtools")
library(devtools)
install_github("TopoToolbox/topotoolboxr")
library(topotoolboxr)
```
The following code should now work.
```
DEM <- GRIDobj(srtm_bigtujunga30m_utm11)
H <- hillshade(DEM)
plot(H, col = "Grays")
```
If you see a hillshade of the Big Tujunga DEM, great!

# Install as a developer

Do you want to contribute to the development of `topotoolboxr`? 
Yes, please. Here's a short introduction on how to get started.

First, fork the main repository and clone it to your local machine.

In R-Studio, install following packages

```
install.packages("devtools")
install.packages("terra")
install.packages("sf")
install.packages(c("roxygen2","usethis", "testthat"))
install.packages(c('colorspace', 'OpenImageR'))
```
Now set the working directory to the cloned topotoolboxr-folder.
```
setwd("~/enter_your_path/topotoolboxr")
```
Run `load_all()` so that R loads the topotoolboxr source code.
This does NOT install the package globally. 
```
library(devtools)
devtools::load_all()
```
You can now edit the code. For example, to open the
hillshade function, enter
```
View(hillshade)
```
Now run `load_all()` again to reflect changes. 
```
devtools::load_all()
```
Any changes to the help text of functions will be written to the
documentation (see folder '\man') using the function `document()`. 
```
devtools::document() 
```
You can now commit and push your edits to your online repository, and
contribute to the main repository.


## Used Tools
- roxigen2
- devtools
