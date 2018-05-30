# A small R package to make blobbograms for NMR biomarker associations

An R package to plot, in a one- or two-column forestplot layout, associations of epidemiological analysis on NMR metabolomics biomarker data. The associations may be linear (linear regression), odds ratios (logistic regression) or hazard ratios (e.g. Cox Proportional hazards).

Authors:

* Qin Wang (Computational Medicine, Faculty of Medicine, University of Oulu)
* Maria Kalimeri (Nightingale Health Ltd.)

## Install and Usage

Open an R session in the parent directory of the forestplot/ folder. If you don't have devtools, install it:
> install.packages("devtools")

then type:

> devtools::install(pkg = "forestplot")

Hopefully, dependencies will be installed automatically. If not let me know. (See also manual in pdf for the list of dependencies.)

The demos subdirectory contains a tutorial in both html and pdf format that will get you started.

### Directory Layout

forestplot/

* ├── R/: scripts with all the package functions (I prefer one file for each function)
* ├── man/: automatically created man files
* ├── data/: library containing data in an R binary format
* ├── demos/: pdf manual, pdf and html tutorial on how to use the package and example plots (the outputs of the tutorial)
