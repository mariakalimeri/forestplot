# A small R package to make blobbograms for NMR biomarker associations

An R package to plot, in a one- or two-column forestplot layout, associations
of epidemiological analysis on NMR metabolomics biomarker data. The associations
may be linear (linear regression), odds ratios (logistic regression) or hazard
ratios (e.g. Cox Proportional hazards).

Authors:

* Qin Wang (Computational Medicine, Faculty of Medicine, University of Oulu)
* Maria Kalimeri (Nightingale Health Ltd.)

## Install and Usage

* Clone or download the repository.
* Open a terminal window.
* Go to the directory that contains the downloaded package.
* Type

```shell
R CMD INSTALL forestplot
```

(Mind that the name of the package directory may be in principle different than
`forestplot` or `forestplotNMR`. E.g., if you downloaded the repository it may
be `forestplot-master`. Substitute above accordingly.)

The demos subdirectory contains a tutorial in both html and pdf format that will
get you started.

### Directory Layout

forestplot/

* ├── R/: scripts with all the package functions (I prefer one file for each
    function)
* ├── man/: automatically created man files
* ├── data/: library containing data in an R binary format
* ├── demos/: pdf manual, pdf and html tutorial on how to use the package and
example plots (the outputs of the tutorial)
