MBAOD: Model Based Adaptive Optimal Design in R
======

The MBAOD package can be used to simulate experiments (often clinical or pre-clinical trials) using predefined adaptation and optimization rules.  The package can be used to plan and evaluate the predicted effectiveness of an upcoming trial. In addition the package can be used to optimize any specific cohort of an actual study using MBAOD.

Adaptive designs (AD), in general, are a way to adapt experiments as your understanding of the system you are studying improves through intermediate analyses of the experimental data.  Adaptive **optimal** design (AOD) uses optimal design theory as a way to design the next stage of your study.  In this package we are assuming the use of **population** models for both the estimation of parameters (analysis of data) as well as for the optimization of the various cohorts of our experiment 

This package currently uses PopED, NONMEM, PsN and R to handle the various tasks inherent to simulating and evaluating an MBAOD experiment.  The code has been written to be (hopefully) quite modular, so that other tools can easily be switched in place of the current tool set (e.g. PFIM instead of PopED). 


## Installation

1. You need to have R installed.  Download the latest version of R from http://www.r-project.org.

2. Install PopED for R (https://github.com/andrewhooker/PopED). To install the latest stable release from CRAN, write at the R command line: 
    
```
    install.packages("PopED")
```
    
3. NONMEM and PsN (http://psn.sf.net) should be installed. 

4. Download the MBAOD package from https://github.com/andrewhooker/MBAOD.  On the right hand side of the page there are links to "Clone in Desktop" and "Download ZIP". Alternatively, if you want to use the latest version of MBAOD but aren't interested in developing the code, you can use the `devtools::install_github()` from the R command line. The `install_github()` 
approach requires that you build a package from source, i.e. `make` and compilers must be installed on your system -- see the R FAQ for your operating system ; you may also need to install dependencies manually:

```
    devtools::install_github("MBAOD",username="andrewhooker")
```

## Getting started

There is a document written to help you get started with the MBAOD package.  The PDF file is located in in the "inst/docs/using_mbaod_pkg" directory of this repository. If you have installed the MBAOD package using the `devtools::install_github()` command then the document can also be found in the "docs/using_mbaod_pkg" directory in the MBAOD installation directory located at:

```
system.file("docs/using_mbaod_pkg", package="MBAOD")
```


