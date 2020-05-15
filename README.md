[![Build Status](https://travis-ci.org/resplab/fev1.svg?branch=master)](https://travis-ci.org/resplab/fev1)
[![CRAN Status](https://www.r-pkg.org/badges/version/fev1)](https://cran.r-project.org/package=fev1)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# fev1
R package for predicting lung function decline over time in COPD patients as described in https://doi.org/10.1503/cmaj.151483

### Installation

You can download the latest development version from GitHub:

```
install.packages("remotes")
remotes::install_github("resplab/fev1")
```

### Cloud-based API Access
The [PRISM platform](http://prism.resp.core.ubc.ca) allows users to access BODEindex through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for BODEindex please refer to the [PRISM model repository](http://resp.core.ubc.ca/ipress/prism).

#### Linux

In Ubuntu, you can call the API with `curl`:

```
curl -X POST -H "Content-Type: application/json" -d '{"api_key":["123456"],"func":["prism_model_run"],"model_input":[{"ID":"10001","male":1,"age":70,"smoker":1,"FEV1":2.5,"height":1.68,"weight":65}]}' http://prism.resp.core.ubc.ca/ocpu/library/fev1Prism/R/gateway/json
```


### Citation

Please cite: 

```
Zafari Z, Sin DD, Postma DS, Löfdahl CG, Vonk J, Bryan S, Lam S, Tammemagi CM, Khakban R, Man SP, Tashkin D. Individualized prediction of lung-function decline in chronic obstructive pulmonary disease. CMAJ. 2016 Oct 4;188(14):1004-11.
```
