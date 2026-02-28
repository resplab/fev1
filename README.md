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

### Using the package

The package includes data for two sample patients that can be accessed as `samplePatients`:

```
library(fev1)

samplePatients
# A tibble: 2 x 11
  ID     male   age smoking fev1_0 height weight tio   int_effect   oco fev1_prev
  <chr> <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <chr>      <dbl> <dbl>     <dbl>
1 10001     1    70       1    2.5   1.68     65 No             0   -12         2
2 10002     0    42       0    3.6   1.82     84 Yes            0   -12         2

patient <- samplePatients[1,]

```

To get the predictions, one can simply pass a patient to the function `predictFEV1`:

```
res <- predictFEV1(patient)

print(res)

```
Note that By default, the third prediction models in the paper will be used; however, the user can also choose their preferred model based on availability of the data using the `predictionModel` argument.

Plotting the results:

```

projectionChart(res |> filter(Scenario=="Smoking"))

```


### Citation

Please cite: 

```
Zafari Z, Sin DD, Postma DS, Löfdahl CG, Vonk J, Bryan S, Lam S, Tammemagi CM, Khakban R, Man SP, Tashkin D. Individualized prediction of lung-function decline in chronic obstructive pulmonary disease. CMAJ. 2016 Oct 4;188(14):1004-11.
```
