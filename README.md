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

samplePatients[1,]
# A tibble: 1 x 11
  ID     male   age smoking fev1_0 height weight tio   int_effect   oco fev1_prev
  <chr> <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <chr>      <dbl> <dbl>     <dbl>
1 10001     1    70       1    2.5   1.68     65 No             0   -12         2
```

To get the predictions, one can simply pass a patient to the function `predictFEV1`:

```
predictFEV1(samplePatients[1,])
   Year     FEV1   variance FEV1_lower FEV1_upper     Scenario
1     0 2.500000 0.00000000  2.5000000   2.500000      Smoking
2     1 2.386493 0.01558353  2.1418180   2.631167      Smoking
3     2 2.319171 0.01812348  2.0553090   2.583033      Smoking
4     3 2.250969 0.02218849  1.9590117   2.542927      Smoking
5     4 2.181888 0.02777856  1.8552164   2.508559      Smoking
6     5 2.111926 0.03489370  1.7458009   2.478051      Smoking
7     6 2.041084 0.04353390  1.6321347   2.450034      Smoking
8     7 1.969363 0.05369916  1.5151704   2.423555      Smoking
9     8 1.896761 0.06538949  1.3955622   2.397960      Smoking
10    9 1.823279 0.07860489  1.2737628   2.372796      Smoking
11   10 1.748918 0.09334534  1.1500893   2.347746      Smoking
12   11 1.673676 0.10961087  1.0247685   2.322584      Smoking
13   12 1.597554 0.12740145  0.8979650   2.297144      Smoking
14   13 1.520553 0.14671710  0.7698009   2.271305      Smoking
15   14 1.442671 0.16755781  0.6403681   2.244974      Smoking
16   15 1.363909 0.18992359  0.5097371   2.218082      Smoking
17    0 2.500000 0.00000000  2.5000000   2.500000 QuitsSmoking
18    1 2.516653 0.01558353  2.2719780   2.761327 QuitsSmoking
19    2 2.474851 0.01812348  2.2109890   2.738713 QuitsSmoking
20    3 2.432169 0.02218849  2.1402117   2.724127 QuitsSmoking
21    4 2.388608 0.02777856  2.0619364   2.715279 QuitsSmoking
22    5 2.344166 0.03489370  1.9780409   2.710291 QuitsSmoking
23    6 2.298844 0.04353390  1.8898947   2.707794 QuitsSmoking
24    7 2.252643 0.05369916  1.7984504   2.706835 QuitsSmoking
25    8 2.205561 0.06538949  1.7043622   2.706760 QuitsSmoking
26    9 2.157599 0.07860489  1.6080828   2.707116 QuitsSmoking
27   10 2.108758 0.09334534  1.5099293   2.707586 QuitsSmoking
28   11 2.059036 0.10961087  1.4101285   2.707944 QuitsSmoking
29   12 2.008434 0.12740145  1.3088450   2.708024 QuitsSmoking
30   13 1.956953 0.14671710  1.2062009   2.707705 QuitsSmoking
31   14 1.904591 0.16755781  1.1022881   2.706894 QuitsSmoking
32   15 1.851349 0.18992359  0.9971771   2.705522 QuitsSmoking
```
By default, the third prediction models in the paper will be used, however, the user can also choose their preferred model based on availablity of the data using the `predictionModel` argument: 

```
predictFEV1(patientData, onePatient = TRUE, predictionModel = 3) 
```
### Web app
A web app is available at [http://resp.core.ubc.ca/ipress/FEV1Pred](http://resp.core.ubc.ca/ipress/FEV1Pred)

### Cloud-based API Access

The [PRISM platform](http://prism.resp.core.ubc.ca) allows users to access FEV1 through the cloud. A MACRO-enabled Excel-file can be used to interact with the model and see the results. To download the PRISM Excel template file for BODEindex please refer to the [PRISM model repository](http://resp.core.ubc.ca/ipress/prism).

#### Linux

In Ubuntu, you can call the API with `curl`:

```
curl \
-X POST \
-H "x-prism-auth-user: REPLACE_WITH_API_KEY" \
-H "Content-Type: application/json" \
-d '{"func":["prism_model_run"],"model_input":[{"male":1,"age":70,"smoker":1,"FEV1":2.5,"height":1.68,"weight":65}]}' \
https://admin-prism-api.cp.prism-ubc.linaralabs.com/route/fev1/run

```


### Citation

Please cite: 

```
Zafari Z, Sin DD, Postma DS, Löfdahl CG, Vonk J, Bryan S, Lam S, Tammemagi CM, Khakban R, Man SP, Tashkin D. Individualized prediction of lung-function decline in chronic obstructive pulmonary disease. CMAJ. 2016 Oct 4;188(14):1004-11.
```
