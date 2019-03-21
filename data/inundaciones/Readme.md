A model for flooding in Mexico City was adjusted using the XGBoost algorithm (see [here](http://uc-r.github.io/gbm_regression) for an explanation of how it works) based on reports provided by SACMEX. A model was adjusted (the model's development and evaluation of  fit can be seen in the document `GBM_inundaciones.Rmd`) for each of the 9 regions defined previosuly (based on expert criteria). The adjusted models were then saved in `.rda` format, which are the objects used to predict floods within the MEGADAPT APP.

The inputs of the model are the following (must be specified by AGEB):

* `f_prec_v`: precipitation volume.
* `f_esc`: volume of runoff.
* `n_tramos`: number of drainage sections.
* `q100`: eviction capacity.
* `bombeo_tot`: number of pumping plants.
* `rejillas`: number of grids.

After selecting values for each variable, the model of the region corresponding to the simulated AGEB can be used to predict the expected frequency of flooding in that area. For example:

```
# Creation of test data

datos_prueba <- structure(list(f_prec_v = 353947,
                               f_esc = 0,
                               n_tramos = 194,
                               q100 = 144,
                               bombeo_tot = 0,
                               rejillas = 88),
                          row.names = c(NA, -1L), class = c("data.frame"))


# Prediction using the model of the corresponding region (in this case, region 1)

predict(modelo_in_region_1,  # model corresponding to region 1
        datos_prueba,        # data frame with selected values for each variable
        n.trees = 9566,      # number of trees used to fit the model (fixed)
         type = "response")
```
