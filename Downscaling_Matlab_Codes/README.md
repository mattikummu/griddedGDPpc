# Downscaled gridded global dataset for Gross Domestic Product  (GDP) for 1990-2022

The following is a brief description of the MATLAB scripts used to downscale GDP dataset.

## Table of Contents

- [Code Usage](#code-usage)
    - [1. Training machine learning models](#1-training-machine-learning-models)
    - [2. Bias Correction and prediction](#2-bias-correction-and-prediction)
- [Dependencies](#dependencies)
- [Installation](#installation)
- [Examples](#examples)
- [Contributing](#contributing)
- [License](#license)


## Code Usage

The code is divided into two segments:
1. Training machine learning models to downscale
2. Applying bias correction methods to the resutls and predicting the ADM2 level GDP ratios.

### 1. Training machine learning models
To train the machine learning models, run the script named `main.m`. To run this code you need to have the data in a `csv` format. Cell variable `features` includes the name of all of the features used including the name of the response variable name. In our case, the features are:
* `gdpRatio` (Response variable): GDP ratio (ratio over admin 1 level value and calculated national value)
* `adm0GDP`:  GDP per capita for Admin 0 level
* `adm0gini`: Income inequality at admin 0 level
* `adm0travelTime`: Travel time to the closest city for Admin 0 level 
* `adm0urb`: Ubanization level for Admin 0 level
* `travelTime`: Travel time to the closest city
* `urb`: Urbanization level

Choose one of the following models to train:
* `EnsembleBag`: Bagged ensemble of decision trees [See in MATLAB Docs](https://www.mathworks.com/help/stats/fitrensemble.html/)
* `EnsembleBoost`: Boosted ensemble of decision trees [See in MATLAB Docs](https://www.mathworks.com/help/stats/fitrensemble.html/)
* `NN3`: Neural network with 3 hidden layers [See in MATLAB](https://www.mathworks.com/help/stats/fitrnet.html)
* `NN2`: Neural network with 2 hidden layers [See in MATLAB](https://www.mathworks.com/help/stats/fitrnet.html)
* `NN1`: Neural network with 1 hidden layers [See in MATLAB](https://www.mathworks.com/help/stats/fitrnet.html)
* `SVMPly`: Regression SVM model with a polynomial kernel [See in MATLAB](https://www.mathworks.com/help/stats/fitrsvm.html)
* `SVMGaussian`: Regression SVM model with a Gaussian kernel [See in MATLAB](https://www.mathworks.com/help/stats/fitrsvm.html)
* `LR`: Linear Regression model with interactions. [See in MATLAB](https://www.mathworks.com/help/stats/fitlm.html?s_tid=doc_ta)

The default option in this script trains all these models. To choose any combination of models, modify the cell variable `models`.
Default division ratios is 80,0, and 20 percents corresponding to train, validation, and test datasets. Training is done using a 10 fold crossvalidation. Final results are stored in the structured array `Results` with fields corresponding to each machine learnin model listed in `models` variable. Function `trainRegressionModel.m` handles the training process using [Byesian optimizaiton](https://www.mathworks.com/help/stats/bayesianoptimization.html?s_tid=doc_ta) to fine tune the hyperparameters for each model, To predict with the trained model stored in variable `Results` use the following command: `Results.EnsembleBag.mdl.predictFcn(T)`. This command will use `EnsembleBag` model to predict using the data provided in table `T`. `T` must be a table containing data for the features used in the training process.
The script ends with saving the workspace to a `mat` file named `TrainedModelsResults.mat`.
### 2. Bias Correction and Prediction

Trained results are loaded from `TrainedModelsResults.mat`. Here the best model of `EnsembleBag` this script is model independent. The bias correction methods, based on the paper by Belitz and Stackelberg (2021) [^1], are as follows and implemented in the function script `bias_corr.m`.
* Empirical distribution matching (EDM)
* Regression of observed on estimated values (ROM)
* Linear transfer function (LTF)
* Z-transform

ROM and LTF outputs are linear regression models outputted as functions. Applying these corrections can be done using the following command:
`corrected_output = predict(MDL, not_corrected_outputs)` where `not_corrected_outputs` are the prediction of the machine learning model and MDL is either ROM and LTF.
To use EDM the following command must be used:
`corrected_output = icdf(F_obs, cdf(F_est, not_corrected_outputs))`. 
To use Z-transform use the following command:
`corrected_output = mzz * not_corrected_outputs + bzz`
`F_obs`, `F_est`, `mzz`, and `bzz` are outputs of the `bias_corr.m` function.

Followed by this section is the final prediction for ADM2 level data which is then corrected by ROM and finally saved in `downsclaling_output.csv` file and also `Corrected.mat`.


## Dependencies

This code was developed and tested using MATLAB version 2023b or later. It also requires the Statistics and Machine Learning Toolbox.


## References
[^1]: Belitz, Kenneth, and P. E. Stackelberg. "Evaluation of six methods for correcting bias in estimates from ensemble tree machine learning regression models." Environmental Modelling & Software 139 (2021): 105006. [DOI](https://doi.org/10.1016/j.envsoft.2021.105006).