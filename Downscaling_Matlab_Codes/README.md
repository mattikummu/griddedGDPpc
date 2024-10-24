# Downscaled gridded global dataset for Gross Domestic Product  (GDP) for 1990-2022

The following is a brief description of the MATLAB scripts used to downscale GDP dataset.

## Table of Contents

- [Code Usage](#code-usage)
    - [1. Training machine learning models](#1-training-machine-learning-models)
    - [2. Visulizing errors and predicting adm2 level GDP ratios](#2-visulizing-errors-and-predicting-adm2-level-GDP-ratios)
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
After training, the best model will get stored seperately. `best_model` is the name of the best model and the predictor function is stored in `model`. To predict with `model` use the command: `model(T)`. `T` must be a table containing data for the features used in the training process.

### 2. Visulizing errors and predicting adm2 level GDP ratios

The next section first puts cross-validation prediction and test predictions for the best model together in `allVal`. Then adds each prediction to it's corresponding row in the original data and gathers everything in `dataWithPrediction` variable. The rest of this section consists of crating error plots for each admin-year unit for each of the 12 regions and also the histogram of the errors for each region. The plots are saved as a PDF files named `ErrorPlot.pdf` and  `ErrorHist.pdf` respectively. Finally this section produces a csv file named `adm1_prediction_error.csv` which is the adm1 level data with predictions and errors.

Followed by this section is the final prediction for ADM2 level data which is then corrected by ROM and finally saved in `downsclaling_output.csv` file.
The script ends with saving the workspace to a `mat` file named `TrainedModelsResults.mat`.


## Dependencies

This code was developed and tested using MATLAB version 2024a or later. It also requires the Statistics and Machine Learning Toolbox.
