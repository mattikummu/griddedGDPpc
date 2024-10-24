clear 
close all
clc

%% Loading and cleaning the data
% Loading raw data
RawData = readtable('adm1DataForDownscaling.csv'); 
% Dropping the columns not required for downscaling
RawData(:,[1,2,4]) = []; 

features = {'gdpRatio', 'adm0GDP', 'adm0gini', 'adm0travelTime', 'adm0urb', 'travelTime', 'urb'};
targetName = 'gdpRatio';
Data = RawData;
% Printing the size of the data after removing NaN values
fprintf('After clearing NAN: nRow = %d, nCol = %d. Data lost = %d \n', size(Data,1),size(Data,2),abs(size(Data,1)-size(RawData,1)))
fprintf('Before outlier clearning: Max= %4f, Min= %4f \n', max(Data.gdpRatio), min(Data.gdpRatio))
% Identifying and removing the outliers
Data = rmoutliers(Data,"percentiles",[0.25 99.75],"DataVariables","gdpRatio");
% Printing the size of the data and max and min of gdpRatio after outlier removal
fprintf('After outlier clearning: Max= %4f, Min= %4f, nRow = %d, nCol = %d. Data lost = %d \n', max(Data.gdpRatio), min(Data.gdpRatio), size(Data,1), size(Data,2),abs(size(Data,1)-size(RawData,1)))

%% Training Process
% Specify the models we want to train:
models = {'EnsembleBag','EnsembleBoost', 'NN2', 'SVMGaussian','LR'};
trainingData = [];
trainResponse = [];
testInput = [];
testResponse = [];

% Split the data into training and testing data for each RegionID
for c = 1:12
    clip = Data(Data.RegionID == c, :);
    target = clip.gdpRatio;
    [trainInd, ~, testInd] = dividerand(size(clip,1), 0.8,0,0.2);
    trainingData = [trainingData; clip(trainInd, :)];
    trainResponse = [trainResponse; clip{trainInd, targetName}];
    testInput = [testInput; clip(testInd, :)];
    testResponse = [testResponse; clip{testInd, targetName}];
end

% Train the model
for alg = models
    tic
    Algorithm = alg{1,1};
    % Trains a regression model using the training data, features and target
    [res.mdl, res.ValAcc, res.valPred, ~] = trainRegressionModel(trainingData, targetName, features(2:end), Algorithm);
    Results.(Algorithm) = res;
    tt = toc;
    % Print the algorithm used, its runtime and the RMSE
    fprintf('%s - (Runtime = %f) and the RMSE is: %f\n',Algorithm, tt, res.ValAcc)
end

% Find the best model
fields = fieldnames(Results);
best_model = fields{1}; % Initialize with first field
best_model_acc = Results.(fields{1}).ValAcc; % Initialize with first ValAcc

for i = 2:length(fields)
    if Results.(fields{i}).ValAcc < best_model_acc
        best_model = fields{i};
        best_model_acc = Results.(fields{i}).ValAcc;
    end
end

% Select the model with best accuracy
model = Results.(best_model).mdl.predictFcn;
%% Error plots
% Get cross validation and test predictions together.

allVal = [Results.(best_model).valPred; model(testInput)];

% Find unique regions names
roi = cell(12,1);

for i = 1:12
    a = Data.RegName(Data.RegionID == i);
    roi{i,1} = a{1,1};
end

% Add corss validation predictions to the data
for k = 1:size(Data,1)
    idx = ismember(Data,dataWithPrediction(k,1:11),'rows');
    dataWithPrediction.prediction(k) = allVal(k);
end

% Calculate absolute error
dataWithPrediction.absolute_error = dataWithPrediction.gdpRatio - dataWithPrediction.prediction;

% Plot error for each admin-year unit for each region
figure
tiledlayout(4,3)
for i = 1:12
    clip = dataWithPrediction(dataWithPrediction.RegionID == i, :);
    target = clip.gdpRatio;
    prediction = clip.ens_out;
    errors=target-prediction;
    RMSE=sqrt(mean(errors(:).^2));
    error_mean=mean(errors(:));
    error_std=std(errors(:));
    nexttile
    plot(errors);
    formated = '%s \n RMSE = %.2f';
    txt = sprintf(formated, roi{i,1}, RMSE);
    title(txt);

    ylabel('Predicted - Target')
     ylim([-1, 3.5])
end

print('ErrorPlot','-dpdf','-bestfit')

% Plot error histogram for each region
figure
tiledlayout(4,3)
for i = 1:12
    clip = dataWithPrediction(dataWithPrediction.RegionID == i, :);
    target = clip.gdpRatio;
    prediction = clip.ens_out;
    errors=target-prediction;
    RMSE=sqrt(mean(errors(:).^2));
    error_mean=mean(errors(:));
    error_std=std(errors(:));
    nexttile
    h = histogram(errors);
    h.BinWidth = 0.05;
    formated = '%s \n Mean = %.2f, STD = %.2f';
    txt = sprintf(formated, roi{i,1}, error_mean, error_std);
    title(txt);

    ylabel('Frequency')
    xlim([-1, 2])
end
print('ErrorHist','-dpdf','-bestfit')

writetable(dataWithPrediction, 'adm1_prediction_error.csv')

%% Downscaling prediction
% Load admin2 level data
adm2_data = readtable("adm2DataForDownscaling.csv");

% Predict gdpRatio for admin2 level data
prediction = model(adm2_data);

% Add the predictions to the data
adm2_data.Prediction = prediction;

% Save the data
writetable(adm2_data, 'downsclaling_output.csv')
save('TrainedModelsResults.mat')