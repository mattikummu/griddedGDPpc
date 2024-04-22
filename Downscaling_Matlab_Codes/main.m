clear 
close all
clc
%% Loading and cleaning the data
% Loading raw data
RawData = readtable('adm1DataForDownscaling.csv');
% Droping the columns not required in downscaling
RawData(:,[1,2,4]) = [];

features = {'gdpRatio', 'adm0GDP', 'adm0gini', 'adm0travelTime', 'adm0urb', 'travelTime', 'urb'};
targetName = 'gdpRatio';
% Discarding NA
fprintf('Before clearing NAN: nRow = %d, nCol = %d \n',size(RawData,1), size(RawData,2))
idx = zeros(size(RawData));
for i = 1:size(RawData,2)
    idx(:,i) = ~isnan(RawData{:,i});
end
IDX = all(idx==1,2);
Data = RawData(IDX,:);
fprintf('After clearing NAN: nRow = %d, nCol = %d. Data lost = %d \n',...
    size(Data,1),size(Data,2),abs(size(Data,1)-size(RawData,1)))
fprintf('Before outlier clearning: Max= %4f, Min= %4f \n', max(Data.gdpRatio), min(Data.gdpRatio))
% Identifying and removing the outliers
Data = rmoutliers(Data,"percentiles",[0.25 99.75],"DataVariables","gdpRatio");
fprintf('After outlier clearning: Max= %4f, Min= %4f, nRow = %d, nCol = %d. Data lost = %d \n', ...
    max(Data.gdpRatio), min(Data.gdpRatio), size(Data,1), ...
    size(Data,2),abs(size(Data,1)-size(RawData,1)))

%% Training Procress
% Specify the models we want to train:
% models = {'EnsembleBag', 'EnsembleBoost', 'NN3', 'NN2', 'NN1', 'SVMPly', 'SVMGaussian','LR'};
models = {'LR'};
% Dividing the data
[trainInd, ~, testInd] = dividerand(size(Data,1), 0.8,0,0.2);

trainingData = Data(trainInd, features);
trainResponse = Data{trainInd, targetName};
testInput = Data(testInd, features);
testResponse = Data{testInd, targetName};

for alg = models
    tic
    Algorithm = alg{1,1};
    [res.mdl, res.ValAcc, res.valPred, res.OptRes] = ...
    trainRegressionModel(trainingData, targetName, features(2:end), Algorithm);
    Results.(Algorithm) = res;
    tt = toc;
    fprintf('%s - (Runtime = %f)\n',Algorithm, tt)

end

% save('TrainedModelsResults.mat')