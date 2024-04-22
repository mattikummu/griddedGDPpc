close all
clear
clc

%% Load results and data
load ('TrainedModelsResults.mat','Results') 

ENS = Results.EnsembleBag.mdl.predictFcn;

%% Bias correction for Ensemble
correctedENS = bias_corr(Data, trainingData, testInput, ENS(Data), ENS(trainingData), ...
                test_output.ENS, 'Ensemble', 'gdpRatio');

EDM_r2 = r2(testResponse, correctedENS.EDM(testInd));
ROM_r2 = r2(testResponse, correctedENS.ROM(testInd));
LTF_r2 = r2(testResponse, correctedENS.LTF(testInd));
Z_r2 = r2(testResponse, correctedENS.Z(testInd));

EDM_rmse = rmse(testResponse, correctedENS.EDM(testInd));
ROM_rmse = rmse(testResponse, correctedENS.ROM(testInd));
LTF_rmse = rmse(testResponse, correctedENS.LTF(testInd));
Z_rmse = rmse(testResponse, correctedENS.Z(testInd));

fprintf("RMSE for: \n EDM: %3f \n ROM: %3f \n LTF: %3f \n ZZ: %3f \n", EDM_rmse, ROM_rmse, LTF_rmse, Z_rmse )

fprintf("R2 for: \n EDM: %3f \n ROM: %3f \n LTF: %3f \n ZZ: %3f \n", EDM_r2, ROM_r2, LTF_r2, Z_r2 )


%% Downscaling using bias corrected ensemble
adm2_data = readtable("adm2DataForDownscaling.csv");
prediction = ENS(adm2_data);
correctedPrediction = predict(correctedENS.ROMmdl, prediction);
adm2_data.ENS_Prediction = prediction;
adm2_data.Corrected_Prediction = correctedPrediction;

writetable(adm2_data, 'downsclaling_output.csv')
save('Corrected.mat')
