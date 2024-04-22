function o = bias_corr(data, traindata, testdata, outputs, trainout, testout, model_name, response)
    % Bias correction method described in https://doi.org/10.1016/j.envsoft.2021.105006
    % Inputs:
    % data: a table containing all data
    % traindata: a table containing data used in the training process 
    % testdata: a table containing data used in testing
    % outputs: all outputs by the model 
    % trainout: model outputs for training dataset 
    % testout: model outputs for testing dataset
    % model_name: name of the model
    % response: respinse variable name
    % Outputs:
    % o: a structure containing outputs for different bias correction
    % methods. misc includes the parameters of the calibrated bias
    % correction method.

    targets = data.(response)(:);
    outputs = outputs(:);
    trainTarget = traindata.(response);
    testTarget = testdata.(response);

    mse = @(t, o) mean(sum((t-o).^2));
    rmse = @(t, o) sqrt(mse(t,o));
    % sum of errors
    err = targets - outputs;
    trainerr = trainTarget - trainout;
    testerr = testTarget - testout;
    disp('For the entire dataset:')
    disp('')
    disp(['Error sum: ', num2str(sum(err))])
    disp(['Absolute error sum: ', num2str(sum(abs(err)))])
    disp(['ME: ', num2str(mean(err)), ' MSE: ', num2str(mean(err.^2)), ' RMSE: ', num2str(sqrt(mean(err.^2)))])
    disp('')
    disp('For training dataset:')
    disp('')
    disp(['Error sum: ', num2str(sum(trainerr))])
    disp(['Absolute error sum: ', num2str(sum(abs(testerr)))])
    disp(['ME: ', num2str(mean(trainerr)), ' MSE: ', num2str(mean(trainerr.^2)), ' RMSE: ', num2str(sqrt(mean(trainerr.^2)))])
    disp('')
    disp('For test dataset:')
    disp('')
    disp(['Error sum: ', num2str(sum(testerr))])
    disp(['Absolute error sum: ', num2str(sum(abs(testerr)))])
    disp(['ME: ', num2str(mean(testerr)), ' MSE: ', num2str(mean(testerr.^2)), ' RMSE: ', num2str(sqrt(mean(testerr.^2)))])
    %% Empirical cumulative distribution
    % Entire dataset
    figure;
    subplot(3,1,1)
    ecdf(targets)
    hold on
    ecdf(outputs)
    hold off
    legend({'Observed',model_name})
    title('All Data')
    xlabel('Data')
    ylabel('Cumulative probability')
    grid on
    % Training dataset
    subplot(3,1,2)
    ecdf(trainTarget)
    hold on
    ecdf(trainout);
    hold off
    title('Train Data')
    xlabel('Data')
    ylabel('Cumulative probability')
    grid on
    % Test dataset
    subplot(3,1,3)
    ecdf(testTarget)
    hold on
    ecdf(testout)
    hold off
    title('Test Data')
    xlabel('Data')
    ylabel('Cumulative probability')
    grid on
    print(model_name, '-dsvg')
    %% Empirical distribution matching (EDM)
    % Fit a non-parameteric probability distribution for observed and estimated
    % values.
    F_est = fitdist(trainout,'kernel','kernel','normal','support','unbounded');
    F_obs = fitdist(trainTarget,'kernel','kernel','normal','support','unbounded');
    Qobs = icdf(F_obs,0:0.01:1);
    Qest = icdf(F_est,0:0.01:1);
    ef = Qobs(2:length(Qobs)-2) - Qest(2:length(Qobs)-2);
    RMSE_f = rmse(Qobs(2:length(Qobs)-2), Qest(2:length(Qobs)-2));
    Bias_f = mean(ef);
    output_edm = icdf(F_obs, cdf(F_est, outputs));
    trainoutput_edm = icdf(F_obs, cdf(F_est, trainout));
    %% Regression of observed on estimated values (ROM)
    ROMmdl = fitlm(trainout, trainTarget);
    output_reg = predict(ROMmdl, outputs);

    %% Linear trasfer function (LTF)
    LTFmdl = fitlm(trainout, trainoutput_edm);
    output_LTF = predict(LTFmdl, outputs);

    %% Z-transform
    mz = sqrt(var(trainTarget))/sqrt(var(trainout));
    bz = mean(trainTarget) - mz * mean(trainout);
    output_z = mz * outputs + bz;

    %% Export results

    o.EDM = output_edm;
    o.ROM = output_reg;
    o.LTF = output_LTF;
    o.Z = output_z;
    o.ROMmdl = ROMmdl;
    o.LTFmdl = LTFmdl;
    o.F_est=F_est; o.F_obs=F_obs; o.Qobs=Qobs; o.Qest=Qest; o.mzz=mz; o.bzz=bz;

end
