% Analyse acoustic stimuli: Temporal & Spectral Power spectra
clear; clc
addpath(genpath('/brainstorm3/'));
addpath(genpath('/utils/'))

origDir ='/stim/'
brainstorm nogui

% key parameters
norm = 1; % normalize volume (1|0)
fs = 44100;
fq = .2:.2:50; % 32 cf
cd([origDir]); stim = dir('*.wav');stim = dir('*.wav');
nfilt = 32;
X1  = NaN(length(stim),     4e4);
stim_name = {};
for j0 = 1:length(stim)
    stim_name{j0} = stim(j0).name;

    % load data - mono - normalize volume
    [in, sr] = audioread(stim(j0).name);
    if sr ~= fs; fprintf('problem'); pause; end
    in = in(:,1);
    if norm == 1; in=in./sqrt(mean(in.^2)); end

    % a. compute envelope w. vocoder - resample - sum env
    [env, ~, ~, deltan] = vocoder(in, 80, nfilt, 1, fs); % env
    env = sum(env, 1); % group filters        
    env = env(max(deltan+1):end); % remove delay   
   % 1e. temporal modulation
    fstf = 100; xcut = 2;
    O  = [];
        O.Method      = 'morlet'; O.Output = 'all'; O.Comment = 'acousticTF';
        O.ListFiles   = []; O.iTargetStudy = []; O.TimeVector = []; % in ms
        O.SensorTypes = []; O.RowNames = {1:length(fq)}; O.Freqs = fq;
        O.TimeBands   = []; O.MorletFc = 1; O.MorletFwhmTc = 3;
        O.Measure     = 'power'; O.ClusterFuncTime = 'none';
    tfenv = resample(env, fstf, fs);
    tfenv = cat(2, zeros(size(tfenv,1),xcut*fstf), tfenv, zeros(size(tfenv,1),xcut*fstf));    
    O.TimeVector = (1:length(tfenv))/fstf;
    tfenv = bst_timefreq(tfenv ,O); tfenv = squeeze(tfenv{1}.TF); bst_progress('stop');
        tfenv = tfenv(xcut*fstf+1:end-xcut*fstf,:);
        tfenv = sqrt(tfenv);
    x = mean(tfenv, 1);
    n = length(x);
    X1(j0,1:n) = x; % group filters
    clear in sr  env deltan  env_r n
end
save([origDir 'modulation.mat'],      'X1', 'stim_name','fq')
 
