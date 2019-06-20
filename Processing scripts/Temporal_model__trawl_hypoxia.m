
% load whole_comm_matlab longevity as a table, others as columns
oxyspring = oxygenspring;
oxysummer = oxygensummer;
oxyautumn = oxygenautumn;
oxywinter = oxygenwinter;
SAR = surface_SAR;

datbenth = table2array(wholecommmatlab);

% I run the model for 100 years, incorporating oxygen as a temporal pattern
% and trawling as a random pattern

% get temporal pattern of oxygen
oxygen = [repmat( oxyspring , 1,92), repmat( oxysummer , 1,91), ...
          repmat( oxyautumn , 1,91), repmat( oxywinter, 1, 91)];
oxygen_tot = repmat(oxygen, 1, 100);
Odef = 0.01 * (1+oxygen_tot-0.5).^-6;

% get random fishing event pattern
SARfl = floor(SAR);
ranF = rand(length(SAR),1);
SARex = SAR-SARfl > ranF; 
totSAR = SARfl + SARex ;

nb = length(SAR);
fishing_tot = zeros(nb,365*100);
for grid = 1:nb
    xx  = 0;
    for year = 1:100
        pos = randperm(365,totSAR(grid));
        fishevent = zeros(1,365);
        fishevent(pos) = 1;
        xx = [xx fishevent];
    end
    fishing_tot(grid,:) =  xx(1,2:end);
end

clear xx oxyautumn oxygen oxyspring  oxysummer oxywinter ...
    ranF  SAR SARex SARfl totSAR year fishevent grid pos oxygen_tot ...
    oxygenautumn oxygenspring oxygensummer oxygenwinter surface_SAR ...
    wholecommmatlab

% get the rates from the resample
state = zeros(nb,3); 
longclasses = 1:15;
K = 1;
r_number = 5.31;
d_rate = 0.06;
Recov =  (r_number./longclasses) / 365;
    
calcul = zeros(nb,15);
for long = 1:15
    Pall = zeros(nb,36500)+1;
    for p = 1:36500
        Fish = fishing_tot(:,p)* d_rate;
        Oxi  = Odef(:,p);
        I = (Pall(:,p) < 0.001);
        Pall(I,p) = 0.001;
        Pall(:,p+1) = Pall(:,p) + Recov(long).*Pall(:,p) .*((K-Pall(:,p))./K) - Fish.*Pall(:,p) - Oxi.*Pall(:,p);
    end  
    calcul(:,long) = mean(Pall(:,18251:end-1),2);
end

state(: , 1) = sum(calcul .* datbenth,2);
save state.mat state
 