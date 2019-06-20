
% load longevity resampled
dat = x;

% load d and r values
% load griddata

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

clear x xx oxyautumn oxygen oxyspring  oxysummer oxywinter ...
    ranF  SAR SARex SARfl totSAR year fishevent grid pos oxygen_tot

% get the rates from the resample
state = zeros(nb,1500); 
longclasses = 1:15;
K = 1;

for samp = 1:1500
    Recov =  (r_number(samp)./longclasses) / 365;
    
    calcul = zeros(nb,15);
    for long = 1:15
        Pall = zeros(nb,36500)+1;
        for p = 1:36500
             Fish = fishing_tot(:,p)* d_rate(samp);
             Oxi  = Odef(:,p);
             I = (Pall(:,p) < 0.001);
             Pall(I,p) = 0.001;
             Pall(:,p+1) = Pall(:,p) + Recov(long).*Pall(:,p) .*((K-Pall(:,p))./K) - Fish.*Pall(:,p) - Oxi.*Pall(:,p);
        end  
        calcul(:,long) = mean(Pall(:,18251:end-1),2);
    end
    state(: , samp) = sum(calcul .* squeeze(dat(1,:,:)),2);
    
    fprintf('%d\n',samp);
    
    if samp == 100
    save state.mat state
    end
     
    if samp == 500
    save state.mat state
    end
    
    if samp == 1000
    save state.mat state
    end
end
save state.mat state

         
         
    
