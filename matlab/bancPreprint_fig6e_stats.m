% bancPreprint_fig6e_stats.m
%
% Script to compute statistics on Figure 6e. Compare distributions
%

% load raw csv file
csvFilepath = '/Users/hyang/HMS Dropbox/Helen Yang/BANCanalysis/figure_6e_data.csv';

rawCsv = readtable(csvFilepath);

descendingVals = rawCsv.prop(strcmp(rawCsv.type,'descending')); 
ascendingVals = rawCsv.prop(strcmp(rawCsv.type, 'ascending'));
otherVals = rawCsv.prop(strcmp(rawCsv.type, 'other'));

% Two-sample Kolmogorov-Smirnov test
[~,p_ks_DN_other,ks2stat_DN_other] = kstest2(descendingVals, otherVals);
[~,p_ks_AN_other,ks2stat_AN_other] = kstest2(ascendingVals, otherVals);
[~,p_ks_AN_DN,ks2stat_AN_DN] = kstest2(ascendingVals,descendingVals);

% ranksum test

[p_rs_DN_other,~,rsStat_DN_other] = ranksum(descendingVals, otherVals);
[p_rs_AN_other,~,rsStat_AN_other] = ranksum(ascendingVals, otherVals);
[p_rs_AN_DN,~,rsStat_AN_DN] = ranksum(ascendingVals,descendingVals);
