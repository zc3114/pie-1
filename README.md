# pie-1
C.elegans embroyonic genomic expression analysis for Leroi's practical

#how to use the code
if you just want to run the code, see the heatmap/network, fiddle with the correlation methods etc, then you need to
1) download all files in this repository
2) either run codes in file 'pie-1.R' if you want to start from scratch; or
3) skip to line 66 (disabled by default; remove # and run) to load the data frame, then run the code for heatmap or network

for network, use function cluster.plot
cluster.plot() runs with the following parameters by default: cluster.plot(df=d,cor='pearson',cut.off=0.8,communities = F)
4 arguments used here: 
1) df=d specifies the dataframe used (no need to change)
2) cor specifies the correlation method used, alternative inc. 'pearson','spearman' and 'kendall'
3) cut.off is the cut off value
4) communities specifies the plot,'pos' for communities with + interaction,'neg' for - interaction, or F for both(whole network, no communities)
