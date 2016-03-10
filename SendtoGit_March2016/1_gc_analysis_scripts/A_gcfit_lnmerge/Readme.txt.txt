Workflow set for processing formatted growth curve data using package "grofit"

1_import_csvs.R:  a script for importing and naming all .csv files in a folder; assumes all csv files are raw gc data

2_growfit-combined-no_overlay.R: runs gcFit and generates indivdiual growht curves for inspection; calls "gcFit3-nonoverlap.R" which is in another folder
                most redundant with gcfit_no_overlay version, which also has the called gcFit3 file
3_gcfall-media-lnmerge.R: runs gcFit with both ln.y=TRUE and FALSE, and organizes both columns; 
	all output columns except "ln_mu"-labeled columns report data from ln.y=FALSE analysis

3b_grofit-combined-Dec2015:  called by 3_gcfall-media-lnmerge; generates time and OD matrixes to feed into gcFit5

3c_gcFit5.R: modified gcFit function called by "3b_grofit-combined-Dec2015"