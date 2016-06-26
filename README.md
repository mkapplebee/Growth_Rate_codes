### Growth Rate codes

These folders contain various scripts I have written to analyze growth-rate data measured from 96-well plates

Analysis is currently done primarily through R-markdown files "gc_analysis-template.Rmd" and "epi_analysis-template.Rmd", by editing them to import the appropriate data;  when executed, the file processes the data, generates output tables, and produces an html report file with plots, tables, and has room for experiment-specific notes and observations.  

Folders 1_gc_analysis_scripts / 2_epi_analysis_scripts hold .R files that mostly hold functions that the .Rmd files call

Folders "cssS_X_X" hold examples of experimental data that have been processed using these scripts.  'cssS' denotes the growth rates are of a set of strains that include the cssS-deletion strain, grown in LB media, DSM, acidified LB (pH5), or LB + 0.008% Triton-X detergent.
