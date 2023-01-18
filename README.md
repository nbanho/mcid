# Detection of airborne SARS-CoV-2 in schools and effects of mask wearing and air cleaners

This is the code accompanying the paper "Detection of airborne SARS-CoV-2 in schools and effects of mask wearing and air cleaners" by Banholzer and Zuercher et al.

The repository is structured as follows:
* *analysis*: contains code files for analyzing and visualizing the data.
* *data-clean*: preprocessed data files (not yet public)
* *data-raw*: raw data files (not yet public)
* *fitted-models*: csv files of the models fitted in Stan (not in repository)
* *helper*: general settings
* *models*: Stan model files
* *preprocessing*: files to preprocess all data, i.e. environmental (Palas) and epidemiological (REDCap) data
* *results*: figures and tables of the paper (not in repository)
* *utils*: additional, general code applicable across analysis, e.g. for plotting

Preprocessed (clean) data files will be made available upon publication and allow reproducing all analysess. 

The relevant files regarding analysis are the following:
* *preprocessing/prob_sim_data.r:* generates the probabilistically simulated datasets
* *analysis/descriptives-redcap.rmd:* descriptive analysis of epidemiological data, including probabilistically generated datasets, and together with results from molecular/laboratory analysis 
* *analysis/analysis-multiverse-redpca.r:* fits Bayesian hierarchical transmission model to each probabilistically generated epidemiological datasets and stores results in fitted-models/multiverse
* *analysis/analysis-transmission-model-class-level.Rmd:* summarizes and visualizes results from Bayesian transmission model in fitted-models/multiverse
* *analysis/descriptives-palas.rmd:* analysis and visualization of results for environmental data
* *analysis/analysis-transmission-risk.Rmd:* modeling of transmission risk using a modified Wells-Riley equation, using both data and results from epidemiological and environmental analyses
