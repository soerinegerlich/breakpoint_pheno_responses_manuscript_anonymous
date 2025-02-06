# breakpoint_responses_manuscript

## Content
This repository contains the code and data necessary to replicate data analyses, figures and tables in:

Gerlich, Hannah Sørine, Martin Holmstrup, Niels Martin Schmidt, Ally Phillimore and Toke T. Høye. (2024). Submitted. ***Keeping up with climate change - have phenological reaction norms reached their limit in Arctic arthropods?***

## Contact
Hannah Sørine Gerlich

Email: soger [at] ecos.au.dk

## Data usage guidelines

### Data

- Phenological observations for Zackenberg were provided by the Greenland Ecosystem Monitoring Programme. Data available at: https://data.g-e-m.dk/ The raw data downloaded from the database is included in this repository (downloaded 14. September 2022) is included in this repository along with a formatted version including all estimated phenological events.

- Environmental predictors: Temperature and snowmelt observations for Zackenberg were also provided by the Greenland Ecosystem Monitoring Programme. Data available at: https://data.g-e-m.dk/ The raw data downloaded from the database is included in this repository (downloaded 13. January 2022) along with a formatted version including all estimated temperature and timing of snowmelt estimates.

### Code 
All code provided for data preparation and analysis is licensed under a MIT License. In accordance with the license the code is available to be shared and adapted, but we would appreciate attribution to the authors, e.g. through citation of the above manuscript, and indications where changes were made. Although not mandatory, we additionally suggest that code users contact and collaborate with contributors should the code form a substantial proportion of a particular publication or analysis.

*Important:* Please note that the phenological metrics used in this study was previously determined in another study, currently found as a preprint. We refer to the following repository for all scripts and data needed to replicate the calculations of phenological events: https://github.com/soerinegerlich/high_arctic_arthropod_phenology_manuscript 

# Data preparation and clean up
The cleaned phenology data and estimated phenological metrics can be found here:

```
/Data/phenology_data

```

*Important:* Please note that this summarised data is for archival purposes only. If you intend to use the phenological observations in this dataset please refer to the data usage guidance for the raw data sets described above. 

The following path leads directly to the phenological estimates and climate variables used to conduct the breakpoint analysis:

```
/Data/phenology_data/df_phen_event_final.csv
/Data/phenology_data/Snowmelt_Climatestation.xlsx

```

The following path leads directly to the model summaries from the breakpoint analysis used to conduct the meta-analysis:

```
/Data/Summary_tables/Final/df_summary_onset_temp_final.xlsx
/Data/Summary_tables/Final/df_summary_peak_temp_final.xlsx
/Data/Summary_tables/Final/df_summary_end_temp_final.xlsx

/Data/Summary_tables/Final/df_summary_onset_snow_final.xlsx
/Data/Summary_tables/Final/df_summary_peak_snow_final.xlsx
/Data/Summary_tables/Final/df_summary_end_snow_final.xlsx

/Data/Summary_tables/Final/df_summary_onset_temp_snow_final.xlsx
/Data/Summary_tables/Final/df_summary_peak_temp_snow_final.xlsx
/Data/Summary_tables/Final/df_summary_end_temp_snow_final.xlsx

/Data/Summary_tables/Final/df_summary_onset_snow_temp_final.xlsx
/Data/Summary_tables/Final/df_summary_peak_snow_temp_final.xlsx
/Data/Summary_tables/Final/df_summary_end_snow_temp_final.xlsx

```

*Please note:* model summaries for taxa-by-ploy-combinations have been divided according to phenological event and climate variable in which breakpoints are tested. Hence, there are three summaries for temperature as a single predictor, three summaries for snowmelt as a single predictor, three summaries for temperature controlling for snowmelt and three summaries for snowmelt controlling for temperature. The summaries are listed accordingly in the above.

# Analysis scripts

The follwoing scripts contain code for the breakpoint model simulations:

```
/bkpr_model_simulation.R
/bkpr_simulation_null_model.R

```

The following scritps with all relevant code can be used to reproduce the analysis.

Breakpoint analyses:

```
/Breakpoint_onset_temp_single.R
/Breakpoint_onset_snow_single.R
/Breakpoint_onset_temp_multiple.R
/Breakpoint_onset_snow_multiple.R

/Breakpoint_peak_temp_single.R
/Breakpoint_peak_snow_single.R
/Breakpoint_peak_temp_multiple.R
/Breakpoint_peak_snow_multiple.R

/Breakpoint_end_temp_single.R
/Breakpoint_end_snow_single.R
/Breakpoint_end_temp_multiple.R
/Breakpoint_end_snow_multiple.R

```

Meta-analyses:

```
/Meta_analysis_onset_temperature.R
/Meta_analysis_onset_snowmelt.R
/Meta_analysis_onset_temperature_snowmelt.R
/Meta_analysis_onset_snowmelt_temperature.R

/Meta_analysis_peak_temperature.R
/Meta_analysis_peak_snowmelt.R
/Meta_analysis_peak_temperature_snowmelt.R
/Meta_analysis_peak_snowmelt_temperature.R

/Meta_analysis_end_temperature.R
/Meta_analysis_end_snowmelt.R
/Meta_analysis_end_temperature_snowmelt.R
/Meta_analysis_end_snowmelt_temperature.R

```

Comparison between models containing one and both climate variables:

```
/Model_comparison_single_multiple_predictors.R

```

The following R scripts contains relevant code to replicate figures 2 - 4:

```
/Figure2_updated_SE.R
/Figure3_only_onset.R
/Figure4_breakpoint_frequency_both_predictors.R

```