# Quantifying-the-Value-of-CS

## Abstract

Monitoring biodiversity can be critical for informing effective conservation strategies, but can also deplete the resources available for management actions. Community science programs may help alleviate this issue, providing data to support decisions for less cost than professional monitoring. However, the direct financial and management benefits of community science for optimal resource allocation have not been quantified. Our objective was to quantify and compare the predicted outcomes of prioritizing conservation action based on regional community science data to predicted outcomes based on targeted professional monitoring data. Using data from The Nature Conservancy’s (TNC) BirdReturns program in the Central Valley of California as a case study, we prioritized management units for conservation action based on the predicted probability of detecting seven shorebird species. We ran prioritizations across a range of budgets using (i) eBird community science data only, (ii) professionally-collected data only, and (iii) both eBird and professional integrated data. The outcomes of decisions based on eBird data were comparable to those based on professional data even before accounting for the cost of professional monitoring, and substantially better when monitoring costs were explicitly considered, particularly at lower budgets. Thus, conservation action based on freely-available community science data could theoretically result in better biodiversity outcomes than paying for targeted professional monitoring. Our findings reveal the exceptional value provided by community science programs for informing conservation decisions and redistributing resources from monitoring to action.

## Data Files

### Scripts

All scripts are found in the main project folder.

**Calc_prevalence.R** 
This is a simple calculation of the prevalence of each species on professional surveys. The outputs is later used as the prior probability of species detection.

**GS1-prioritization.R** 
Prioritizing properties for conservation action based on the modeled probability of detection from Robinson et al. (2020)

**GS2-validation.R** 
Using estimates from the integrated model, adjusted for sensitivity and specificity, to determine how the prioritizations based on modeled estimates performed.

**GS3-results.R**
Calculating the difference in prioritization performance among the three prioritizations (eBird, TNC and integrated, plus some supplemental analyses) and visualizing the results.

**GS4 - cost results.R**
Assessing the difference in prioritization performance between CS and professional data at a set budget of 400,000 USD.

*Note that some files were removed to protect landowner privacy. Therefore, the project stored here cannot be run from start to finish, but data were saved at checkpoint throughout the analysis, allowing most of it to be run without these files.
