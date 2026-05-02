# The-Musician-and-The-Music-School
- Pranav Bommareddy
- ECON 4970
- May 2026

## Overview
My project began with a general interest in the orchestral labor market in America, which led me to a recent paper by industry professional Alfredo Muñoz of the Boston Lyric Opera. His paper, "The Meritocracy Myth: Supply-Demand Concentration and Economic Barriers in Elite Orchestral Pipelines," empirically showed that access to elite music training institutions is sharply unequal, and that this inequality is strongly correlated with sharply unequal demographic representation on the orchestral stage.

I originally intended to undertake an independent or pure reproduction study, as the paper is very recent and has not yet been published. However, given the breadth of theory and heavy framework design, I found the paper difficult to replicate on my own. So, I chose to focus on one aspect of Muñoz's framework and modify his focus of study to ask the question: Does access to the professional orchestra remain sharply unequal when we include all LAO (League of American Orchestras) large-budget orchestras?

## The Dataset
Data for the supply side of the question comes from the Integrated Postsecondary Education Data System (IPEDS), filtering for CIP (program codes) for orchestral performance major awardees from 2011 to 2024. Musician data comes from scraping publicly released and available biographies of musicians currently employed by orchestras that are members of the League of American Orchestras. For this project, names were removed, and only information about where the musician graduated from, their date of graduation, and their date of entry into the orchestra.

Demand-side, major US orchestras were grouped from 1 through 8 according to their LAO budget and artistic expenses (which strongly indicate prestige and influence on the market). Groups 1-3 constitute large-budget orchestras. Additionally, a special subset of the Top 8 American orchestras was used as a division in the dataset.

All raw datasets can be accessed from the Harvard Dataverse: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VKY6EE

## Methodology (Follows Muñoz paper)
1. Setup
   
- The following packages were required for data manipulation: tidyverse, readr,       janitor, lubridate, and stringr.
- For visualization: scales, paletteer, patchwork, RColorBrewer, ggalluvial,          ggrepel, cowplot, ggpattern, ragg.
- Load in data and define file paths
  
2. Data Transformation
-  IPEDS: standardized unitid and data_year, added variable for numeric demographic counts, calculated percentages for demographics
-  defined underrepresented minorities: Black, Hispanic, and Native American musicians
-  Orchestra: create a variable for LAO group number and sort orchestras by group
-  Musician data: case_when to link musician's school to ipeds_unitid.
-  Standardize institution key and join musician data to ipeds data.
  
3. Primitve Functions
 -  creating special functions to obtain k-coverage sets, Gini and hhi coefficients, Wilson score, statistical tests, attrition metrics, and demographic metrics
 -   I pulled this code directly from the Muñoz code

4. Core Analyses
   
  - Calculate placement concentrations by tier of orchestra using a special function, get k50 coverage sets to determine how many schools it takes to account for 50% of orchestra members in each tier of orchestra. Schools that are within this k50 coverage for at least two tiers were made elite feeder school candidates.
  - This is where I tweaked the Muñoz approach. I tiered out and obtained k50 coverage sets for schools in the LAO groups 1, 2, 3, Top 8, and the whole set of orchestras. Any school that met the k50 coverage in at least two of the 5 tiers was a candidate for elite status. I used a modified version of Muñoz's greedy algorithm to minimize the number of schools to meet the 50% threshold.
  - Use left_join to build a table merging elite schools' unitid with ipeds institution information, and then ranking the schools.
  - Now we can use this table to study and compare its demographics to all the other schools.
  - I also pull Muñoz's code directly to build a contingency table and the
statistical tests.
  - I also used filter to pool all degrees awards across the timeline
  - Calculate attrition rates from population to profession for underrepresented minorities.
   
5. Tables & Figures
  - I used Muñoz's specifications and code to build my tables to maintain consistency; however, I have indicated in the code file if I made any changes

## Findings with Visualizations
<img src="[YOUR URL HERE]" alt="[IMAGE DESCRIPTION HERE]" width="200">


## Conclusion

