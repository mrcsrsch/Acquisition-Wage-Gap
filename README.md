# Replication Package for 'Do workers or firms drive the foreign acquisition wage gap?'

**Last update: April 24, 2025**

## Overview

This replication package contains the scripts required to replicate the results of the paper [*"Do workers or firms drive the foreign acquisition wage gap?"*](https://mrcsrsch.github.io/research/4_2022_acquisition_premium/) by Marcus Roesch, Michiel Gerritse, Bas Karreman, Frank van Oort and Bart Loog. Due to the confidential nature of the data, the analyses must be executed on the servers of Statistics Netherlands (CBS). For more details on accessing the data, see "source data access."

## Folder structure

- `scripts/`: Contains R and Stata code to run the analysis.
- `data/`: Stores intermediate datasets created during the analysis.
- `data/source_data/`: Place the (confidential) source data files here (see "source data access").
- `outputs/`: Result figures and tables for the paper will be saved here.

## Replication instructions

### Data preparation
1. Place the source data in the `data/source_data/` folder. Make sure to also place the file `data/source_data/        - SBI_2008_versie_2018_update_2019.xlsx`, which is available in the replication package folder.
2. Adjust the file paths in `00_main.R` to match your directory structure.

### Run analysis 
3. Run `00_main.R` from the `scripts/` folder in R. This will:
   - Build the analysis datasets from the source data.
   - Reproduce the result figures and tables from the paper.
4. Result tables and figures will be saved in the `outputs/` folder.
5. **Important**: Running the full analysis may take several days. We recommend running the scripts in stages while monitoring closely. 

### Overview of tables created by scripts
| Table / Figure                                                                 | Script                        |
|--------------------------------------------------------------------------------|-------------------------------|
| Figure 1: Decomposition of the post-acquisition wage gap.                     | 10_main_result_size_industry.R |
| Table 1: Cross-sectional wage decompositions.                                 | 10_xsec_decompositions.R      |
| Table 2: Difference-in-differences decomposition of the acquisition wage gap on unmatched sample. | 10_no_matching_DiD.R          |
| Table 3: Change in firm and worker fixed effects by employment size.          | 10_main_result_size_industry.R |
| Table 4: Change in firm and worker fixed effects by industry type.            | 10_main_result_size_industry.R |
| Table 5: Wage decomposition of managers’ and non-managers’ wages.             | 10_firmFE_mechanisms.R        |
| Table 6: Decomposition of moving workers’ wage at new firm                    | 10_firmFE_mechanisms.R        |
| Table 7: Hire and separation margins.                                         | 10_hires_separations.R        |

### Software and hardware requirements

- **R version >= 3.5.1**
- The analysis was performed on a server with an 8-core processor running at 3.19 GHz and 48 GB of memory. Ensure you get access to a server at CBS that has sufficient computational resources for large datasets and long runtimes.
- Important R-packages: data.table, fixest, ggplot2 

## Source data access

This project uses confidential datasets provided by Statistics Netherlands (CBS). The specific datasets are:

### Worker-level datasets
- **(S)POLISBUS** (employer-employee data): 2006 - 2018
- **GBAPERSOON** (worker demographics): 2018
- **NIET_GBAPERSOON** (worker demographics): 2018
- **SECM** (socio-economic status): 2006 - 2018

### Firm-level datasets
- **Uci** (ultimate controlling institutional unit): 2006 - 2018
- **Sfgo** (foreign subsidiaries): 2006 - 2015
- **Abr** (company group IDs, firm birth years): 2006 - 2018
- **Abr_regio** (locations of firms): 2006 - 2018
- **Abr_pab** (NACE industry codes): 2006 - 2018
- **ihg** (export and import statistics of firms): 2007 - 2018
- **productiestatistieken** (financial data of firms): 2006 - 2018

Due to confidentiality, we cannot share the data directly. To replicate the analysis, researchers can obtain access to these datasets through the [CBS Microdata Service](https://www.cbs.nl/en-gb/our-services/customised-services-microdata/microdata-conducting-your-own-research). Please note that access is subject to CBS's terms and fees. All data processing occurs on CBS servers, and exported outputs are subject to privacy and confidentiality checks.

To request access, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl).

### Important note

We had employee access to CBS data, which did not require requesting data through the Microdata Service. However, to the best of our knowledge, with the code provided here the analyses can be replicated with Microdata Service access. If any of the individual datasets are incomplete or unavailable through the Microdata Service, please contact CBS at [microdata@cbs.nl](mailto:microdata@cbs.nl) with your specific request.