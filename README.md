# Seminararbeit_code_version2_SoSe25
R code for the second version of th seminar paper
This R script is designed to be used within RStudio. The first part of the code clears the environment and loads all required packages in a single step. A fixed random seed is set for reproducibility, although no random elements are used in this particular analysis.

The code begins by importing population data from the UN’s World Population Prospects 2024 Excel file. It selects only the necessary columns (country, population, and year), removes missing values, and renames variables for readability. A full list of all country names is then exported to a .csv file, allowing for manual filtering outside of R.

A manually curated version of this list – which flags which countries should be excluded (this was done using a list of the current 193 UN member states) – is re-imported in the next section. Based on this file, only the relevant countries are kept. These filtered countries are then automatically assigned to continents using the countrycode package.

The core analysis is divided into two parts: by year and by region. First, for each of the selected years (2002, 2007, 2012, 2017, 2022), the script extracts the first and then first two digits of population counts and compares them to the expected frequencies under Benford’s Law. Several metrics are computed: Chi-squared test p-values, the Kolmogorov–Smirnov (KS) statistic, and the mean absolute deviation (MAD), which is also categorized based on standard conformity thresholds.

In the next section, population data from all target years is aggregated by region. The same digit-based analysis is performed at the regional level, allowing for comparisons across continents.

The script then focuses on the most recent year (2022) and creates plots comparing the observed digit distributions (first and first two digits) with theoretical Benford proportions. These plots are saved as .pdf files.

Finally, the code produces additional summary barplots showing MAD and KS statistics over time and by region. Results are also output as LaTeX-compatible tables using the xtable package.
