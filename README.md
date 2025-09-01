# Project

Data visualization: U.S. Government Current Population Survey (CPS) 

## Description

This project is for data communication using CPS data maintained monthly and provided at https://www.census.gov/data/datasets/time-series/demo/cps/cps-basic.html

A data dictionary is located at: https://www2.census.gov/programs-surveys/cps/datasets/2025/basic/2025_Basic_CPS_Public_Use_Record_Layout_plus_IO_Code_list.txt 
Always check the notes for any measurement differences that may introduce error. Data are provided monthly. The initial sample subset of data for January through July 2025 are located in `data/raw/rds/2025/` and earlier data for incorporating into the project are located at https://www.census.gov/data/datasets/time-series/demo/cps/cps-basic.html. The files are `.Rds` compressed versions of the `.csv` data in order to fulfill space requirements.

## Potential Avenues & Caveats

Full monthly data sets may contain variables not required for the visualization project. The full complement of data may also be bulk under some conditions. Although not needed, there is no up-to-date direct integration with an R library. However, the https://www.ipums.org provides {ipumsr}, an R library to access their CPS data, which may serve as an alternative. 
{ipumsr} allows for API access to CPS data they maintain. Their website about CPS data specifically, https://cps.ipums.org/cps/, provides an overview of their variables, though exporting seems more complex to export than the completed data located at https://www.census.gov/data/datasets/time-series/demo/cps/cps-basic.html mentioned earlier. The API *may* provide greater data access. 

Some {ipumsr} links if desired:

- https://cran.r-project.org/web/packages/ipumsr/index.html
- https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html
