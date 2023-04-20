# DATA ACQUISITION 
This file contains the original data files for all data used in this analysis. When there were not spreadsheets available to be downloaded as raw files, I created an excel sheet and copied the information into that spreadsheet. The files in which this occurred are clearly noted below and 
any modifications to tidy the data are clearly noted. 

**DOWNLOADED DATA SETS** 
**Watts, D. P. (2020). Meat eating by nonhuman primates: a review and synthesis. Journal of human evolution, 149, 102882.**

There was not a dataset available to download for the above article. This data was copied into an excel spreadsheet. The following modifications were made to enhance usability of this data: 1) Some species rows had blank spaces where the prey order and family were inferred to be the taxonomy specified above. I input the prey order and family when appropriate information as given in the source column. I did not add any specifications when the source did not give further 
identifying information. 2) Some names had astrichs beside them. These astricts were removed. 3) In the chimpanzee predator rows, the prey order column listed primates, rather than mammalia. In order to maintain consistency, I adjusted this taxonomy so all primate prey were listed as mammalia in the prey column, primates in the order column, and the appropriate family for each primate given in the family column. 


**DeCasien, A. R., Williams, S. A., & Higham, J. P. (2017). Primate brain 
size is predicted by diet but not sociality. Nature ecology & evolution, 
1(5), 0112.**

This data set was downloaded on 03/26/23 from its supplemental file 
format. No changes were made. 

# DATA CLEANING
The above datasets were imported into R and cleaned. The R file to clean these datasets is saved within this directory as data_cleaning.R. There were two cleaned files produced called cleaned_df.csv and meat_df.csv. These files were used in the subsequent analysis. 
