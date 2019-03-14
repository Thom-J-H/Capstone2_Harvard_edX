# Capstone2_Harvard_edX

In partial fulfillment of the requirements for the [Harvard edX: Data Science Professional Certificate](https://www.edx.org/professional-certificate/harvardx-data-science), this repository contains the following files:
* *R Markdown*: [Capstone_Two_Report_Haslam_2019_03_12.Rmd](https://github.com/Thom-J-H/Capstone2_Harvard_edX/blob/master/Capstone_Two_Report_Haslam_2019_03_12.Rmd) 
* *R_Script*: [Capstone_Two_Script.r](https://github.com/Thom-J-H/Capstone2_Harvard_edX/blob/master/Capstone_Two_Script.r)  
* *Report in PDF*: [Capstone_Two_Report_Haslam_2019_03_12.pdf](https://github.com/Thom-J-H/Capstone2_Harvard_edX/blob/master/Capstone_Two_Report_Haslam_2019_03_12.pdf)  
* *Extra*: Capstone_Two_Report_Haslam_2019_03_12.html 

And the original [Breast Cancer Wisconsin (Diagnostic) data set (WDBC)](https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Diagnostic)), available from the [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/index.php), Center for Machine Learning and Intelligent Systems, University of California, Irvine:
* [wdbc.data.csv](https://github.com/Thom-J-H/Capstone2_Harvard_edX/blob/master/wdbc.data.csv)
* [wdbc.names.txt](https://github.com/Thom-J-H/Capstone2_Harvard_edX/blob/master/wdbc.names.txt)
  
The script (and RMD) import the data set from the UCI source, so there is no need to download it first.

**Please note:** <br />
The RMD will take a minimum of 40 minutes -- and more likely over an hour -- to run.  It also requires that the user has installed a number of ML packages for R, consistent with those used in for [ensemble modelling in the Harvard edX course](https://rafalab.github.io/dsbook/machine-learning-in-practice.html#ensembles) on Machine Learning.  The script largely runs silently (the output captured).  Any warnings or error messages may be safely ignored.  Not every model works perfectly on each testing condition/variation, which is the point of testing the various models against similar controlled conditions.

Thank you,<br />
Thom J. Haslam<br />
March 12, 2019

&nbsp; <br />
![Run Two: Visual Overview](https://raw.githubusercontent.com/Thom-J-H/Capstone2_Harvard_edX/master/Run_two_graph.png)

## Update: 2019-03-14
I thank the Harvard edX peer and staff reviewers for their encouraging and helpful comments.  One suggestion was to change the loading procedure in the RMD from
* library(tidyverse)
* library(caret) # etc

To 

* if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
* if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org") # etc

Which will ensure that if someone is missing the needed packages, the packages will be installed from CRAN so that the RMD runs without terminating by error.

This is an excellent suggestion, so I will update the script and the RMD (by 15 March 2019) for future use/reference.  I will also take one last crack at fixing any typos or infelicities of expression in the report, even though the project has received **full marks** (50 out of 50) and for all practical purpose is done: [certificate earned!](https://courses.edx.org/certificates/670fdf3fe3e948f890134889fa55676d)

Otherwise, I will leave this Machine Learning project up as an archive: as part of what I hope will be a growing *R for Data Science* portfolio.

&nbsp; <br />
![PCA Graphs 1-2, 4-5](https://raw.githubusercontent.com/Thom-J-H/Capstone2_Harvard_edX/master/PCA_graph.png)


