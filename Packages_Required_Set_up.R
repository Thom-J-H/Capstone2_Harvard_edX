# For replicating the ML project here, or for setting up an R installation
# A listing of packages either required or useful


# The following code with the object(s) Tidy_pack, ML_pack 
# install.packages(package_cluster, repos = "http://cran.rstudio.com")

# Tidyverse packages
Tidy_pack <- c("tidyverse", "broom", "tidytext", "infer", 
               "tidymodels", "skimr", "reshape", "rlang",
               "blogdown", "bookdown", "shiny", "flexdashboard",
               "devtools", "git2r", "DT", "kableExtra")

# Machine Learning packages
ML_pack <- c("import", "gbm", "glmnet", "mlp", "monmlp", "ranger",
             "randomForest", "Rborist", "ada",  "fastAdaboost",
             "gam",  "GAMBoost", "gamboostLSS", "kknn", "naivebayes",
             "rpart", "Rborist", "keras", "arm", "JOUSBoost", "wsrf", 
             "xgboost" ) # caret imported earlier with tidypack

# Also
#
# Textmining packages
TeMi_Pack <- c("tm", "qdap", "quanteda", "qpdf", "pdftools", "plotly",
               "radarchart", "sentimentr", "gutenbergr", "SenitmentAnalysis",
               "wordcloud2")


# Social Network Analysis packages
SNA_pack <- c("igraph","ggigraph", "coefplot", "cowplot", "ggmap",
               "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
               "here", "interplot", "margins", "maps", "mapproj",
               "mapdata", "scales", "sp", "plotlyGeoAssets", "ndtv", 
               "survey", "srvyr", "viridis", "viridisLite")

# Other Bayes
Bayes_pack <- c("BAS", "rstan", "rstanarm", "tidybayes", "gtools")


# Other stat packages and useful miscellanous
Stat_misc <- c("dslabs", "openintro", "pid", "nycflights13", "Lahman",
                "waffle", "rJava", "HistData", "historydata", "SpatialEpi",
                "raster", "DiagrammeR", "pkgbuild", "htmlTable", 
                "generalhoslem", "sigr")

