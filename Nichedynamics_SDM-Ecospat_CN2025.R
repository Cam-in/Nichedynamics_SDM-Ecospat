################################################################################
###
###    SDM_version2024
###    Authors script: Dra. Camila Neder & Catalina Escobar
###    Supervision: Dr. Pablo Guerrero
###    Citation: 1. Neder C (2023) El bentos antártico y su respuesta al cambio climático: una aproximaci?n usando modelos de distribuci?n de especies como caso de estudio en caleta Potter. Universidad Nacional de C?rdoba https://rdu.unc.edu.ar/handle/11086/551476
###              2. Neder et al. (2024) Antarctic benthic species distribution models and compositional analysis in a coastal ecosystem under glacier retreat DOI: https://doi.org/10.3354/meps14731
###              3. Escobar et al. (in press) Climate-driven range shifts across sub-Antarctic and Antarctic terrestrial and marine niches in sheathbills (Chionis)
###
###          last changes: 2024-December-01
###  
###                   R-4.1.2    biomod 4.2
###
################################################################################

################################################################################
### 1. Load (install if necessary) all needed packages
################################################################################

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages <- c("automap", 
              "biomod2",
              "blockCV",
              "caret",
              "Cairo",
              "corrplot", 
              "cowplot", 
              "dismo",
              #"future.apply",
              "ggplot2",
              "geosphere",
              "here", 
              #"OpenStreetMap",
              "performance",
              "PerformanceAnalytics",
              "PNWColors",
              "raster", 
              "randomForest",
              "rgdal",
              "rstudioapi",
              "RColorBrewer",
              "see",
              "sf", 
              "shiny",
              #"shinydashboard",
              "spatialEco", 
              "stars", 
              "stats",
              "stringi",
              "svDialogs", 
              "terra",
              "tidyverse", 
              "tidyterra",
              "tmap", 
              "tmaptools",
              "utils",
              "ggpubr",
              "earth"
)

## Now load or install&load all
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(char = packages, install = TRUE)

#remotes::install_github("rvalavi/blockCV", dependencies = TRUE)

rm(packages)

################################################################################
### 2. Define the working directory
################################################################################
# clear workspace and console and remove all plots
cat("\014")      # clear console
rm(list=ls())    # clear workspace
graphics.off()   # remove all plots as dev.off()

date.today <- format(Sys.time(), "%b_%d_%Y") # get today's date and save it. Will be used to create subfolder with date
options(scipen = 999)                        # disable scientific notation; Modify global options in R

################################################################################
## Define the path to the current R-script

# In order to determine the actual path, we first have to create the following function: 'thisPath'. 
# The path to the current R-script will be saved in the global environment under the name 'path_R_scripts'

## create function 'thisPath'
{
  thisPath <- function() {
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    if (length(grep("^-f$", cmdArgs)) > 0) {
      # R console option
      normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
    } else if (length(grep("^--file=", cmdArgs)) > 0) {
      # Rscript/R console option
      scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
    } else if (Sys.getenv("RSTUDIO") == "1") {
      # RStudio
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else if (is.null(attr(stub, "srcref")) == FALSE) {
      # 'source'd via R console
      dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
    } else {
      stop("Cannot find file path")
    }
  } # end of 'thisPath <- function() {'
  
  path_R_scripts <- thisPath()
  
  ## define the paths to different folders
  path_working_folder <- dirname(path_R_scripts)
  path_raster_data <- paste(path_working_folder, "/SDM_Chionis/bm_Chionis/02_original_data/01_raster_data/C_minor/marino/PRES22km", sep ="") ##Change for species
  path_raster_future_126 <- paste(path_working_folder, "/SDM_Chionis/bm_Chionis/02_original_data/01_raster_data/C_minor/marino/FUT/2041_2070/SSP126", sep ="")
  path_raster_future_585 <- paste(path_working_folder, "/SDM_Chionis/bm_Chionis/02_original_data/01_raster_data/C_minor/marino/FUT/2041_2070/SSP585", sep ="")
  path_bio_data <- paste(path_working_folder, "/SDM_Chionis/bm_Chionis/02_original_data/02_biological_data", sep ="")
  path_results_folder <- paste(path_working_folder, "/SDM_Chionis/bm_Chionis/03_R_results", sep ="")
  path_outputs_folder <- paste(path_working_folder, "/SDM_Chionis/bm_Chionis/04_R_outputs", sep ="")
  #path_bio_data_PA  <- paste0(path_working_folder, "/SDM_Chionis/bm_Chionis/02_original_data/02_biological_data/OCC_C_minor_Biomod_PA.txt")
  
  rm(thisPath)
}
################################################################################
### 3. BIOMOD2 settings
################################################################################

# Select the models used in BIOMOD2. Here you have to choose between 
# 10 different algorithms (GLM, GBM, GAM, CTA, ANN, SRE, FDA, MARS,
# RF, MAXENT).
# In a second step you can choose the evaluation metrics among KAPPA, TSS, 
# ROC, FAR, SR, ACCURACY, BIAS, POD, CSI and ETS

################################################################################
### 3.1 Select the models used in BIOMOD2
################################################################################
{
  model_list <- c("GLM", "GBM", "GAM", "CTA", "ANN", "SRE", "FDA", "MARS", "RF", "MAXENT", "XGBOOST", "MAXNET")
  chosen_models <-c()
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose the models for BIOMOD")$res
    chosen_models <- utils::select.list(model_list, preselect = NULL, multiple = TRUE,
                                        title = "Choose one or more models", graphics = TRUE)
    if(length(chosen_models)>=1){
      answer <- "yes"
    }
  }
  rm(model_list, answer)
  chosen_models
}
#################################################################################
### 3.2 Choose the evaluation metrics in BIOMOD2
#################################################################################
{
  eval_list <- c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")
  chosen_evals <-c()
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose the evaluation metrics for BIOMOD")$res
    chosen_evals <- utils::select.list(eval_list, preselect = c("TSS", "ROC", "KAPPA", "ACCURACY"), multiple = TRUE,
                                       title = "Choose one or more evaluations", graphics = TRUE)
    if(length(chosen_evals)>=1){
      answer <- "yes"
    }
  }
  rm(eval_list, answer)
  chosen_evals
}
################################################################################
### 4. Settings for the correlation analyse
################################################################################

################################################################################
### 4.1. Define method for the correlation matrix (pearson, kendall or spearman)
################################################################################

## Pearson correlation vs Spearman and Kendall correlation
# Non-parametric correlations are less powerful because they use less information
# in their calculations. In the case of Pearson's correlation uses information about
# the mean and deviation from the mean to search for linear relationships, while non-parametric correlations use only 
# the ordinal information and scores of pairs.
# In the case of non-parametric correlation, it's possible that the X and Y values 
# can be continuous or ordinal, and approximate normal distributions for X and Y are
# not required. But in the case of Pearson's correlation, it assumes the distributions
# of X and Y should be normal distribution and also be continuous.
# Correlation coefficients only measure linear (Pearson) or monotonic (Spearman and
# Kendall) relationships.
#
# In the normal case, Kendall correlation is more robust and efficient than Spearman
# correlation. It means that Kendall correlation is preferred when there are small 
# samples or some outliers.
# Kendall correlation has a O(n^2) computation complexity comparing with O(n logn) of
# Spearman correlation, where n is the sample size. 
# Spearmans rho usually is larger than Kendalls tau. 
# The interpretation of Kendalls tau in terms of the probabilities of observing the
# agreeable (concordant) and non-agreeable (discordant) pairs is very direct.

### In short:
### If you have outliers in your data (i.e. extreme observations), Pearson correlation
### measures will be distorted and correlation calculations will not be trustworthy.
### What to do then? Then you use non-parametric correlation measures: Spearman and 
### Kendall. Kendall is a little bit more sophisticated mathematically than Spearman,
### but you should expect to get similar results from both non-parametric measures.
{
  c.method.list <- c("pearson", "kendall", "spearman")
  chosen_corr_method <-c()
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose the method for the correlation analyse")$res
    chosen_corr_method <- utils::select.list(c.method.list, preselect = "spearman", multiple = FALSE,
                                             title = "Choose one method", graphics = TRUE)
    if(length(chosen_corr_method)>=1){
      answer <- "yes"
    }
  }
  rm(c.method.list, answer)
  chosen_corr_method
}
################################################################################
### 4.2. Decision how to handle highly correlated variables
################################################################################
## Decide if highly correlated variables shall be removed 
## automatically or not. If so, one has to define cut-off value
## for highly correlated variables
{
  answer <- "no"
  input_01 <- c()
  hcv_cutoff <- c()
  # Ask a question
  while (answer == "no") {
    input_01 <-  svDialogs::dlg_message("Automatic removal of highly correlated variables?", "yesno")$res
    if (input_01 == "yes"){
      answer2 <- "no"
      while (answer2 == "no") {
        hcv_cutoff <- as.numeric(dlgInput("Cutoff value for highly correlated variables (between 0.5 and 1)", 0.75)$res)
        if (hcv_cutoff <= 1 & hcv_cutoff >= 0.5) {
          answer2 <- "yes"
        }
      }
      rm(answer2)
    } else if (input_01 == "no") {
      hcv_cutoff <- NA
    }
    if (length(input_01)>=1 & length(hcv_cutoff)>=1) {
      answer <- "yes"
    }
  }
  if (input_01 == "no"){
    hcv_cutoff <- 0.75 #change value if you want a different correlation
  }
  rm(answer)
}
################################################################################
### 5. Decision how to handle Zero- and Near Zero-Variance Predictors (if exists!)
################################################################################
## Decide if predictor variables with a Zero- and Near Zero-Variance
## shall be removed automatically or not. If so, define the VIF threshold (normally 10)
{
  answer <- "no"
  input_ZNZ <- c()
  
  # Ask a question
  while (answer == "no") {
    input_ZNZ <- svDialogs::dlg_message("Automatic removal of variables with Zero- and Near Zero-Variance?", "yesno")$res
    if (length(input_ZNZ)>=1) {
      answer <- "yes"
    }
  }
  
  rm(answer)
}


################################################################################
### 6. Settings for multicollinearity
################################################################################
## Decide if predictor variables with a high VIF-Value (= high multicollinearity) 
## shall e removed automatically or not.
## If so, one has to define the VIF threshold (normally 10)
{
  answer <- "no"
  input_mc <- c()
  mc_cutoff <- c()
  # Ask a question
  while (answer == "no") {
    input_mc <- svDialogs::dlg_message("Automatic removal of variables with high multicollinearity?", "yesno")$res
    if (input_mc == "yes"){
      mc_cutoff <- as.numeric(dlgInput("Enter multicollinearity cutoff value (normally >= 10)", 10)$res)
    } else if (input_mc == "no") {
      mc_cutoff <- NA
    }
    if (length(input_mc)>=1 & length(mc_cutoff)>=1) {
      answer <- "yes"
    }
  }
  
  rm(answer)
}
################################################################################
### 7. Create functions to used later
################################################################################
####  function 1: Counts points in raster cell
# a function that counts the number of overlaying points in each raster cell
{
  pointcount = function(r, pts){
    # make a raster of zeroes like the input
    r2 = r
    r2[] = 0
    # get the cell index for each point and make a table:
    counts = table(cellFromXY(r,pts))
    # fill in the raster with the counts from the cell index:
    r2[as.numeric(names(counts))] = counts
    return(r2)
  }
  
  #### function 2: Corrplot with significance p values
  # function to wrap original corrplot::cor.mtest for keeping names and structure
  # Significance test which produces p-values and confidence intervals for each
  # pair of input features.
  # see: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
}
################################################################################
### 8. Load biological data
################################################################################
## set path to path where the biological data are located. 
## Txt should be used in tabulations separation and preferentially
## coordinates in decimal degree 1? x, 2?y and speciesname separated by point
{
  if(getwd() != path_bio_data) { setwd(path_bio_data) }
  dir()
  bio.data <- read.delim2(choose.files(default = path_bio_data, caption = "Select biological data",
                                       multi = FALSE),
                          header=TRUE)
}
################################################################################
### 8.1. Select the columns (= species, one or more) you want to work with
################################################################################
{
  chosen_col_999 <- c() # create an empty list for the column choice
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose (all) the species you want to work with")$res
    col_names <-c(names(bio.data)) 
    # chosen_col <- dlgList(col_names, multiple = FALSE)$res
    chosen_col_999 <- utils::select.list(col_names, preselect = NULL, multiple = TRUE,
                                         title = "Choose the columns (one or more) to work with", graphics = TRUE)
    if(length(chosen_col_999)>=1){
      answer <- "yes"
    }
  }
  rm(answer)
}
################################################################################
### 9. Create (new) subfolders in the result directory with the chosen species names
################################################################################
{
  ## now check if folder(s) with the chosen species name(s) already exist(s) in
  ## result folder. If not create those folder(s)
  W <- 1
  
  # create result folder if it doesn't exists yet
  dir.create(file.path(path_results_folder), showWarnings = FALSE)
  
  for (W in 1:length(chosen_col_999)) {
    mainDir <- path_results_folder
    subDir <- paste(chosen_col_999[W], date.today, sep="_")     # chosen_col_999 will be used later!
    dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  } # end of 'for (i in 1:length(chosen_col_999))'
  rm(W, mainDir, subDir)
}
################################################################################
### 10. Load and stack raster data
################################################################################
{
  ## First we create an empty data frame to store the metadata of the raster files.
  raster_meta_df <- data.frame(File=character(),
                               nrow=integer(),
                               ncol=integer(),
                               ncell=integer(),
                               projection=character(),
                               resolution_X_Y=character(),
                               extent=character(),
                               min_Value=numeric(),
                               max_Value=numeric(),
                               stringsAsFactors=FALSE) 
  
  
  ## set path to path where the raster data are located
  if(getwd() != path_raster_data) { setwd(path_raster_data) }
  
  ## create a list of all raster files in the folder with the ending 'tif'
  r_list <- dir(pattern=".tif$")
  length(r_list)
}
################################################################################
### 10.1. Select raster data manual
################################################################################
{
  answer <- "no"
  input_03 <- c()
  r_list_man <- c()
  # Ask a question
  while (answer == "no") {
    input_03 <- svDialogs::dlg_message("Do you want to select raster data manually?", "yesno")$res
    if (input_03 == "yes"){
      r_list_man <- utils::select.list(r_list, 
                                       preselect = c("bathy.tiff"
                                       ), 
                                       multiple = TRUE,
                                       title = "Choose the raster (one or more) to work with", graphics = TRUE)
      r_list <- r_list_man
    } else if (input_03 == "no") {
      r_list_man <- NA
      r_list <- r_list
    }
    if (length(input_03)>=1 & length(mc_cutoff)>=1) {
      answer <- "yes"
    }
  }
  
  rm(answer)
}
################################################################################
### 10.2. Stack raster save metadata in csv file and resample if necessary
################################################################################
{
  ## stack all raster in one raster stack,
  ## check extent of the raster and change if necessary
  all_raster <- raster::raster(r_list[1])
  
  ## Now the first raster is loaded and we are going to safe some of the metadata
  ## to the data frame 'raster_meta_df'
  raster_meta_df[c(nrow(raster_meta_df)+1), 1] <- r_list[1]
  raster_meta_df[nrow(raster_meta_df), 2] <- dim(all_raster)[1]
  raster_meta_df[nrow(raster_meta_df), 3] <- dim(all_raster)[2]
  raster_meta_df[nrow(raster_meta_df), 4] <- ncell(all_raster)
  raster_meta_df[nrow(raster_meta_df), 5] <- projection(all_raster)
  raster_meta_df[nrow(raster_meta_df), 6] <- paste('X=', res(all_raster)[1], ", Y=", res(all_raster)[2], sep="")
  raster_meta_df[nrow(raster_meta_df), 7] <- paste('Xmin=', round(extent(all_raster)[1], 2),
                                                   ", Xmax=", round(extent(all_raster)[2], 2),
                                                   ", Ymin=", round(extent(all_raster)[3], 2),
                                                   ", Ymax=", round(extent(all_raster)[3], 2),
                                                   sep="")
  raster_meta_df[nrow(raster_meta_df), 8] <- minValue(all_raster)
  raster_meta_df[nrow(raster_meta_df), 9] <- maxValue(all_raster)
  
  ## So now do the same for all other raster files beginning with #2. Every new raster file will be
  ## compared with the others (raster::all.equal()). If there is a difference regarding the extent, 
  ## the number of rows and columns, the crs and/or the resolution, the this raster file will be 
  ## automatically resampled (raster::resample()) to the values of the other raster files using the
  ## 'ngb' method (using the nearest neighbor value).
  setwd(path_raster_data)
  i <- 2
  for (i in 2:length(r_list)){
    print(i)
    r.temp <- raster::raster( r_list[i] )
    # add the metadata to the dataframe ''raster_meta_df'
    raster_meta_df[c(nrow(raster_meta_df)+1), 1] <- r_list[i]
    raster_meta_df[nrow(raster_meta_df), 2] <- dim(r.temp)[1]
    raster_meta_df[nrow(raster_meta_df), 3] <- dim(r.temp)[2]
    raster_meta_df[nrow(raster_meta_df), 4] <- ncell(r.temp)
    raster_meta_df[nrow(raster_meta_df), 5] <- projection(r.temp)
    raster_meta_df[nrow(raster_meta_df), 6] <- paste('X=', res(r.temp)[1], ", Y=", res(r.temp)[2], sep="")
    raster_meta_df[nrow(raster_meta_df), 7] <- paste('Xmin=', round(extent(r.temp)[1], 2),
                                                     ", Xmax=", round(extent(r.temp)[2], 2),
                                                     ", Ymin=", round(extent(r.temp)[3], 2),
                                                     ", Ymax=", round(extent(r.temp)[3], 2),
                                                     sep="")
    raster_meta_df[nrow(raster_meta_df), 8] <- minValue(r.temp)
    raster_meta_df[nrow(raster_meta_df), 9] <- maxValue(r.temp)
    
    if (isFALSE( all.equal(all_raster, r.temp, 
                           extent=TRUE, 
                           rowcol=TRUE, 
                           crs=TRUE, 
                           res=TRUE, 
                           orig=FALSE, 
                           values=FALSE, 
                           stopiffalse=FALSE, 
                           showwarning=TRUE) )){
      print(paste(i, " ouch, oh no!"))
      r.temp <- raster::resample(r.temp, all_raster, method="ngb")
    }
    all_raster <- stack(all_raster, r.temp)
  }
  print("Calculation finished")
  nlayers(all_raster)
  rm(i, r.temp)
}

##############################################################################
### 11. Background creation
##############################################################################
# If necessary, convert the relevant columns (e.g., 'x' and 'y') to numeric
str(bio.data)
bio.data$x <- as.numeric(bio.data$x)
bio.data$y <- as.numeric(bio.data$y)
#background points
bg_points <- dismo::randomPoints(mask = all_raster[[1]],
                                 n = 5000, #Between 5000 & 9000
                                 p = bio.data[, c("x", "y")],  # Ensure only numeric coordinates are passed from the presence points. Random points won't be in the same cells (as defined by mask)
                                 excludep = TRUE) #presence points are excluded from background


## Round x and y coordinates to 2 decimal places
bg_points <- round(bg_points, 2)

## create data frame with named colums, and join background data with presence data
#names(bio.data)
bg_points <-data.frame(name= chosen_col_999, x = bg_points[, 1], y = bg_points[, 2]) #check x is column 1 and y in column 2
bg_points[[chosen_col_999]]<-0 #add the last column as absence background points named after the chosen column

###BG point generated by KDE (For Chionis minor)
#bg_points <- read.table(path_biodata_PA, header = TRUE, sep = "")  

## check structure and plot data points
# print(head(bg_points))
# print(head(bio.data))
plot(bg_points$x, bg_points$y, col = 'red', pch = 3, main = "Presence and Background Points")
points(bio.data$x, bio.data$y, col = 'blue', pch = 16)


bio.data.PA <- rbind.data.frame(bio.data, bg_points) #join

################################################################################
### 12. Final occurences
################################################################################
################################################################################
### 12.1 Metadata occurences
################################################################################

T_nr <- 1

i <- 1
# loop over all selected species (= 'chosen_col_999')
for (i in 1:length(chosen_col_999)) {
  # print(i)
  if (exists("actual.path")) {rm(actual.path)}
  actual.path <- paste(path_results_folder,paste(chosen_col_999[T_nr], date.today, sep="_"), sep="/")
  
  # set wd to the actual path in the result folder
  if(getwd() != actual.path) { setwd(actual.path) }
  # getwd()
  
  ## remove everything in this directory
  do.call(file.remove, list(list.files(actual.path, full.names = TRUE)))
  
  ## write raster files metadata table
  write.csv(raster_meta_df, 
            file = paste(chosen_col_999[T_nr], "METADATA_raster_before_resampling.csv", sep="_"), 
            row.names = FALSE)
  
  if (exists("bio.data1")) { rm(bio.data1) }
  bio.data1 <- bio.data.PA[ ,c("x", "y", c(chosen_col_999[T_nr])) ] #bio.data instead of bio.data.PA when not using pseudo-absence background
  # summary(bio.data1)
  # table(bio.data1[, c(chosen_col_999[1])]) # (!) to check the values and arrange the table!, amount of row and order in factors
  df_01 <- data.frame(matrix(ncol=2,nrow=4, dimnames=list(NULL, c("value", "n"))))
  df_01[1] <- c("0", "1", "NA", "Total") #absence,presence, no data available, total
  df_01[1,2] <- as.integer(table( bio.data1[,c(chosen_col_999[1])] )[1])
  df_01[2,2] <- as.integer(table( bio.data1[,c(chosen_col_999[1])] )[2])
  df_01[3,2] <- as.integer(sum(is.na( bio.data1[,c(chosen_col_999[1])])))
  df_01[4,2] <- as.integer(nrow(bio.data1))
  
  write.table(df_01, 
              paste(chosen_col_999[T_nr], "DATA_before_cleaning_values_greater_1.txt", sep="_"), 
              sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  rm(df_01)
} # end of 'for (i in 1:length(chosen_col_999))'
rm(i)
rm(raster_meta_df)

################################################################################
### 12.2 Replace all '999' with NA
################################################################################
bio.data1[,c(chosen_col_999[1])] <- sapply(bio.data1[,c(chosen_col_999[1])],
                                           function(x) ifelse(x>1, NA, x))
# summary(bio.data1)

################################################################################
### 12.3 Remove all 'NA's'
################################################################################
# remove rows with NA data
bio.data1 <- na.omit(bio.data1)
# convert values in selected rows to integer
bio.data1[,c(chosen_col_999[T_nr])] <- as.integer( bio.data1[,c(chosen_col_999[T_nr])] )

# Convert biological data frame to a spatial object (sf) Here the projection WGS 84
# (EPSG4326) has to be chosen, because the x and y coordinates are in degrees
# (= longitude and latitude)
if (exists("sf.bio.data")) { rm(sf.bio.data) }
sf.bio.data = sf::st_as_sf(bio.data1, coords=c("x","y"), crs= "+proj=longlat +datum=WGS84 +no_defs" )
if (exists("shape_01")) { rm(shape_01) }

# now just keep the presence/absence data
if (exists("shape_01")) { rm(shape_01) }
shape_01 <- sf.bio.data
rm(sf.bio.data)
# now reproject the spatial biological data to projection of the raster stack ("all_raster")
if ( raster::projection(shape_01) != raster::projection(all_raster) ){
  shape_01 <- sf::st_transform( shape_01, crs=raster::projection(all_raster) )
}

################################################################################
### 12.4 Reduce number of points in raster cell to 1 (keep maximum value (0 or 1))
################################################################################
{
  if (exists("shape_df")) { rm(shape_df) }
  shape_df <- as.data.frame(shape_01)
  #plot(sp.shape)
  if (exists("myRespName")) { rm(myRespName) }
  myRespName <- colnames(shape_df[1]) #defined character as column
  species_name <- myRespName
  if (exists("myResp")) { rm(myResp) }
  myResp <- c( as.numeric(shape_df[,1]) )
  myResp[myResp > 1] <- NA  # convert all values > 1 (derived from the shapefile or txt) to NA values
  ### now analyse if there are more than one point in the raster cells. If so, melt it down to one
  ### point per cell with the maximum value of the points in this cells
  # convert to a sp spatial object
  if (exists("sp.shape")) { rm(sp.shape) }
  sp.shape <- as(shape_01[, toString(myRespName)], "Spatial")
  # select one raster of the raster stack to get the grid information
  raster.file <- all_raster[[1]]
  # Now use the our function 'pointcount' to count the number of intersection point
  # in each raster cell
  if (exists("r2")) { rm(r2) }
  r2 = pointcount(raster.file, sp.shape)
  ## create new raster 'r3' to calculate with
  if (exists("r3")) { rm(r3) }
  r3 <- r2
  if (exists("r4")) { rm(r4) }
  ## get the maximum value of the points if there are more than one point in a cell
  r4 <- rasterize(sp.shape, raster.file,
                  fun='max',   # select the maximum value of the observation
                  background=NA,
                  field = toString(myRespName))
  # plot(r4)
  ## make new point data frame with this calculated maximum (see step before)
  ## Raster to point conversion ('raster::rasterToPoints').
  ## Cells with NA are not converted.
  if (exists("pts2")) {rm(pts2)}
  # convert raster 'r4' to spatial points
  pts2 <- raster::rasterToPoints(r4, spatial=TRUE)
  #names(pts2)[1] <- as.character(myRespName)
  pts2_presence <- subset(pts2, pts2$layer > 0) 
  pts2_absence <- subset(pts2, pts2$layer < 1)
}

 #------------------------------------------------------
#  table(as.data.frame(pts2$layer))
{
  df_01 <- data.frame(matrix(ncol=2,nrow=3, dimnames=list(NULL, c("value", "n"))))
  df_01[1] <- c("0", "1", "Total")
  df_01[1,2] <- as.integer( table(as.data.frame(pts2$layer))[1] )
  df_01[2,2] <- as.integer( table(as.data.frame(pts2$layer))[2] )
  df_01[3,2] <- as.integer( nrow(pts2) )
  
  write.table(df_01, 
              paste(chosen_col_999[T_nr], "DATA_after_cleaning_and_reducing_to_one_per_raster_cell.txt", sep="_"), 
              sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  rm(df_01)
}
#-----------------------------------------------------------------------------
rm(shape_01,sp.shape, raster.file, r2, r3)

##############################################################################
### 13. Extract all raster values to the point data (2nd)
##############################################################################
# extract all raster values to the point data
if(exists("species.values")){ rm(species.values) }
species.values <- as.data.frame(raster::extract(all_raster,
                                                pts2,
                                                method ="simple")) # method: 'simple' or 'bilinear'. If 'simple' values for the cell a point falls in are returned. If 'bilinear' the returned values are interpolated from the values of the four nearest raster cells.
dat <- cbind(c(pts2$layer), species.values)
names(dat)[1] <- "pres_absence"

##############################################################################
### 13.1 Remove rows with NAs (= no environmental raster information)
##############################################################################
unique(which(is.na(species.values)))
test <- species.values[c(complete.cases(species.values)),]
pts2 <- pts2[c(complete.cases(species.values)),]
test3 <- as.data.frame(cbind(test, pts2))

##############################################################################
### 13.2 Find all Zero- and Near Zero-Variance Predictors
##############################################################################
# Zero- and Near Zero-Variance Predictors
# see: https://topepo.github.io/caret/pre-processing.html#zero--and-near-zero-variance-predictors
# Two metrics can be calculated:
#  - the frequency of the most prevalent value over the second most frequent
#    value (called the "frequency ratio''), which would be near one for
#    well-behaved predictors and very large for highly-unbalanced data and
#  - the "percent of unique values'' is the number of unique values divided
#    by the total number of samples (times 100) that approaches zero as the
#    granularity of the data increases
{
  nearZero_df <- as.data.frame(caret::nearZeroVar(dat[c(2:ncol(dat))], saveMetrics= TRUE))
  # View(nearZero_df)
  write.csv(nearZero_df, 
            paste(chosen_col_999[T_nr], "Zero_and_Near_Zero_Variance_Predictors_table.csv", sep="_"), 
            row.names = TRUE)
}
##############################################################################
### 13.3 Remove Zero- and Near Zero-Variance Predictors (if exists !)
##############################################################################
# remove Zero- and Near Zero-Variance Predictors (if exists !)
{
  nzv <- caret::nearZeroVar(dat[c(2:ncol(dat))])
  if (input_ZNZ == "yes" & length(nzv)>=1){
    reduced_Data <- dat[, -nzv]
  } else {
    reduced_Data <- dat
  }
  rm(nearZero_df, nzv)
}

##############################################################################
### 13.4 Calculate correlation
##############################################################################
# The Caret R package provides the findCorrelation which will analyze a
# correlation matrix of your data attributes report on attributes that
# can be removed.
{
  # calculate Correlation Matrix of raster values
  correlationMatrix <- stats::cor(reduced_Data[c(2:ncol(reduced_Data))], use = "complete.obs", method = chosen_corr_method)
  # summarize the correlation matrix
  print(correlationMatrix)
  
  ##############################################################################
  ###     13.4.1. Save Correlation matrix as table and as figure
  ##############################################################################
  write.csv(correlationMatrix, 
            paste(chosen_col_999[T_nr], "_CORRELATION_TABLE_raster_values_pairwise_", chosen_corr_method, ".csv", sep=""), 
            row.names = TRUE)
  
  # save corrplot with significances as png
  #dev.off()
  graphics.off()
  Cairo::Cairo(
    55, #length
    55, #width
    file = paste(chosen_col_999[T_nr], "_CORRELATION_PLOT_SIGNIFICANCE_raster_values_pairwise_", chosen_corr_method, ".png", sep=""),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement
    dpi = 300,
    pointsize=14,
    units = "cm" #you can change to pixels etc
  )
  options(warn=-1) # warnings off, when computing spearman correlation The warning only affects the computation of the p-values, not the correlation coefficient itself
  PerformanceAnalytics::chart.Correlation(reduced_Data[c(2:ncol(reduced_Data))],
                                          method = chosen_corr_method,
                                          histogram=TRUE, pch="+") 
  # dev.off()
  graphics.off()
  
  graphics.off()
  Cairo::Cairo(55, 55, file = paste(chosen_col_999[T_nr], "_CORRELATION_PLOT_SIGNIFICANCE_raster_values_pairwise_pearson.png", sep=""),
    type = "png", bg = "white",dpi = 300, pointsize=14,units = "cm")
  PerformanceAnalytics::chart.Correlation(reduced_Data[c(2:ncol(reduced_Data))],
                                          method = c("pearson"),
                                          histogram=TRUE, pch="+") 
  # dev.off()
  graphics.off()
  
  
  # save simple corrplot as png
  #library(Cairo)
  Cairo::Cairo(
    35, #length
    35, #width
    file = paste(chosen_col_999[T_nr], "_CORRELATION_PLOT_raster_values_pairwise_", chosen_corr_method, ".png", sep=""),
    type = "png", #tiff
    bg = "white", #white or transparent depending on your requirement
    dpi = 300,
    pointsize=14,
    units = "cm" #you can change to pixels etc
  )
  corrplot::corrplot.mixed(correlationMatrix,
                           lower = "number",
                           upper = "circle",
                           tl.pos="lt",
                           diag="u",
                           bg = "white",
                           addgrid.col = "gray")
  # dev.off()
  graphics.off()
} 

##############################################################################
### 13.4.2. Find raster attributes that are highly corrected (with the cutoff.value the user defined)
##############################################################################

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff=hcv_cutoff, exact = TRUE)
# print indexes of highly correlated attributes
highlyCorrelated <- sort(highlyCorrelated)
# print the names of the highly correlated raster variables

print(sort(c(names(reduced_Data[c(2:ncol(reduced_Data))])[highlyCorrelated])))

if( exists("hc.predictors")) { rm(hc.predictors) }
hc.predictors <- data.frame(matrix(NA, nrow = length(highlyCorrelated), ncol = 0))
hc.predictors$high_correlated_predictors <- c(colnames(species.values)[highlyCorrelated])
#hc.predictors$cutoff_value <- paste(">=", hcv_cutoff, sep="")

##############################################################################
### 13.4.3. Remove these highly correlated raster attributes
##############################################################################
write.csv(hc.predictors,
          paste(chosen_col_999[T_nr], "_HIGHLY_CORRELATED_raster_values_", chosen_corr_method, "_correlation_table.csv", sep=""),
          row.names = TRUE)
## here add possibility to select variables to drop
if (input_01 == "yes"){
  reduced_Data = reduced_Data[,-c(highlyCorrelated)]
}
rm(highlyCorrelated, hc.predictors)
# View(reduced_Data)

##############################################################################
### 13.5 Multicollinearity
##############################################################################
### Check for Multicollinearity - Variance Inflation Factor (package 'performance v0.7.0')
m_calc <- lm(pres_absence ~ ., reduced_Data)
result <- check_collinearity(m_calc)
# plot results
if (require("see")) {
  x <- check_collinearity(m_calc)
  plot(x)
}
dev.off()
graphics.off()

# save corrplot as png
Cairo::Cairo(
  35, #length
  35, #width
  file = paste(chosen_col_999[T_nr], "_MULTICOLLINEARITY_PLOT_VIF.png",sep=""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement
  dpi = 300,
  pointsize=16,
  units = "cm" #you can change to pixels etc
)
if (require("see")) {
  x <- check_collinearity(m_calc)
  plot(x)
}
dev.off()
graphics.off()


{if (exists("result_multic")) {rm(result_multic)}
  result_multic <- as.data.frame(result[,1])
  names(result_multic)[1] <- "Variables"
  result_multic$VIF <- c(result$VIF)
  result_multic$Increased_SE <- c(result$SE_factor)
  result_multic$multicollinearity <- "low"
  
  result_multic <- result_multic %>%
    dplyr::mutate(
      multicollinearity = dplyr::if_else(VIF >= 10, "high", multicollinearity)
    )
  result_multic <- result_multic %>%
    dplyr::mutate(
      multicollinearity = dplyr::if_else(VIF < 10 & VIF >= 5, "moderate", multicollinearity)
    )
  
  write.csv(result_multic,paste(chosen_col_999[T_nr], "_MULTICOLLINEARITY_TABLE_different_classes.csv", sep=""))
}

## if "automatic removal of variables with high multicollinearity"  was chosen,
## all variables with a high VIF (user chosen threshold) will be removed now
if (input_mc == "yes"){
  d.names <- c(result_multic[result_multic$VIF >= mc_cutoff,1])
  reduced_Data <- reduced_Data[ , -which(names(reduced_Data) %in% c(d.names))]
  rm(d.names)
}

rm(m_calc, result, x, result_multic, input_mc)

##############################################################################
### 13.6 Calculate predictor variable importance
##############################################################################
#### Calculate variable importance and add results to table
  
  {   ###  Do some preliminary work
    bioenv_data <- reduced_Data
    names(bioenv_data)[1] <- "pres_absence"
    # rm(list = ls()[!ls() %in% c("bioenv_data")])
    bioenv_data$pres_absence  <- as.factor(as.numeric(unlist(bioenv_data$pres_absence)))
    
    ### create a new empty data frame for the results
    result_table_variable_importance <- data.frame(matrix(vector(), 
                                                          c(ncol(bioenv_data)-1), 1,
                                                          dimnames=list(c(), c("Predictor"))),
                                                   stringsAsFactors=F)
    # now sort the variable names alphabetically because randomForest will do so either
    result_table_variable_importance[1] <- c(sort(names(bioenv_data[2:ncol(bioenv_data)])) )
    
    ## 1. Random Forest
    # run the randomForest implementation
    # library(randomForest)
    
    # Your data contains NAs, so randonForest will not run. You have to remove
    # the rows with NAs in your data:
    train <- bioenv_data
    row.has.na <- apply(train, 1, function(x){any(is.na(x))})
    bioenv_data <- train[!row.has.na, ]
    rm(train)
  }   ### End of Do some preliminary work
  
  ###
  
  {   ### Random Forest to calculate variable importance
    rf1 <- randomForest::randomForest(pres_absence~., 
                                      data=bioenv_data, 
                                      mtry= c(ncol(bioenv_data)-1), 
                                      ntree=3000, 
                                      importance=TRUE)
    rf_imp <- as.data.frame(importance(rf1,type=1), row.names = FALSE)
    rf_imp$Predictor <- rownames( importance(rf1,type=1) )
    rf_imp <- rf_imp[order(rf_imp$Predictor),]
  }   ### End of Random Forest
  
  ###
  
  {    ### add variable importance to result table
    
    result_table_variable_importance$Rf_importance <- as.numeric(rf_imp[,1])
    result_table_variable_importance <- result_table_variable_importance[order(result_table_variable_importance$Rf_importance),]
    
    foo <- function(x, round=10) ceiling(max(x+10^-9)/round + 1/round)*round
    
    # Save result table as csv
    write.csv(result_table_variable_importance, 
              paste(chosen_col_999[T_nr], "_VARIABLE_IMPORTANCE_TABLE_calculated_with_random_forest.csv", sep=""),
              row.names = FALSE)
    
    result_table_variable_importance_scaled <- result_table_variable_importance
    tttt <- c(result_table_variable_importance_scaled$Rf_importance)
    tttt <- scale(tttt,FALSE,max(tttt))
    result_table_variable_importance_scaled$Rf_importance <- as.numeric(tttt)
    rm(tttt)
    
    png(filename=paste(chosen_col_999[T_nr], "_VARIABLE_IMPORTANCE_BARPLOT_SCALED_calculated_with_random_forest.png", sep=""))
    par(mar=c(5,8,2,2)) # margins down, left, up, right
    barplot(height=result_table_variable_importance_scaled$Rf_importance, 
            names=result_table_variable_importance_scaled$Predictor, 
            col="khaki", 
            xlab = "Scaled variable importance calculated with random forest",
            horiz=T_nr, 
            las=1,
            xlim=c(0, max(result_table_variable_importance_scaled$Rf_importance))
    )
    dev.off()
    graphics.off()
    
    # write result table (scaled values) as csv
    write.csv(result_table_variable_importance_scaled, 
              paste(chosen_col_999, "_VARIABLE_IMPORTANCE_TABLE_SCALED_calculated_with_random_forest.csv", sep=""),
              row.names = FALSE)
  }   ### End of add variable importance to ...
  
  # remove uneeded files
  rm(foo, result_table_variable_importance)
  
 ### End of Calculate variable importance and add results to table

###############################################################################
###############################################################################

################################################################################
### 14. Analyse spatial autocorrelation for the remaining raster data
################################################################################
  
  ###  Spatial analyse
    ## now stack all remaining raster files
    names( bioenv_data[2:ncol( bioenv_data)]) 
    
     if(exists("myExpl")) { rm(myExpl) }
    myExpl <- all_raster[[c(names(bioenv_data[2:ncol(bioenv_data)]))]]
    
    spat_raster <- terra::rast(myExpl) #as SpatRaster from terra
    
    pts2_sf <- st_as_sf(pts2) #as spatial feature sf object
    
    
    options(warn=-1) # warnings off
    if (exists("sac")) {rm(sac)}
    # see: https://htmlpreview.github.io/?https://github.com/rvalavi/blockCV/blob/master/inst/doc/tutorial_1.html
    # Block cross-validation for species distribution modelling,  Roozbeh Valavi, Jane Elith, Jos? Lahoz-Monfort and Gurutzeta Guillera-Arroita
    # 2020-02-17
    set.seed(100)
    #sf::sf_use_s2(FALSE)
  { sac <- blockCV::cv_spatial_autocor (r = spat_raster,
                                          num_sample = 5000,
                                          deg_to_metre = cos(mean(sf::st_bbox(pts2_sf)[c(2,4)]) * pi/180) * 111325,
                                          plot= TRUE,
                                          progress = TRUE)
    #save plot
    dev.copy(png, file = paste(chosen_col_999[T_nr], "_SPATIAL_AUTOCORRELATION_plot.png", sep="_"))
    dev.off()
    
    options(warn=0)
    summary(sac)
      
      # save result values as txt
      write.table(sac$range_table, 
                  paste(chosen_col_999[T_nr], "_SPATIAL_AUTOCORRELATION_TABLE.txt", sep="_"), 
                  sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
      
      write.table(summary(sac$range_table), 
                  paste(chosen_col_999[T_nr], "_SPATIAL_AUTOCORRELATION_SUMMARY_TABLE.txt", sep="_"), 
                  sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
         }
  ### End of spatial analyse

### create plots of spatial autocorrelation without outliers and save as png
  
    ############################################################################
    trtr <- as.data.frame( sac$range_table[,c(1:2)] )
      
      #find Q1, Q3, and interquartile range for values in column A
      Q1 <- quantile(trtr$range, .25)
      Q2 <- quantile(trtr$range, .5)
      Q3 <- quantile(trtr$range, .75)
      IQR <- IQR(trtr$range)
      
      trtr <- subset(trtr, trtr$range > (Q1 - 1.5*IQR) & trtr$range < (Q3 + 1.5*IQR))
      trtr <- trtr[order(trtr$range), ]
      graphics.off()
      
        Cairo::Cairo(
          20, #length
          15, #width
          file = paste(chosen_col_999[T_nr], "_SPATIAL_AUTOCORRELATION_RANGE_MAP_WITHOUT_OUTLIER", ".png", sep = ""),
          type = "png", #tiff
          bg = "white", #white or transparent depending on your requirement 
          dpi = 300,
          units = "cm" #you can change to pixels etc 
        )
        
        P1 <- ggplot(trtr, aes(y = range, x = layers) ) + 
          geom_bar(stat = "identity", fill = "steelblue1", show.legend = FALSE) +
          xlab("Layers") + ylab("Range (m)") +
          theme(# axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            # axis.ticks.x=element_blank()
          ) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          ggtitle("Autocorrelation range without outlier",
                  subtitle = "Based on 5000 sample points") + 
          geom_hline(yintercept=Q2, color = "red", linetype="dashed") +
          geom_text(aes(x = 2, y = Q2,label = paste("Block size: ", round(Q2, 0), sep=""), 
                        vjust = -1, color = "red"), show.legend = FALSE )
        plot(P1)
      
      dev.off()
      graphics.off()
      rm(Q1, Q3, IQR, trtr, P1)
    
    # End of create plots and save as png

################################################################################
### 14.1 Outlier detection and define range size
################################################################################
summary(sac$range_table)
median_range.size <-round(median(sac$range_table[,2]),0)

{  ### Finding outliers
  Q <- quantile(sac$range_table[,2], probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(sac$range_table[,2])
  # Now that you know the IQR and the quantiles, you can find the cut-off ranges 
  # beyond which all data points are outliers.
  up <-  Q[2]+1.5*iqr # Upper Range  
  low<- Q[1]-1.5*iqr # Lower Range
  
  # The code for removing the outliers is
  eliminated <- subset(sac$range_table[,2], 
                       sac$range_table[,2] > (Q[1] - 1.5*iqr) & sac$range_table[,2] < (Q[2]+1.5*iqr))
  
  range.size <- round(median(eliminated),0)
}   ### End of Finding outliers

# remove unneeded files
rm(Q, iqr, up, low, eliminated)

################################################################################
### 14.2 Spatial blocking to see only where the ocurrences are
################################################################################
names(pts2)[1] <- as.character(myRespName)
names(pts2_sf)[1] <- as.character(myRespName)
#sf::sf_use_s2(FALSE)
sac2 <- blockCV::cv_spatial_autocor (x = pts2_sf,
                                     column = species_name,
                                     deg_to_metre = cos(mean(sf::st_bbox(pts2_sf)[c(2,4)]) * pi/180) * 111325,
                                     progress = TRUE,
                                     plot= TRUE)

biodata_range.size <- round(sac2$range,0)

################################################################################
### 14.3 Final blocking
################################################################################
#print two potential range size
median_range.size #median of environmental variables
range.size #median of environmental variable without outliers 
biodata_range.size #range size where biological data (ocurrences are available)

#Control steps
# # Check if species_name column exists in pts2_sf
# if (!species_name %in% colnames(pts2_sf)) {
#   stop("species_name column not found in pts2_sf")
# }
# 
# # Ensure CRS of pts2_sf and spat_raster match
# if (st_crs(pts2_sf) != st_crs(spat_raster)) {
#   pts2_sf <- st_transform(pts2_sf, st_crs(spat_raster))
# }

# Reproyectar a un CRS en metros (por ejemplo, UTM zona 21S)
pts2_sf_utm <- st_transform(pts2_sf, crs = "+proj=utm +zone=19 +south +datum=WGS84 +units=m")
spat_raster <- terra::rast(myExpl) #as SpatRaster from terra
#spat_raster_utm <- project(spat_raster, "+proj=utm +zone=19 +south +datum=WGS84 +units=m")


# # Ensure range.size is valid
# if (is.null(range.size) || !is.numeric(range.size)) {
#   stop("range.size is not a valid numeric value")
# }

if (exists("sb")) {rm(sb)}
for (l in seq(2,20,2)) { # this loop will be done until the value of l is equal to the number of blocks
  print(l)

  ## this is done to calculate the number of blocks containing biological information
  sb <- blockCV::cv_spatial(pts2_sf,
                            column = species_name,
                            r = spat_raster,
                            k = 5,
                            size = range.size,
                            #size = median_range.size,
                            #size = biodata_range.size,
                            #size = NULL,
                            hexagon = FALSE,
                            selection = "random", # randomly grouping the folds
                            iteration = 100, # find evenly dispersed folds
                            biomod2 = TRUE, # creates a table format you can use in biomod
                            extend = 1.0,
                            raster_colors = terrain.colors(10, rev = TRUE))
}
## l = number of blocks containing biological information
l <- length(c(sb$blocks$folds))

## for time efficiency & working in biomod, we have to divide the block number by 2
# print(l)
 if((l %% 2) == 0) { L2 <- l/2 }              # il l is even, just divide it by 2
 if((!l %% 2) == 0) { L2 <- floor((l-1)/2) }  # if l is odd, divide it by 2 and round it down to the next natural number
# #print(L2)



## This function creates spatially separated folds based on a pre-specified distance. 
## It assigns blocks to the training and testing folds randomly and it maximizes k
if (exists("sb")) {rm(sb)}
sb <- blockCV::cv_spatial(pts2_sf,
                          column = species_name,
                          r = spat_raster,
                          k = L2,
                          size = range.size,
                          #size = median_range.size,
                          #size = biodata_range.size,
                          #size = NULL,
                          hexagon = FALSE,
                          selection = "random", # randomly grouping the folds
                          iteration = 100, # find evenly dispersed folds
                          biomod2 = TRUE, # creates a table format you can use in biomod
                          extend = 1.0,
                          raster_colors = terrain.colors(10, rev = TRUE))

#Plot folds and blocks
# adding points on spatialBlock plot
mapdata1 <- data.frame(pts2_presence)
mapdata2 <- data.frame(pts2_absence)

folds_plot<- cv_plot(cv = sb,
                     r = spat_raster,
                     raster_colors = terrain.colors(10, alpha = 0.5),
                     label_size = 4) 

folds_plot <- folds_plot + geom_point(data=mapdata2, aes(x=x, y=y), color="grey", cex=0.35, alpha = 0.5)
folds_plot <- folds_plot + geom_point(data=mapdata1, aes(x=x, y=y), color="blue", cex=0.45, alpha = 0.5)
folds_plot

#save plot white background
dev.off()
graphics.off()
Cairo::Cairo(
  20, #length
  15, #width
  file = paste(chosen_col_999[T_nr], "_SPATIAL_BLOCKS_with_random_fold_assigment", ".png", sep = ""),
  type = "png", #tiff
  bg = "white", #white or transparent depending on your requirement
  dpi = 300,
  units = "cm" #you can change to pixels etc
)
folds_plot
dev.off()
graphics.off()
rm(folds_plot)

rm(input_01, input_03, input_ZNZ)
save.image(paste(actual.path, paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep="/"))

###############################################################################################
###############################################################################################
###
###  15 - B I O M O D   version 4
###
###############################################################################################

################################################################################
{  ###  here again the possibility to change BIOMOD model selection
  
  cm <- chosen_models
  
  model_list <- c("GLM", "GBM", "GAM", "CTA", "ANN", "SRE", "FDA", "MARS", "RF", "MAXENT", "XGBOOST", "MAXNET")
  chosen_models <-c()
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose the models for BIOMOD")$res
    chosen_models <- utils::select.list(model_list, preselect = cm, multiple = TRUE,
                                        title = "Choose one or more models", graphics = TRUE)
    if(length(chosen_models)>=1){
      answer <- "yes"
    }
  }
  rm(model_list, answer)
  # chosen_models
  
  rm(cm)
}   ### End of Possibility change BIOMOD model selection



##############################################################################
##  all needed evaluation methods(KAPPA, TSS, ROC) will be added automatically
##  if necessary

{  ### Possibility to choose evaluation methods
  
  ce <- chosen_evals
  
  eval_list <- c("KAPPA", "TSS", "ROC", "FAR", "SR", "ACCURACY", "BIAS", "POD", "CSI", "ETS")
  chosen_evals <-c()
  answer <- "no"
  while (answer=="no"){
    svDialogs::dlgMessage("Choose the evaluation metrics for BIOMOD")$res
    chosen_evals <- utils::select.list(eval_list, preselect = c("ROC", "TSS", ce), multiple = TRUE,
                                       title = "Choose one or more evaluations", graphics = TRUE)
    if(length(chosen_evals)>=1){
      answer <- "yes"
    }
    # } # end of 'while (answer=="no")'
    # rm(eval_list, answer)
    chosen_evals
    
    item <- c("KAPPA", "ROC", "TSS")
    # add "ROC" and "TSS" to evaluation list if not already in the list
    for (o in 1:length(item)){
      if (item[o] %in% chosen_evals) {
        o <- o+1
      } else {
        # print(paste("The method ", item[o] , "will be added to the evaluation method list.", sep=""))
        chosen_evals <- sort(c(chosen_evals, item[o]))
      }  # end of 'else'
    }  # end of 'for (o in 1:length(item))'
    rm(o)
    rm(ce)
  } # end of 'while (answer=="no")'
  
  rm(eval_list, answer)
  
}  ### End of Possibility to choose evaluation methods

##############################################################################

{   ### Do some preliminary work for BIOMOD: species occurrences etc
  
  test <- merge(bioenv_data, pts2, by="row.names",all.x=TRUE)
  test <- test[,c(names(pts2)[1], "x", "y")]
  
  DataSpecies <- as.data.frame(test)
  rm(test)
  # the name of studied species
  myRespName <- names(pts2)[1]
  
  
  # the presence/absences data for our species
  myResp <- as.numeric(DataSpecies[,1])
  # the XY coordinates of species data
  myRespXY <- DataSpecies[,c("x","y")]
  
  
}  ### End of Preliminary work for BIOMOD  


################################################################################
### 15.1 BIOMOD - Formatting data
################################################################################
### Initialize the datasets for usage in biomod2
#OCC_combined_ecospat<- cbind(myRespXY, data.frame(myResp))
#new_myResp <- OCC_combined_ecospat[OCC_combined_ecospat$myResp == 1, ]
#myResp<-as.numeric(new_myResp$myResp == 1)
#MyRespXY <- occ1_myresp[, 1:2]

myBiomodData <- biomod2::BIOMOD_FormatingData(resp.var = new_myResp,
                                              expl.var = myExpl, # explanatory raster data as RasterStack
                                              resp.xy = MyRespXY1,
                                              resp.name = myRespName,
                                             #PA.nb.rep = 1,
                                              #PA.nb.absences = 1000,
                                              #PA.strategy = "random",
                                              filter.raster = TRUE, #point per cell, but this was previously controlled
                                              na.rm = TRUE)

################################################################################
### 15.2 BIOMOD - Defining the folds for DataSplitTable (Neder et al. 2024)
################################################################################
if (exists("DataSplitTable")) { rm(DataSplitTable) }

### Method 1: blockCV based on the effective range of spatial autocorrelation calculated as range size and the block where presence/absences are located
# note that biomodTable should be used here not folds. (see Neder et al. 2024 MEPS)
DataSplitTable <- sb$biomod_table # use generated folds from spatialBlock in previous section

#change names for biomod split
colnames(DataSplitTable) <- paste0("_allData_RUN", 1:ncol(DataSplitTable))

################################################################################
### 15.3 BIOMOD - Model fitting
################################################################################
if(exists("myBiomodModelOut")) {rm(myBiomodModelOut)}

run_number <- as.integer(ncol(DataSplitTable)) #set by k in the DataSplitTable from spatial autocorrelation
perm_number <- 10  # Number of permutation to estimate variable importance

ptm <- proc.time() # Start the clock! measure of processing time

suppressWarnings(myBiomodModelOut <- biomod2::BIOMOD_Modeling( bm.format = myBiomodData,
                                                               models = chosen_models,# the chosen model algorithms
                                                               #bm.options = myBiomodOption, # where to find algorithms
                                                               CV.strategy = "kfold", #"user.defined"
                                                               CV.user.table = DataSplitTable , # blocking folds as table                                                              
                                                               CV.balance = "presences",
                                                               CV.k = l,
                                                               data.split.perc = 80,
                                                               nb.rep = run_number, # Number of number of repetitions to be done for calibration/validation splitting, ignored when defined data.split.per
                                                               weights = NULL,
                                                               prevalence = 0.5,
                                                               var.import = perm_number, # num of bootstraps to determine var importance
                                                               metric.eval = chosen_evals,
                                                               scale.models = FALSE, # run using all training data
                                                               modeling.id = paste("BIOMOD_analyse_", chosen_col_999[T_nr], "_", date.today, sep="") )
)

################################################################################
### 15.4 BIOMOD -  E V A L U A T I O N   O F   M O D E L S
################################################################################
# Save whole workspace to result directory as RData image
save.image(paste(actual.path, paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep="/"))

# load(paste(path_results_folder, "/Environment_BIOMOD_results.RData", sep=""))

################################################################################
### 15.4.1 BIOMOD -  get all models evaluation including CutOff value, Sensitivity, specificity
################################################################################

if(exists("myBiomodModelEval")) {rm(myBiomodModelEval)}
myBiomodModelEval <- biomod2::get_evaluations(myBiomodModelOut)
myBiomodModelEval[]

myBiomodModelEval <- dplyr::arrange(myBiomodModelEval, desc(metric.eval), 
                            desc(validation), 
                            algo)

write.csv(myBiomodModelEval, paste(chosen_col_999[T_nr], "_BIOMOD_results_myBiomodModelEval.csv", sep=""))

# Calculate summary statistics
summary_stats_eval <- myBiomodModelEval %>%
                       group_by(algo, metric.eval) %>%
                       summarise(min_validation = min(validation),
                           mean_validation = mean(validation),
                           max_validation = max(validation)
)
print(summary_stats_eval)

write.csv(summary_stats_eval, paste(chosen_col_999[T_nr], "_BIOMOD_results_Evaluation_model_summary_scores.csv", sep=""))

#plot evaluation
{
  png(filename = (
       paste(chosen_col_999[T_nr], "_Evaluation_plot.png", sep = "")),
       width = 900, height = 496, units = "px", pointsize = 10, res = 150)

  #algo_colors <- c( "GAM" = "cyan", "GBM" = "blue", "GLM" = "green",  "RF" = "red", "MAXENT" ="black","SRE" ="violetred")# Adjust based on the algorithms you have
  bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'algo'), do.plot = TRUE)
  dev.off()
}

bm_PlotEvalBoxplot(bm.out = myBiomodModelOut, group.by = c('algo', 'run'))
bm_PlotEvalMean(bm.out = myBiomodModelOut, group.by = "algo")

################################################################################
### 15.4.2 BIOMOD -  Calculation of the importance of the individual variables of all models
################################################################################   
var.import <- biomod2::get_variables_importance(myBiomodModelOut)
var.import

{
  png(filename = (paste(chosen_col_999[T_nr], "_VariableImportance_plot.png", sep = "")),
    width = 900, height = 496, units = "px", pointsize = 10, res = 150)
  
  bm_PlotVarImpBoxplot(myBiomodModelOut,
                       group.by = c("expl.var", "algo","algo"))
   
  dev.off()
}

# bm_PlotVarImpBoxplot(myBiomodModelOut,
#                       group.by = c("algo", "expl.var","expl.var"))
# bm_PlotVarImpBoxplot(myBiomodModelOut,
#                      group.by = c("run", "expl.var", "algo"))

summary_stats_var <- var.import %>%
  group_by(algo, expl.var) %>%
  summarise(min_importance = min(var.imp),
    mean_importance = mean(var.imp),
    max_importance = max(var.imp))
print(summary_stats_var)

write.csv(summary_stats_var, paste(chosen_col_999[T_nr], "_BIOMOD_results_variables_importance.csv", sep=""))


save.image(paste(actual.path, paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep="/"))


################################################################################
################################################################################
### 15.5 BIOMOD -  E N S E M B L E   M O D E L I N G 
################################################################################ 
#arrange values depending on metrics and select threshold
summary_eval <- aggregate(validation ~ metric.eval, data = myBiomodModelEval, 
                          FUN = function(x) c(min = min(x), mean = mean(x), max = max(x)))
summary_eval <- do.call(data.frame, summary_eval)
summary_eval

#loop for threshold selection
{ 
  tss_data <- myBiomodModelEval[myBiomodModelEval$metric.eval == "TSS", ] #TSS selected metric for EM
  tss_data$validation <- as.numeric(tss_data$validation)
  dlgMessage("Please select the valuation method to which the threshold should be applied")$res
 
  treshold <- 0.70
  res <- "no" 
  while(res == "no"){ 
    
  treshold <- as.numeric(dlgInput(paste("Define a threshold for TSS between ",
                                      min(tss_data$validation)," and ", max(tss_data$validation) ), 
                                     default = treshold)$res)
  model_subset <- subset(tss_data, tss_data$validation >= treshold)
  text <- (paste("With this threshold ", nrow(model_subset),"out of ", nrow(tss_data), "models are selected. Do you want to use this treshold?"))

  res <- dlgMessage(text, "yesno")$res 
  }
  if(res == "no") {treshold <- as.numeric(dlgInput(paste("Define a threshold for TSS between ",
                                                         min(tss_data$validation)," and ", max(tss_data$validation) ), 
                                                   default = treshold)$res)
  model_subset <- subset(tss_data, ttss_data$validation >= treshold)
  text <- (paste("With this threshold ", nrow(model_subset),"out of ", nrow(tss_data), "models are selected. Do you want to use this treshold?"))
  res <- dlgMessage(text, "yesno")$res 
  print(res)
  }
 }
    

treshold
models_used_in_EM<-model_subset$full.name

# Doing Ensemble Modelling
setwd(actual.path) #set directory where all results are saved

if (exists("myBiomodEM")) { rm(myBiomodEM) }
myBiomodEM <- BIOMOD_EnsembleModeling( bm.mod = myBiomodModelOut,
                                       models.chosen = c("all"),
                                       em.by = 'all',          # Flag defining the way the models will be combined to build the ensemble models. Available values are 'PA_dataset+repet' (default), 'PA_dataset+algo', 'PA_dataset', 'algo' and 'all'
                                       metric.select = c('TSS'),
                                       metric.select.thresh = c(treshold),
                                       metric.eval = c('TSS','ROC'), #c(chosen_evals),
                                       em.algo = c('EMwmean','EMca'), #c('EMmean', 'EMmedian', 'EMcv', 'EMci', 'EMca', 'EMwmean'),
                                       EMci.alpha = 0.05,
                                       EMwmean.decay = 'proportional', # Define the relative importance of the weights. A high value will strongly discriminate the 'good' models from the 'bad' ones
                                       var.import = perm_number) 
# print summary
myBiomodEM

################################################################################
### 15.5.1 BIOMOD -  get all models evaluation for Ensemble Model
################################################################################
StatsEM<-get_evaluations(myBiomodEM)
StatsEM
write.csv(StatsEM, file =paste(chosen_col_999[T_nr], "_BIOMOD_results_myBiomodModelEM_StatsEval.csv", sep=""))

#plot evaluation
{
  png(filename = (
    paste(chosen_col_999[T_nr], "_Evaluation_plot.png", sep = "")),
    width = 900, height = 496, units = "px", pointsize = 10, res = 150)
  
  bm_PlotEvalMean(bm.out = myBiomodEM, metric.eval = NULL, group.by = 'algo', do.plot = TRUE)
    dev.off()
}

################################################################################
### 15.5.2 BIOMOD -  Calculation of the importance of variables for Ensemble
################################################################################   
#variable importance
var.import.EM <- biomod2::get_variables_importance(myBiomodEM)
var.import.EM
{
  png(filename = (paste(chosen_col_999[T_nr], "_VariableImportance_EnsembleModel_plot.png", sep = "")),
      width = 900, height = 496, units = "px", pointsize = 10, res = 150)
  
  bm_PlotVarImpBoxplot(myBiomodEM,
                       group.by = c("expl.var", "algo","algo"))
  
  dev.off()
}

bm_PlotVarImpBoxplot(myBiomodEM,group.by = c("algo", "expl.var","expl.var"))

png(filename = "VarImpBoxplot_algo_explvar.png", width = 900, height = 600, res = 150)
bm_PlotVarImpBoxplot(myBiomodEM, group.by = c("algo", "expl.var", "expl.var"))
dev.off() 

bm_PlotVarImpBoxplot(myBiomodEM,group.by = c("expl.var", "expl.var", "algo")) 

png(filename = "VarImpBoxplot_explvar_algo.png", width = 900, height = 600, res = 150)
bm_PlotVarImpBoxplot(myBiomodEM, group.by = c("expl.var", "expl.var", "algo"))
dev.off()  


tabla_variableEM <- bm_PlotVarImpBoxplot(myBiomodEM,
                                        group.by = c("expl.var", "algo","algo"))


write.csv(tabla_variableEM$tab, paste(chosen_col_999[T_nr], "_BIOMOD_results_varImp_EnsembleModel.csv", sep=""))


summary_stats_var <- var.import.EM %>%
  group_by(algo, expl.var) %>%
  summarise(min_importance = min(var.imp),
            mean_importance = mean(var.imp),
            max_importance = max(var.imp))
print(summary_stats_var)

write.csv(summary_stats_var, paste(chosen_col_999[T_nr], "_BIOMOD_results_summary_varImp_EnsembleModel.csv", sep=""))

#------------------------------------------------------------------------------
save.image(paste(actual.path, paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep = "/"))


################################################################################
###   15.6 BIOMOD - P R O J E C T I O N S 
################################################################################  
#if modeling has been already run to reload the projection object from hard-drive
# change directory and name accordingly
#setwd("../03_R_results/C_minor_analyse_nov._08_2024/layer/proj_current")
# dir()
# myBiomodProjtmp <- load("C_albus.current.projection.out")
# ### lets store this object with another name
# myBiomodProj <- get(myBiomodProjtmp)
# rm("myBiomodProjtmp")

################################################################################
###  15.6.1 BIOMOD -  projection over the study area under current conditions
###    for single algorithm
################################################################################  
#setwd(actual.path)
myBiomodProj <- BIOMOD_Projection( bm.mod = myBiomodModelOut,
                                   new.env = myExpl,
                                   proj.name = "current",
                                   binary.meth = "TSS",
                                   output.format = ".img",
                                   compress = FALSE,
                                   build.clamping.mask = FALSE,
                                   do.stack = FALSE )
myBiomodProj
plot(myBiomodProj)

################################################################################
###  15.6.2 BIOMOD -  projection Ensemble Model
################################################################################  
BiomodEM_Proj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'current ensemble',
                                             new.env = myExpl,
                                             models.chosen = 'all',
                                             metric.binary = 'TSS', 
                                             metric.filter = 'NULL',
                                             output.format = '.img',
                                             do.stack = FALSE)
BiomodEM_Proj
plot(BiomodEM_Proj)

save.image(paste(actual.path, paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep = "/"))


################################################################################
###   15.6.3 BIOMOD - B I N A R Y   T R A N S F O R M A T I O N
################################################################################
# We use the binary transformation generated froom biomod
setwd(paste(actual.path, myRespName, "proj_current ensemble","individual_projections", sep ="/")) #set directory where individual projection are located
img_EM_files <- dir(pattern = "\\.img$", full.names = FALSE, ignore.case = TRUE)
 img_EM_files
EMwmean_file <- img_EM_files[grep("EMwmeanByTSS_mergedData_mergedRun_mergedAlgo\\.img$", img_EM_files)]
EMca_file<- img_EM_files[grep("EMcaByTSS_mergedData_mergedRun_mergedAlgo\\.img$", img_EM_files)]

EMwmean_binary<-terra::rast(EMwmean_file)
cutoff_EMwM_TSS <-StatsEM$cutoff[StatsEM$metric.eval=="TSS"& StatsEM$algo == "EMwmean"]


BiomodEMwM_Proj_binary <- bm_BinaryTransformation (data = EMwmean_binary,
                                                    threshold = cutoff_EMwM_TSS,
                                                    do.filtering = FALSE)
#BiomodEMwM_Proj_binary2<- terra::rast(img_EM_files[grep("EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img$", img_EM_files)])

# setwd(actual.path)
# png("EMWmean_plot.png", width = 800, height = 600)  # Save the plot to a file
# plot(BiomodEMwM_Proj_binary,
#      main = "Binary Prjection of Ensemble wModel (TSS)",
#      col = c("lightgrey", "darkgreen"),
#      legend = TRUE,
#      axes = TRUE)
# legend("topright", legend = c("Absence", "Presence"),
#        fill = c("lightgrey", "darkgreen"),
#        bty = "n")
# dev.off()
# graphics.off()
# terra::writeRaster(BiomodEMwM_Proj_binary,
#                     paste(chosen_col_999[T_nr], "_BIOMOD_binarycutoff_EMwm.img", sep=""),
#                     overwrite=TRUE)


################################################################################
###  16 BIOMOD -  Standard deviation
################################################################################
setwd(paste(actual.path,chosen_col_999[T_nr],"proj_current","individual_projections", sep ="/")) #set directory where individual projection are located
#setwd(paste(actual.path,chosen_col_999[T_nr],"proj_current ensemble","individual_projections", sep ="/")) #set directory where individual projection are located

all_img_files<-dir(pattern=".img$", full.names = FALSE, ignore.case = TRUE)
selected_EMfiles <- all_img_files[grep(paste(models_used_in_EM, collapse="|"), all_img_files)]

raster_EMstack <- terra::rast(selected_EMfiles)
EM_sd <- terra::app(raster_EMstack, fun = sd, na.rm = TRUE) #standard deviation

setwd(actual.path)
png("sd_models_plot.png", width = 800, height = 600)  # Save the plot to a file
plot(EM_sd, main = "Standard Deviation of Selected Models", col = brewer.pal(9, "YlOrRd"), legend = TRUE)
#map("world", add = TRUE, col = "gray90", fill = TRUE)
dev.off()
graphics.off()

terra::writeRaster(EM_sd, "sd_models_raster.tif", overwrite = TRUE)

################################################################################
###  18 BIOMOD -  Response Curves
################################################################################
setwd(paste(actual.path, sep ="/"))
  
{
    myModelsGLM <- BIOMOD_LoadModels(myBiomodModelOut, algo ='GLM')
    myModelsGBM <- BIOMOD_LoadModels(myBiomodModelOut, algo ='GBM')
    myModelsSRE <- BIOMOD_LoadModels(myBiomodModelOut, algo ='SRE')
    myModelsRF <- BIOMOD_LoadModels(myBiomodModelOut,algo ='RF')
    myModelsMAXNET <- BIOMOD_LoadModels(myBiomodModelOut, algo = 'MAXNET')
    
    RC_data <- get_formal_data(myBiomodModelOut, 'expl.var')
    RC_variables <- get_formal_data(myBiomodModelOut, 'expl.var.names')
    RC_species <- get_formal_data(myBiomodModelOut, 'resp.var')
    
    # Generate and save response curves
    save_bm_plot <- function(models, file_name, bm_out, data, variables, species) {
      jpeg(paste0(file_name, ".jpg"), width = 800, height = 800, units = "px", res = 200) # Configuración JPG
      biomod2::bm_PlotResponseCurves(
        bm.out = myBiomodModelOut,
        models.chosen = models,
        Data = RC_data,
        show.variables = RC_variables,
        do.bivariate = FALSE,
        fixed.var.metric = 'median',
        legend = TRUE, # Legend
        display_title = TRUE, # Title
        data_species = RC_species
      )
      dev.off() 
    }
    
    
    save_bm_plot(myModelsGLM, "Response_curve_GLM", myBiomodModelOut, data, variables, species)
    save_bm_plot(myModelsGBM, "Response_curve_GBM", myBiomodModelOut, data, variables, species)
    save_bm_plot(myModelsSRE, "Response_curve_SRE", myBiomodModelOut, data, variables, species)
    save_bm_plot(myModelsRF, "Response_curve_RF", myBiomodModelOut, data, variables, species)
    save_bm_plot(myModelsMAXNET, "Response_curve_MAXNET", myBiomodModelOut, data, variables, species)

    png("Response_curve_All.png", width = 800, height = 800, units = "px", res = 100)
    responsecurve_all <- biomod2::bm_PlotResponseCurves(
      bm.out = myBiomodModelOut,
      Data = get_formal_data(myBiomodModelOut, 'expl.var'),
      show.variables = get_formal_data(myBiomodModelOut, 'expl.var.names'),
      do.bivariate = FALSE,
      fixed.var.metric = 'median',
      legend = FALSE,
      display_title = FALSE,
      data_species = get_formal_data(myBiomodModelOut, 'resp.var')
    )
    
    if (!is.null(responsecurve_all)) {
    plot(responsecurve_all) 
    }
    dev.off()
    
}

save.image(paste(actual.path,paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep = "/"))
  
################################################################################
###  19 BIOMOD -  Future estimations
################################################################################ 
# Check the extension of future raster is consistent along selected raster
################################################################################
###  19.1.1 BIOMOD -  Future environment raster 2070 Scenario SSP126
################################################################################  

raster_names <- names(myExpl) # Get the names of rasters from the 'all_raster' stack
future_files <- list.files(path_raster_future_126, pattern = "\\.tif$", full.names = TRUE) #List all raster files in the FUTURE folder
matching_files <- future_files[basename(future_files) %in% paste0(raster_names, ".tif")] #match raster
future_rasters <- stack(matching_files) 
all_raster_fut <- future_rasters

myExpl126 <-all_raster_fut

#plot(myExpl126)
#print(myExpl126)

rm(future_files,matching_files,future_rasters,all_raster_fut)

################################################################################
###  19.1.2 BIOMOD -  Projection 2070 Scenario 126
################################################################################  
setwd(actual.path)
myBiomodProj_EM126<-BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                               proj.name = '2070EM_126',
                                               new.env = myExpl126,
                                               models.chosen = 'all',
                                               metric.binary = 'TSS',
                                               metric.filter = 'NULL',
                                               output.format = '.img',
                                               compress = FALSE,
                                               build.clamping.mask = FALSE,
                                               do.stack = FALSE)

plot(myBiomodProj_EM126)


################################################################################
###  20.1.3 BIOMOD -  Binary 2070 Scenario SSP126
################################################################################  
setwd(paste(actual.path, myRespName, "proj_2070EM_126","individual_projections", sep ="/")) #set directory where individual projection are located
img_EM126_files<-dir(pattern=".img$", full.names = FALSE, ignore.case = TRUE)
#img_EM126_files

Ewmean126_file <- img_EM_files[grep("EMwmeanByTSS_mergedData_mergedRun_mergedAlgo\\.img$", img_EM126_files)]
EwM126_binary<-terra::rast(Ewmean126_file)

BiomodEMwM126_Proj_binary <- bm_BinaryTransformation (data = EwM126_binary,
                                                      threshold = cutoff_EMwM_TSS,
                                                      do.filtering = FALSE)
#BiomodEMwM126_Proj_binary2 <- terra::rast(img_EM126_files[grep("EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img$", img_EM126_files)])

################################################################################
###  20.2.1 BIOMOD -  Future environment raster 2070 Scenario SSP585
################################################################################  

raster_names <- names(myExpl) # Get the names of rasters from the 'all_raster' stack
future_files <- list.files(path_raster_future_585, pattern = "\\.tif$", full.names = TRUE) #List all raster files in the FUTURE folder
matching_files <- future_files[basename(future_files) %in% paste0(raster_names, ".tif")] #match raster
future_rasters <- stack(matching_files) 
all_raster_fut <- future_rasters

myExpl585 <-all_raster_fut

#plot(myExpl585)
#print(myExpl585)

rm(future_files,matching_files,future_rasters,all_raster_fut)

################################################################################
###  20.2.2 BIOMOD -  Projection 2070 Scenario SSP585
################################################################################  
setwd(actual.path)
myBiomodProj_EM585<-BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                               proj.name = '2070EM_585',
                                               new.env = myExpl585,
                                               models.chosen = 'all',
                                               metric.binary = 'TSS',
                                               metric.filter = 'TSS',
                                               output.format = '.img',
                                               compress = FALSE,
                                               build.clamping.mask = FALSE,
                                               do.stack = FALSE)

plot(myBiomodProj_EM585)

################################################################################
###  20.1.3 BIOMOD -  Binary 2070 Scenario SSP585
################################################################################  
setwd(paste(actual.path, myRespName, "proj_2070EM_585","individual_projections", sep ="/")) #set directory where individual projection are located
img_EM585_files<-dir(pattern=".img$", full.names = FALSE, ignore.case = TRUE)
#img_EM585_files

Ewmean585_file <- img_EM_files[grep("EMwmeanByTSS_mergedData_mergedRun_mergedAlgo\\.img$", img_EM585_files)]
EwM585_binary<-terra::rast(Ewmean585_file)

BiomodEMwM585_Proj_binary <- bm_BinaryTransformation (data = EwM585_binary,
                                                     threshold = cutoff_EMwM_TSS,
                                                     do.filtering = FALSE)
#BiomodEMwM585_Proj_binary2 <- terra::rast(img_EM585_files[grep("EMwmeanByTSS_mergedData_mergedRun_mergedAlgo_TSSbin.img$",img_EM585_files)])

save.image(paste(actual.path,paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep = "/"))

################################################################################
###  21 BIOMOD -  Areas comparison done with ArcGIS difference for combined ecosystems
################################################################################

save.image(paste(actual.path,paste(chosen_col_999[T_nr],"_Environment_BIOMOD_results.RData", sep=""), sep = "/"))

################################################################################
###  22 Summary Run
################################################################################

###Summary of run
#Autocorrelation
summary(sac$range_table) #Resumen de sac sin outliers
summary(sb) 
l
L2
median_range.size
range.size
biodata_range.size

#Biomod single algorithms
myBiomodData
myBiomodModelOut
print(summary_stats_var)
print(summary_stats_eval)


#Ensemble Model
treshold
models_used_in_EM
StatsEM

#Mean variable importance Single algorithms
aggregate(var.imp ~ expl.var, data = var.import, FUN = mean)
aggregate(var.imp ~ expl.var, data = var.import, FUN = sd) 

# Variable importance EM
aggregate(var.imp ~ expl.var + algo + filtered.by, data = var.import.EM, FUN = mean) 
aggregate(var.imp ~ expl.var + algo + filtered.by, data = var.import.EM, FUN = sd) 

# Area changes
Scenario2070_SSP126$Compt.By.Models
Scenario2070_SSP585$Compt.By.Models

################################################################################
############### Extra
# setwd(".../RStudio/03_R_results/Specie_analysis_date")
# dir()
# myBiomodOuttmp <- load("species.models.out")
# ### lets store this object with another name
# myBiomodModelOut <- get(myBiomodOuttmp)
# ## check our model is well loaded
# myBiomodModelOut

################################################################################
################################################################################
###  ECOSPAT SECTION
### 
################################################################################
  
#The following data combine terrestrial and marine environment. This following 
# workflow shows the combined version for Ecospat analysis. Analysis were also run 
# and modified accordingly to each terrestrial and marine environment.
# For terrestrial environment consider column selection from 3:6, for 
# marine environment from 3:6
# Also, for marine and terrestrial "[, 7] == 1"
##load packages
library(raster)
library(ecospat)
library(dplyr)
library(tidyverse)
library(ade4)

# Set path
###Read input data
sp1 <- read.delim("C:/SDM_Chionis/Ecospat/DATA_CHIONIS/OCC_C_albus_terrestre.txt") #change this for marine data
sp2 <- read.delim("C:/SDM_Chionis/Ecospat/DATA_CHIONIS/OCC_C_minor_terrestre.txt")

sp1 <- sp1[,1:3]
sp2 <- sp2[,1:3]

data <- rbind(sp1, sp2)

names(data)[names(data) == "name"] <- "species"
names(data)[names(data) == "x"] <- "lon"
names(data)[names(data) == "y"] <- "lat"
data$spp <- data$species

data$species[data$species == "C.albus"] <- "sp1"
data$species[data$species == "C.minor"] <- "sp2" 


# Dataframe for Specie1
df1 <- data %>%
  filter(species == "sp1") %>%
  select(lon, lat)

# Dataframe for Specie2
df2 <- data %>%
  filter(species == "sp2") %>%
  select(lon, lat)


folder_path1 <- "C:/SDM_Chionis/Ecospat/DATA_CHIONIS/BIOS/C_albus/terrestre/PRES"
folder_path2 <- "C:/SDM_Chionis/Ecospat/DATA_CHIONIS/BIOS/C_minor/terrestre/PRES"


##Load raster data
environ1 = stack(list.files(path = folder_path1, pattern='.tif', full.names=TRUE))
environ2 = stack(list.files(path = folder_path2, pattern='.tif', full.names=TRUE))

#Crop by extent
studyArea1 = extent(-74,-33,-67,-26)
predictors1 <- crop(environ1, studyArea1)

#Crop by extent
studyArea2 = extent(33,76,-57,-43)
predictors2 <- crop(environ2, studyArea2)

#plot area
sp1Data = df1
sp2Data = df2

##background points
sp1BG = data.frame(dismo::randomPoints(predictors1[[1]], 1000)); names(sp1BG) = c('lon','lat')
sp2BG = data.frame(dismo::randomPoints(predictors2[[1]], 1000)); names(sp2BG) = c('lon','lat')

##data.frames
sp1DF = data.frame(rbind(sp1Data,sp1BG), pres=c(rep(1,nrow(sp1Data)), rep(0,nrow(sp1BG)) ))
sp2DF = data.frame(rbind(sp2Data,sp2BG), pres=c(rep(1,nrow(sp2Data)), rep(0,nrow(sp2BG)) ))

##data.frame with ambiental variables
sp1DFenv = raster::extract(predictors1, sp1DF[,c('lon','lat')], na.rm=TRUE); sp1DF = data.frame(sp1DF, sp1DFenv)
sp2DFenv = raster::extract(predictors2, sp2DF[,c('lon','lat')], na.rm=TRUE); sp2DF = data.frame(sp2DF, sp2DFenv)

##NA cleaning
sp1DF = sp1DF[complete.cases(sp1DF),]
sp2DF = sp2DF[complete.cases(sp2DF),]

## Niche and PCA analize

##The PCA is calibrated on all the sites of the study area
pca.env <- dudi.pca(rbind(sp1DF,sp2DF)[,c(4:ncol(sp1DF))],scannf=F,nf=2)

##PCA scores for the whole study area
scores.globclim <- pca.env$li

##PCA scores for the species native distribution
scores.sp.sp1 <- suprow(pca.env,sp1DF[which(sp1DF[,'pres']==1),c(4:ncol(sp1DF))])$li

##PCA scores for the species invasive distribution
scores.sp.sp2 <- suprow(pca.env,sp2DF[which(sp2DF[,'pres']==1),c(4:ncol(sp2DF))])$li

##PCA scores for the whole native study area
scores.clim.sp1 <-suprow(pca.env,sp1DF[,c(4:ncol(sp1DF))])$li

##PCA scores for the whole invaded study area
scores.clim.sp2 <- suprow(pca.env,sp2DF[,c(4:ncol(sp2DF))])$li

##gridding the native niche
grid.clim.sp1 <-ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.sp1, sp=scores.sp.sp1, R=100, th.sp=0)

##gridding the invasive niche
grid.clim.sp2 <- ecospat.grid.clim.dyn(glob=scores.globclim, glob1=scores.clim.sp2, sp=scores.sp.sp2, R=100, th.sp=0)

##Niche equivalency
##OBS: Niche equivalency test H1: Is the overlap between the native and invaded niche higher than two random niches
D.overlap <- ecospat.niche.overlap (grid.clim.sp1, grid.clim.sp2, cor=T)$D 
eq.test.equi <- ecospat.niche.equivalency.test(grid.clim.sp1, grid.clim.sp2, rep=100)
eq.test.simi <- ecospat.niche.similarity.test(grid.clim.sp2, grid.clim.sp1, rep=100) #aleatoriza so uma das sp

Dobs_equi= eq.test.equi$obs$D #Observed D index
Iobs_equi = eq.test.equi$obs$I #Observed I index
DpValue_equi = eq.test.equi$p.D #p-value Index D
IpValue_equi = eq.test.equi$p.I #p-value Index I
Dobs_simi= eq.test.simi$obs$D #Observed D index
Iobs_simi = eq.test.simi$obs$I #Observed I index
DpValue_simi = eq.test.simi$p.D #p-value Index D
IpValue_simi = eq.test.simi$p.I #p-value Index I
D.overlap = D.overlap

#### save output as data frame
output <- data.frame(
  sp1 = character(),
  sp2 = character(),
  D.equi = numeric(),
  p_value.equi = numeric(),
  I.equi = numeric(),
  p_valor.equi = numeric(),
  D.simi = numeric(),
  p_value.simi = numeric(),
  I.simi = numeric(),
  p_valor.simi = numeric(),
  D.overlap = numeric(),
  stringsAsFactors = FALSE
)

sp1_name <- "C.albus"
sp2_name <- "C.minor"

#export results as a table 
output <- rbind(output, data.frame(
  sp1 = sp1_name,
  sp2 = sp2_name,
  D.equi = Dobs_equi,
  p_value.equi = DpValue_equi,
  I.equi = Iobs_equi,
  p_valor.equi = IpValue_equi,
  D.simi = Dobs_simi,
  p_value.simi = DpValue_simi,
  I.simi = Iobs_simi,
  p_valor.simi = IpValue_simi,
  D.overlap = D.overlap
))

##save as .csv
write.csv(output, "C:/SDM_Chionis/Ecospat/results/terr7_overlap.csv", row.names=FALSE)

ecospat.plot.overlap.test(eq.test.equi, "D", "Equivalency")
ecospat.plot.overlap.test(eq.test.simi, "D", "Similarity")

ecospat.plot.niche.dyn(grid.clim.sp1, grid.clim.sp2, intersection = 0, title = "", name.axis1 =
                         "Axis 1", name.axis2 = "Axis 2", interest = 1, col.abn
                       = "lightgreen", col.unf = "green", col.exp = "red",
                       col.stab = "blue", col.pio = "pink", col.NA = "grey",
                       colZ1 = "green3", colZ2 = "red3", transparency = 70)



# ecospat.plot.niche.dyn(grid.clim.sp1, grid.clim.sp2, quant = 0.1, interest = 2,
#                        title = "Niche Dynamics", name.axis1 = "PC1", name.axis2 = "PC2")

legend("bottom", 
       legend = c("Unfilling (C.albus)", "Stability", "Expansion (C.minor)"),
       fill = c("green", "blue", "red"),
       bty = "n",
       inset = c(0, -0.2),   # move down
       xpd = TRUE,            # allow legend outside the graphical range
       horiz = TRUE,          # optional: horizontal legend
       cex = 0.6)             # text size
