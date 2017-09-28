# source('Prepare_SurveyMonkeyData.R')

print("Sunshine")

rm(list = ls()) # clear workspace

library(MASS)

library('readxl') # for reading data from Excel file
# library(nnet) # for multinomial regression (multinom)
library(VGAM) # for multinomial regression (vglm)

setwd('U:/Methods/Methods_Skills_Survey_2015/R')

datain_filename <- 'U:/Methods/Methods_Skills_Survey_2015/Sheet_1_converted_161212.xls'

# directory for bar plots etc.
fig_outdir <- "U:/Methods/Methods_Skills_Survey_2015/R/Figures/"
print( sprintf("Writing figures to %s", fig_outdir))

source('Survey_functions.R') # function that gets Results (counts, fractions) from data frame data

graphics.off() # close all open plot windows

#######################
# Read and prepare data
#######################

# READ EXCEL spreadsheet (prepared with Convert_SurveyMonkeyData_4R.m)
# first row contains column names
# data_ori <- read_excel('U:/documents/Methods/Methods_Skills_Survey_2015/Sheet_1_converted_151123.xls', col_names=T)
print( sprintf("Reading data from %s", datain_filename))
data_ori <- read_excel(datain_filename, col_names=T)

names(data_ori)[3] <- "Age" # remove the silly "?"
names(data_ori)[4] <- "Nationality"

# get row with correct responses
correct <- data_ori[1,]

q_yesno <- c(13:32) # indices for "real" knowledge questions
q_names <- names( data_ori )[q_yesno] # names of the real knowledge questions

q_meth_yesno <- c(15:32) # indices for methods-related questions
q_meth_names <- names( data_ori )[q_meth_yesno] # names of the methods-related questions

# remove row with correct responses from data frame
# note: row numbering now starts at 2
data_all <- data_ori[2:nrow(data_ori),]

# PRUNE respondents with insufficient responses etc.
print("Prune data.")
data_all <- prune_respondents(data_all, qnames)

# get indices for various SUB-GROUPS of respondents
print("Get group indices.")
groups_all <- get_groups(data_all)
# groups_all$needs_alot contains Boolean yes/no

###
######### Analysis ###########
###

print("Computing demographics.")
demos <- demographics(groups_all, data_all)

print("Get results list.")
Results <- get_all_Results(data_all, q_meth_names, correct, groups_all)
# Results$sex$males$frac$Ohm contains values for Cor/Err/NoI/Skp

print(sprintf("Plot results to %s.", fig_outdir))
plot_Results(Results, fig_outdir)

print("Computing logistic regression.")
stat_list <- get_Stats(Results, groups_all)
# stat_list$Derivative$Err contains stats output from glm/polr

# Done