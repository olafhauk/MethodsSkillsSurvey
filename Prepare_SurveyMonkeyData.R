# setwd('u:/documents/Methods/Methods_Skills_Survey_2015/R')
# source('Prepare_SurveyMonkeyData.R')
# install.packages('readxl')

print("Sunshine")

rm(list = ls()) # clear workspace

library('readxl') # for reading data from Excel file

setwd('U:/documents/Methods/Methods_Skills_Survey_2015/R')

datain_filename <- 'U:/documents/Methods/Methods_Skills_Survey_2015/Sheet_1_converted_161212.xls'

# directory for bar plots etc.
fig_outdir <- "U:/documents/Methods/Methods_Skills_Survey_2015/R/Figures/"
print( sprintf("Writing figures to %s", fig_outdir))

source('Survey_functions.R') # function that gets Results (counts, fractions) from data frame data

graphics.off() # close all open plot windows

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

###
######### Analysis ###########
###

# PRUNE respondents with insufficient responses etc.
data_all <- prune_respondents(data_all, qnames)

N <- nrow( data_all ) # number of respondents

print( sprintf("%d good respondents", N) )

### Demographics ###

# get indices for various SUB-GROUPS of respondents
groups_all <- get_groups(data_all)

n_males <- length( groups_all[["males"]] )
n_females <- length( groups_all[["females"]] )

# Age
mean_age <- mean( as.numeric( data_all$Age ), na.rm=T )
sd_age <- sd( as.numeric( data_all$Age ), na.rm=T )
mean_age_males <- mean( as.numeric( data_all$Age[ groups_all[["males"]] ]), na.rm=T )
mean_age_females <- mean( as.numeric( data_all$Age[ groups_all[["females"]] ]), na.rm=T )
sd_age_males <- sd( as.numeric( data_all$Age[ groups_all[["males"]] ]), na.rm=T )
sd_age_females <- sd( as.numeric( data_all$Age[ groups_all[["females"]] ]), na.rm=T )
print( sprintf("Age: %.1f (%.0f), males: %.1f (%.0f), females: %.1f (%.0f)", mean_age, sd_age,
                    mean_age_males, sd_age_males, mean_age_females, sd_age_females) )

n_int_males <- length(which(groups_all[["internet_yes"]] & groups_all[["males"]]))
n_int_females <- length(which(groups_all[["internet_yes"]] & groups_all[["females"]]))
n_int <- n_int_males + n_int_females
print( sprintf("%d of respondents used the internet (males: %.2f, females: %.2f)", n_int, n_int_males/n_males,
                                                                            n_int_females/n_females) )

### Questions ###
Results <- list() # for different respondent groups
# Results will be of form:
# Results[["sex"]][["males"]]$frac[[qq]] will produce four numbers Cor/Err/NoI/Skp

print("Get results.")

print ("Gender")
Results[["sex"]][["All"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["All"]])
Results[["sex"]][["males"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["males"]])
Results[["sex"]][["females"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["females"]])

print ("Degree")
Results[["degree"]][["ugrad_group_psych"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych"]])
Results[["degree"]][["ugrad_group_meth"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth"]])
Results[["degree"]][["ugrad_group_biol"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol"]])

print ("Degree by Gender")
Results[["deg_sex"]][["ugrad_group_psych_males"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych_males"]])
Results[["deg_sex"]][["ugrad_group_meth_males"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth_males"]])
Results[["deg_sex"]][["ugrad_group_biol_males"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol_males"]])
Results[["deg_sex"]][["ugrad_group_psych_females"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych_females"]])
Results[["deg_sex"]][["ugrad_group_meth_females"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth_females"]])
Results[["deg_sex"]][["ugrad_group_biol_females"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol_females"]])

print ("Expertise")
Results[["Expertise"]][["expert_yes"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["expert_yes"]])
Results[["Expertise"]][["expert_sortof"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["expert_sortof"]])
Results[["Expertise"]][["expert_no"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["expert_no"]])
# not enough trials for "dont know"

print ("Expertise by Gender")
Results[["ExpGend"]][["expert_yes_males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["expert_yes_males"]])
Results[["ExpGend"]][["expert_sortof_males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["expert_sortof_males"]])
Results[["ExpGend"]][["expert_no_males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["expert_no_males"]])
Results[["ExpGend"]][["expert_yes_females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["expert_yes_females"]])
Results[["ExpGend"]][["expert_sortof_females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["expert_sortof_females"]])
Results[["ExpGend"]][["expert_no_females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["expert_no_females"]])

# Training needs
Results[["Training needs"]][["All"]] <- get_training_needs(data_all, groups_all[["All"]])
Results[["Training needs"]][["males"]] <- get_training_needs(data_all, groups_all[["males"]])
Results[["Training needs"]][["females"]] <- get_training_needs(data_all, groups_all[["females"]])

# Researcher type
Results[["Research area"]][["All"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["All"]])
Results[["Research area"]][["males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["males"]])
Results[["Research area"]][["females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["females"]])

Results[["Research area"]][["Master's All"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["researcher_master"]])
Results[["Research area"]][["Master's males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["researcher_master_males"]])
Results[["Research area"]][["Master's females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["researcher_master_females"]])
Results[["Research area"]][["PhD All"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["researcher_phd"]])
Results[["Research area"]][["PhD males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["researcher_phd_males"]])
Results[["Research area"]][["PhD females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["researcher_phd_females"]])
Results[["Research area"]][["Postdoc All"]] <- get_Results(data_all, q_meth_names, correct, groups_all[["researcher_postdoc"]])
Results[["Research area"]][["Postdoc males"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["researcher_postdoc_males"]])
Results[["Research area"]][["Postdoc females"]] <- get_Results(data_all, q_meth_names, correct,
                                                                groups_all[["researcher_postdoc_females"]])

# Future area counts
Results[["Research area cnts"]][["All"]] <- get_research_area(data_all, groups_all[["All"]])
Results[["Research area cnts"]][["males"]] <- get_research_area(data_all, groups_all[["males"]])
Results[["Research area cnts"]][["females"]] <- get_research_area(data_all, groups_all[["females"]])

# Future area counts
Results[["Future area cnts"]][["All"]] <- get_future_area(data_all, groups_all[["All"]])
Results[["Future area cnts"]][["males"]] <- get_future_area(data_all, groups_all[["males"]])
Results[["Future area cnts"]][["females"]] <- get_future_area(data_all, groups_all[["females"]])


# SUMMARY PLOTS across ALL QUESTIONS
# restype <- c("sum_counts", "sum_frac") # summary across all questions
restype <- c("sum_frac")

pdf_name <- sprintf("%sSummaries_General.pdf", fig_outdir) # avoid some problems with long filenames
pdf(pdf_name, onefile=TRUE)
print ( sprintf("Plotting to PDF: %s", pdf_name) )

# All
print ("All")
groups <- c("All")
bar_legend <- c("All")
plot_bargraphs(Results[["sex"]], groups, "sum", restype, "All", bar_legend)

# gender
print ("Gender")
groups <- c("males", "females")
bar_legend <- c("males", "females")
plot_bargraphs(Results[["sex"]], groups, "sum", restype, "Gender", bar_legend)

# degree
print ("Degree")
groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
bar_legend <- c("Undergrads Psychology", "Undergrads Methods", "Undergrads Biology")
plot_bargraphs(Results[["degree"]], groups, "sum", restype, "Undergraduate Degree", bar_legend)

# degree-by-sex
print ("Degree by Gender")
groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
bar_legend <- c("Undergrads Psychology M", "Undergrads Psychology F", "Undergrads Methods M", "Undergrads Methods F", "Undergrads Biology M", "Undergrads Biology F")
plot_bargraphs(Results[["deg_sex"]], groups, "sum", restype, "Degree-by-Gender", bar_legend)

# expertise
print ("Expertise")
groups <- c("expert_yes", "expert_sortof", "expert_no")
bar_legend <- c("Expert", "Sort of Expert", "No Expert")
plot_bargraphs(Results[["Expertise"]], groups, "sum", restype, "Expertise", bar_legend)

# gender-by-expertise
print ("Expertise by Gender")
groups <- c("expert_yes_males", "expert_yes_females", "expert_sortof_males", "expert_sortof_females", "expert_no_males", 
            "expert_no_females")
bar_legend <- c("Expert Males", "Expert Females", "Sort of Expert Males", "Sort of Expert Females", "No Expert Males",
                "No Expert Females")
plot_bargraphs(Results[["ExpGend"]], groups, "sum", restype, "Expertise-by-Gender", bar_legend)

# Researcher type results
print ("Researcher type")
groups <- c("Master's All", "Master's males", "Master's females", "PhD All", "PhD males", "PhD females",
                "Postdoc All", "Postdoc males", "Postdoc females")
bar_legend <- c("Master's All", "Master's males", "Master's females", "PhD All", "PhD males", "PhD females",
                "Postdoc All", "Postdoc males", "Postdoc females")
plot_bargraphs(Results[["Research area"]], groups, "sum", restype, "Researcher type", bar_legend)

# Training needs
print ("Training needs")
groups <- c("All", "males", "females")
bar_legend <- c("All", "males", "females")
bar_names <- c("A lot", "Significantly", "A little", "Not at all", "Don't know", "Skipped")
plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend)

# Researcher type counts
print ("Researcher type cnts")
groups <- c("All", "males", "females")
bar_legend <- c("All", "males", "females")
bar_names <- c("Undergraduate", "PhD", "Post-Doc", "Master's", "Res'ch Ass't", "Skipped")
plot_general_questions(Results[["Research area cnts"]], groups, "Researcher type", bar_names, bar_legend)

# Future area
print ("Future area cnts")
groups <- c("All", "males", "females")
bar_legend <- c("All", "males", "females")
bar_names <- c("Psych", "Cog Sci", "Cog N'sci", "Cli N'sci", "DK", "Other", "Skipped")
plot_general_questions(Results[["Future area cnts"]], groups, "Future area", bar_names, bar_legend)

dev.off()

# SUMMARY PLOTS across SUB-GROUPS OF QUESTIONS
pdf_name <- sprintf("%sSummaries_QuestionGroups.pdf", fig_outdir) # avoid some problems with long filenames
pdf(pdf_name, onefile=TRUE)
print ( sprintf("Plotting to PDF: %s", pdf_name) )

# restype <- c("sum_counts", "sum_frac") # summary across all questions
restype <- c("sum_qgroup_frac")

qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal Analysis",
                phys = "Physics", stats = "Statistics")

for (qq in names(qgroup_names))
{
    # gender
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    title <- sprintf("Gender | %s", qgroup_names[qq])
    plot_bargraphs(Results[["sex"]], groups, qq, restype, title, bar_legend)

    # degree
    groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
    bar_legend <- c("Undergrads Psychology", "Undergrads Methods", "Undergrads Biology")
    title <- sprintf("Undergraduate Degree | %s", qgroup_names[qq])
    plot_bargraphs(Results[["degree"]], groups, qq, restype, title, bar_legend)

    # degree-by-sex
    groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
    bar_legend <- c("Undergrads Psychology M", "Undergrads Psychology F", "Undergrads Methods M", "Undergrads Methods F", "Undergrads Biology M", "Undergrads Biology F")
    title <- sprintf("Degree-by-Gender | %s", qgroup_names[qq])
    plot_bargraphs(Results[["deg_sex"]], groups, qq, restype, title, bar_legend)

    # expertise
    groups <- c("expert_yes", "expert_sortof", "expert_no")
    bar_legend <- c("Expert", "Sort of Expert", "No Export")
    title <- sprintf("Expertise | %s", qgroup_names[qq])
    plot_bargraphs(Results[["Expertise"]], groups, qq, restype, title, bar_legend)
}
dev.off()


# types of results to be plotted
# restype <- c("counts", "frac")
restype <- c("frac")

# plot into one PDF
pdf_name <- sprintf("%sAllplots.pdf", fig_outdir) # avoid some problems with long filenames
pdf(pdf_name, onefile=TRUE)
print ( sprintf("Plotting to PDF: %s", pdf_name) )

for (qq in q_meth_names)
{
    filestem <- "all"
    groups <- c("males", "females")
    bar_legend <- c("Males", "Females")
    plot_bargraphs(Results[["sex"]], groups, qq, restype, qq, bar_legend)

    filestem <- "degree"
    groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
    bar_legend <- c("Undergrads Psychology", "Undergrads Methods", "Undergrads Biology")
    plot_bargraphs(Results[["degree"]], groups, qq, restype, qq, bar_legend)

    filestem <- "deg_sex"
    groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
    bar_legend <- c("Undergrads Psychology M", "Undergrads Psychology F", "Undergrads Methods M", "Undergrads Methods F", "Undergrads Biology M", "Undergrads Biology F")
    plot_bargraphs(Results[["deg_sex"]], groups, qq, restype, qq, bar_legend)

    filestem <- "expertise"
    groups <- c("expert_yes", "expert_sortof", "expert_no")
    bar_legend <- c("Expert", "Sort of Expert", "No Export")
    plot_bargraphs(Results[["Expertise"]], groups, qq, restype, qq, bar_legend)

}

# closes all plot windows:
dev.off()
graphics.off()

### STATISTICS
stat_list <- list() # collect results from logistic regression

# response categories
categs <- c("Cor", "Err", "NoI")

## GENDER stats
cat("\n\n")
print("######################")
print("GENDER.")
cat("\n")

label <- "gender" # used as index in results

# INDEPENDENT VARIABLE (male/female)
iv <- factor( 1*groups_all[["males"]] )

for (qq in q_meth_names)
{
    print( qq )

    for (cc in categs)
    {
        dv <- get_dep_var(Results[["sex"]][["All"]], qq, cc)
        stat_list[[qq]][[label]][[cc]] <- logistic_regression(dv, iv)
    }
}

# for performance across all questions
print("###")
print("Across all questions - gender.")
for (cc in categs)
{
    dv <- array( Results[["sex"]][["All"]][["indiv"]][["AllQs"]][[cc]] )

    stat_list[[qq]][[label]][[cc]] <- logistic_regression(dv, iv, "quasibinomial")
}


## UNDERGRADUATE DEGREE stats
cat("\n\n")
print("######################")
print("UNDERGRADUATE DEGREE.")
cat("\n")

label <- "undgrad" # used as index in results

tmp <- matrix(0,nrow(data_all),1)
tmp[which(groups_all[["ugrad_group_psych"]])] = 1
tmp[which(groups_all[["ugrad_group_biol"]])] = 2
tmp[which(groups_all[["ugrad_group_meth"]])] = 3

# INDEPENDENT VARIABLE (undergrad degree)
iv <- factor( tmp )

for (qq in q_meth_names)
{
    print( qq )

    for (cc in categs)
    {
        dv <- get_dep_var(Results[["sex"]][["All"]], qq, cc)
        stat_list[[qq]][[label]][[cc]] <- logistic_regression(dv, iv)
    }
}


# for performance across all questions
print("###")
print("Across all questions - undergrad degree.")
for (cc in categs)
{
    dv <- array( Results[["sex"]][["All"]][["indiv"]][["AllQs"]][[cc]] )

    stat_list[[qq]][[label]][[cc]] <- logistic_regression(dv, iv, "quasibinomial")
}