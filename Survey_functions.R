prune_respondents <- function(data_all, qnames)
# remove respondents depending on skipped responses
# data: dataframe from survey data
# qnames: names of knowledge questions
# Returns: pruned data

{
    
    skip_thresh <- 0 # exclude when more knowledge questions skipped

    N <- nrow( data_all ) # number of overall respondents

    # filter for consent
    consent_filt <- (data_all$Consent == 1)

    # filter for gender
    gender_filt <- data_all$Gender %in% c(1,2)

    # filer for undergraduate degree
    ugrad_filt <- !is.na( data_all$"Undergrad degree" )

    # filter for skipped questions
    is_na <- rep(0,N) # initialise with zeros
    for (qq in q_names)
    {
        is_na <- is_na + is.na(data_all[[qq]]) # find missing values for this question
    }
    is_na_filt <- (is_na<=skip_thresh) # find respondents who skipped too much

    all_filt <- consent_filt & gender_filt & ugrad_filt & is_na_filt # combine all filters

    data <- data_all[which(all_filt),] # reduce data frame according to filters

    n_consent <- length(which(consent_filt))
    n_gender <- length(which(gender_filt))
    n_ugrad <- length(which(ugrad_filt))
    n_na <- length(which(is_na_filt))
    n_all <- length(which(all_filt))
    n_skip_all <- length(which(is_na==20))

    # # it appears those who didn't respond to methods questions also didn't respond to gender question
    # n_skip_males <- length(which((is_na==20) & (data_all$Gender==1)))
    # n_skip_females <- length(which((is_na==20) & (data_all$Gender==2)))

    print( sprintf("Consented: %d - Gender responses: %d - Ugrad responses: %d - Enough responses: %d",
                                                                    n_consent, n_gender, n_ugrad, n_na) )
    print( sprintf("Remaining respondents: %d", n_all) )

    print( sprintf("Number of respondents who skipped all knowledge questions: %d.", n_skip_all))

    # some awkward entries
    if (data$Age[[34]] == "40br")
    {
        data$Age[[34]] = "40.000000" # I assume typo
    }

    return( data )
} # prune_respondents()


get_groups <- function(data)
# create indices for subgroups of respondents (males/females etc.)
# data: data frame of survey data
# returns groups: list, with strings as indices to subgroups
{
    groups <- list()

    n_pp <- length(data[,1]) # number of participants here

    ### All
    groups[["All"]] <- Vectorize(isTRUE)(c(1:n_pp)>0) # all TRUE

    ### GENDER
    groups[["males"]] <- Vectorize(isTRUE)(data$Gender == 1)
    groups[["females"]] <- Vectorize(isTRUE)(data$Gender == 2)

    ### EXPERTISE
    groups[["expert_yes"]] <- Vectorize(isTRUE)(data$Expertise == 1)
    groups[["expert_sortof"]] <- Vectorize(isTRUE)(data$Expertise == 2)
    groups[["expert_no"]] <- Vectorize(isTRUE)(data$Expertise == 3)
    groups[["expert_dk"]] <- Vectorize(isTRUE)(data$Expertise == 4)

    ### INTERNET USE
    groups[["internet_yes"]] <- Vectorize(isTRUE)(data$Internet == 1)
    groups[["internet_no"]] <- Vectorize(isTRUE)(data$Internet == 2)

    ### UNDERGRADUATE DEGREE
    ugrad <- data$"Undergrad degree"

    groups[["ugrad_psych"]] <- Vectorize(isTRUE)(ugrad == "1.000000") # numbers are strings here
    groups[["ugrad_cogsci"]] <- Vectorize(isTRUE)(ugrad == "2.000000") # Vectorize turns NAs to FALSE
    groups[["ugrad_cogneu"]] <- Vectorize(isTRUE)(ugrad == "3.000000")
    groups[["ugrad_phys"]] <- Vectorize(isTRUE)(ugrad == "4.000000")
    groups[["ugrad_math"]] <- Vectorize(isTRUE)(ugrad == "5.000000")
    groups[["ugrad_comp"]] <- Vectorize(isTRUE)(ugrad == "6.000000")
    groups[["ugrad_biomed"]] <- Vectorize(isTRUE)(ugrad == "6.000000")
    groups[["ugrad_biol"]] <- Vectorize(isTRUE)(ugrad == "8.000000")
    groups[["ugrad_med"]] <- Vectorize(isTRUE)(ugrad == "9.000000")
    groups[["ugrad_other"]] <- Vectorize(isTRUE)(substr( ugrad, 2, 8) != ".000000") # not a number string

    # BROADER GROUPS
    groups[["ugrad_group_psych"]] <- groups[["ugrad_psych"]] | groups[["ugrad_cogsci"]] | groups[["ugrad_cogneu"]]
    groups[["ugrad_group_meth"]] <- groups[["ugrad_phys"]] | groups[["ugrad_math"]] | groups[["ugrad_comp"]] | groups[["ugrad_biomed"]]
    groups[["ugrad_group_biol"]] <- groups[["ugrad_med"]] | groups[["ugrad_biol"]]

    # for broader groups, include some respondents who provided string as input
    for (uu in 1:length(ugrad))
    {        
        if ( any(grep('linguist', tolower(ugrad[uu])) == 1) | any(grep('psychology', tolower(ugrad[uu])) == 1) | any(grep('therapy', tolower(ugrad[uu])) == 1) )        
        {
            groups[["ugrad_group_psych"]][uu] <- TRUE
        }

        if ( any(grep('engineer', tolower(ugrad[uu])) == 1) | any(grep('statistic', tolower(ugrad[uu])) == 1) | any(grep('artifical intelligence', tolower(ugrad[uu])) == 1) | any(grep('chemistry', tolower(ugrad[uu])) == 1) | any(grep('electronic', tolower(ugrad[uu])) == 1) )        
        {
            groups[["ugrad_group_meth"]][uu] <- TRUE
        }

        if ( any(grep('neuroscience', tolower(ugrad[uu])) == 1) | any(grep('physiology', tolower(ugrad[uu])) == 1) | any(grep('medicine', tolower(ugrad[uu])) == 1) )        
        {
            groups[["ugrad_group_biol"]][uu] <- TRUE
        }
    }
     
    ## Researcher type (undergrad, PhD etc.)
    res_type <- data[["Research area"]]
    len <- length(res_type)

    ### get data into vector for bar plot
    res_vec <- matrix(0,6,1) # vector: number of response options

    # Take care of "Other" option (master's, RAs)
    for (uu in 1:len)
    {
        groups[["researcher_undgrad"]][uu] <- FALSE
        groups[["researcher_phd"]][uu] <- FALSE
        groups[["researcher_postdoc"]][uu] <- FALSE
        groups[["researcher_resass"]][uu] <- FALSE
        groups[["researcher_skipped"]][uu] <- FALSE
        # skipped?
        if ( is.na(res_type[[uu]]) )
        {
            groups[["researcher_skipped"]][uu] <- TRUE
        }
        else
        {
            # undergrads, incl. master, mphil etc.
            if ((res_type[[uu]]=="1.000000") | ( any(grep('master', tolower(res_type[[uu]])) == 1) |
                                any(grep('Msc', tolower(res_type[[uu]])) == 1) |
                                any(grep('MPhil', tolower(res_type[[uu]])) == 1) ))
            {
                groups[["researcher_undgrad"]][uu] <- TRUE
            } 
            else if ( res_type[[uu]]=="2.000000" | any(grep('phd', tolower(res_type[[uu]])) == 1) )
            {
                groups[["researcher_phd"]][uu] <- TRUE
            }
            else if (res_type[[uu]]=="3.000000")
            {
                groups[["researcher_postdoc"]][uu] <- TRUE
            }            
            else if ( any(grep('research assistant', tolower(res_type[[uu]])) == 1) )
            {
                groups[["researcher_resass"]][uu] <- TRUE
            }
        }
    }

    ## gender by researcher type
    groups[["researcher_undgrad_males"]] = groups[["researcher_undgrad"]] & groups[["males"]]
    groups[["researcher_undgrad_females"]] = groups[["researcher_undgrad"]] & groups[["females"]]
    groups[["researcher_phd_males"]] = groups[["researcher_phd"]] & groups[["males"]]
    groups[["researcher_phd_females"]] = groups[["researcher_phd"]] & groups[["females"]]
    groups[["researcher_postdoc_males"]] = groups[["researcher_postdoc"]] & groups[["males"]]
    groups[["researcher_postdoc_females"]] = groups[["researcher_postdoc"]] & groups[["females"]]    
    groups[["researcher_resass_males"]] = groups[["researcher_resass"]] & groups[["males"]]
    groups[["researcher_resass_females"]] = groups[["researcher_resass"]] & groups[["females"]]

    ## Training needs
    groups[["needs_alot"]] <- Vectorize(isTRUE)(data$"Training needs" == 1)
    groups[["needs_sign"]] <- Vectorize(isTRUE)(data$"Training needs" == 2)
    groups[["needs_alit"]] <- Vectorize(isTRUE)(data$"Training needs" == 3)
    groups[["needs_notaa"]] <- Vectorize(isTRUE)(data$"Training needs" == 4)
    groups[["needs_dk"]] <- Vectorize(isTRUE)(data$"Training needs" == 5)
    groups[["needs_skpd"]] <- Vectorize(isTRUE)(is.na(data$"Training needs"))

    # Gender by undergraduate degree
    groups[["ugrad_group_psych_males"]] <- groups[["ugrad_group_psych"]] & groups[["males"]]
    groups[["ugrad_group_psych_females"]] <- groups[["ugrad_group_psych"]] & groups[["females"]]
    groups[["ugrad_group_meth_males"]] <- groups[["ugrad_group_meth"]] & groups[["males"]]
    groups[["ugrad_group_meth_females"]] <- groups[["ugrad_group_meth"]] & groups[["females"]]
    groups[["ugrad_group_biol_males"]] <- groups[["ugrad_group_biol"]] & groups[["males"]]
    groups[["ugrad_group_biol_females"]] <- groups[["ugrad_group_biol"]] & groups[["females"]]

    # Expertise by Gender
    groups[["expert_yes_males"]] <- groups[["expert_yes"]] & groups[["males"]]
    groups[["expert_yes_females"]] <- groups[["expert_yes"]] & groups[["females"]]
    groups[["expert_sortof_males"]] <- groups[["expert_sortof"]] & groups[["males"]]
    groups[["expert_sortof_females"]] <- groups[["expert_sortof"]] & groups[["females"]]
    groups[["expert_no_males"]] <- groups[["expert_no"]] & groups[["males"]]
    groups[["expert_no_females"]] <- groups[["expert_no"]] & groups[["females"]]
    groups[["expert_dk_males"]] <- groups[["expert_dk"]] & groups[["males"]]
    groups[["expert_dk_females"]] <- groups[["expert_dk"]] & groups[["females"]]

    # group counts
    for (name in names(groups))
    {
        print( sprintf("%s: %d", name, length( which(groups[[name]]) ) ) )
    }

    return( groups )
} # get_groups()


get_demographics <- function(groups, data_all)
{
    # compute basic demographics, e.g. age
    # groups_all:  list, with strings as indices to subgroups, from get_groups()
    # data_all: dataframe with survey data, from prune_respondents()
    # returns: list demos with demographic values (age etc.)

    demos <- list()
    
    # General
    demos['N_all'] <- nrow( data_all ) # number of respondents
    demos['N_males'] <- length(which(groups[["males"]]))
    demos['N_females'] <- length(which(groups[["females"]]))
    demos['n_int_males'] <- length(which(groups[["internet_yes"]] & groups[["males"]]))
    demos['n_int_females'] <- length(which(groups[["internet_yes"]] & groups[["females"]]))
    demos['n_int'] <- demos[['n_int_males']] + demos[['n_int_females']]

    # Current degree/programme
    demos['n_ugrad_males'] <- length(which(groups[["researcher_undgrad_males"]]))
    demos['n_ugrad_females'] <- length(which(groups[["researcher_undgrad_females"]]))
    demos['n_phd_males'] <- length(which(groups[["researcher_phd_males"]]))
    demos['n_phd_females'] <- length(which(groups[["researcher_phd_females"]]))
    demos['n_pd_males'] <- length(which(groups[["researcher_postdoc_males"]]))
    demos['n_pd_females'] <- length(which(groups[["researcher_postdoc_females"]]))    
    demos['n_ra_males'] <- length(which(groups[["researcher_resass_males"]]))
    demos['n_ra_females'] <- length(which(groups[["researcher_resass_females"]]))

    # undergraduate degree
    demos['n_psych_males'] <- length(which(groups[["ugrad_group_psych_males"]]))
    demos['n_psych_females'] <- length(which(groups[["ugrad_group_psych_females"]]))
    demos['n_meth_males'] <- length(which(groups[["ugrad_group_meth_males"]]))
    demos['n_meth_females'] <- length(which(groups[["ugrad_group_meth_females"]]))
    demos['n_biol_males'] <- length(which(groups[["ugrad_group_biol_males"]]))
    demos['n_biol_females'] <- length(which(groups[["ugrad_group_biol_females"]]))

    # Age
    demos['mean_age'] <- mean( as.numeric( data_all$Age ), na.rm=T )
    demos['mean_age_males'] <- mean( as.numeric( data_all$Age[ groups[["males"]] ]), na.rm=T )
    demos['mean_age_females'] <- mean( as.numeric( data_all$Age[ groups[["females"]] ]), na.rm=T )
    demos['sd_age'] <- sd( as.numeric( data_all$Age ), na.rm=T )
    demos['sd_age_males'] <- sd( as.numeric( data_all$Age[ groups[["males"]] ]), na.rm=T )
    demos['sd_age_females'] <- sd( as.numeric( data_all$Age[ groups[["females"]] ]), na.rm=T )
    demos['age_psych_males'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_psych_males"]] ]), na.rm=T )
    demos['age_psych_females'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_psych_females"]] ]), na.rm=T )
    demos['age_meth_males'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_meth_males"]] ]), na.rm=T )
    demos['age_meth_females'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_meth_females"]] ]), na.rm=T )
    demos['age_biol_males'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_biol_males"]] ]), na.rm=T )
    demos['age_biol_females'] <- mean( as.numeric( data_all$Age[ groups[["ugrad_group_biol_females"]] ]), na.rm=T )

    print( sprintf('Age: %.1f (%.1f)', demos[['mean_age']], demos[['sd_age']]))
    print( sprintf('Males: %.1f (%.1f)', demos[['mean_age_males']], demos[['sd_age_males']]))
    print( sprintf('Females: %.1f (%.1f)', demos[['mean_age_females']], demos[['sd_age_females']]))

    return( demos )
} # demographics()


get_all_Results <- function(data_all, q_meth_names, correct, groups_all)
{
    # create list with all results for groups and questions
    # data_all: data frame, columns: questions, rows: respondents
    # q_meth_names: names of questions used to refer to columns in data
    # correct: data frame with correct response codes, same column names as data_all
    # groups_all: list with indices of respondents (rows of data) for different groups
    # Returns: list with results, entries are output from get_indiv_Results()

    Results <- list() # for different respondent groups

    print ("Gender")
    Results[["sex"]][["All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["All"]])
    Results[["sex"]][["males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["males"]])
    Results[["sex"]][["females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["females"]])

    print ("Degree")
    Results[["degree"]][["ugrad_group_psych"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych"]])
    Results[["degree"]][["ugrad_group_meth"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth"]])
    Results[["degree"]][["ugrad_group_biol"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol"]])

    print ("Degree by Gender")
    Results[["deg_sex"]][["ugrad_group_psych_males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych_males"]])
    Results[["deg_sex"]][["ugrad_group_meth_males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth_males"]])
    Results[["deg_sex"]][["ugrad_group_biol_males"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol_males"]])
    Results[["deg_sex"]][["ugrad_group_psych_females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_psych_females"]])
    Results[["deg_sex"]][["ugrad_group_meth_females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_meth_females"]])
    Results[["deg_sex"]][["ugrad_group_biol_females"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["ugrad_group_biol_females"]])

    print ("Expertise")
    Results[["Expertise"]][["expert_yes"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["expert_yes"]])
    Results[["Expertise"]][["expert_sortof"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["expert_sortof"]])
    Results[["Expertise"]][["expert_no"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["expert_no"]])
    # not enough trials for "dont know"

    print ("Expertise by Gender")
    Results[["ExpGend"]][["expert_yes_males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_males"]])
    Results[["ExpGend"]][["expert_sortof_males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_males"]])
    Results[["ExpGend"]][["expert_no_males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_males"]])
    Results[["ExpGend"]][["expert_yes_females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_yes_females"]])
    Results[["ExpGend"]][["expert_sortof_females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_sortof_females"]])
    Results[["ExpGend"]][["expert_no_females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["expert_no_females"]])

    # Training needs
    Results[["Training needs"]][["All"]] <- get_training_needs(data_all, groups_all[["All"]])
    Results[["Training needs"]][["males"]] <- get_training_needs(data_all, groups_all[["males"]])
    Results[["Training needs"]][["females"]] <- get_training_needs(data_all, groups_all[["females"]])
    Results[["Training needs"]][["Undgrad All"]] <- get_training_needs(data_all, groups_all[["researcher_undgrad"]])
    Results[["Training needs"]][["Undgrad males"]] <- get_training_needs(data_all, groups_all[["researcher_undgrad_males"]])
    Results[["Training needs"]][["Undgrad females"]] <- get_training_needs(data_all, groups_all[["researcher_undgrad_females"]])
    Results[["Training needs"]][["PhD All"]] <- get_training_needs(data_all, groups_all[["researcher_phd"]])
    Results[["Training needs"]][["PhD males"]] <- get_training_needs(data_all, groups_all[["researcher_phd_males"]])
    Results[["Training needs"]][["PhD females"]] <- get_training_needs(data_all, groups_all[["researcher_phd_females"]])
    Results[["Training needs"]][["Postdoc All"]] <- get_training_needs(data_all, groups_all[["researcher_postdoc"]])
    Results[["Training needs"]][["Postdoc males"]] <- get_training_needs(data_all, groups_all[["researcher_postdoc_males"]])
    Results[["Training needs"]][["Postdoc females"]] <- get_training_needs(data_all, groups_all[["researcher_postdoc_females"]])
    Results[["Training needs"]][["ugrad_group_meth"]] <- get_training_needs(data_all, groups_all[["ugrad_group_meth"]])
    Results[["Training needs"]][["ugrad_group_meth_males"]] <- get_training_needs(data_all, groups_all[["ugrad_group_meth_males"]])
    Results[["Training needs"]][["ugrad_group_meth_females"]] <- get_training_needs(data_all, groups_all[["ugrad_group_meth_females"]])
    Results[["Training needs"]][["ugrad_group_psych"]] <- get_training_needs(data_all, groups_all[["ugrad_grouppsych"]])
    Results[["Training needs"]][["ugrad_group_psych_males"]] <- get_training_needs(data_all, groups_all[["ugrad_group_psych_males"]])
    Results[["Training needs"]][["ugrad_group_psych_females"]] <- get_training_needs(data_all, groups_all[["ugrad_group_psych_females"]])
    Results[["Training needs"]][["ugrad_group_biol"]] <- get_training_needs(data_all, groups_all[["ugrad_group_biol"]])
    Results[["Training needs"]][["ugrad_group_biol_males"]] <- get_training_needs(data_all, groups_all[["ugrad_group_biol_males"]])
    Results[["Training needs"]][["ugrad_group_biol_females"]] <- get_training_needs(data_all, groups_all[["ugrad_group_biol_females"]])
    

    # Researcher type
    Results[["Research area"]][["All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["All"]])
    Results[["Research area"]][["males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["males"]])
    Results[["Research area"]][["females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["females"]])

    Results[["Research area"]][["Undgrad All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["researcher_undgrad"]])
    Results[["Research area"]][["Undgrad males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_undgrad_males"]])
    Results[["Research area"]][["Undgrad females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_undgrad_females"]])
    Results[["Research area"]][["PhD All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["researcher_phd"]])
    Results[["Research area"]][["PhD males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_phd_males"]])
    Results[["Research area"]][["PhD females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_phd_females"]])
    Results[["Research area"]][["Postdoc All"]] <- get_indiv_Results(data_all, q_meth_names, correct, groups_all[["researcher_postdoc"]])
    Results[["Research area"]][["Postdoc males"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_postdoc_males"]])
    Results[["Research area"]][["Postdoc females"]] <- get_indiv_Results(data_all, q_meth_names, correct,
                                                                    groups_all[["researcher_postdoc_females"]])

    # Future area counts
    Results[["Research area cnts"]][["All"]] <- get_research_area(data_all, groups_all[["All"]])
    Results[["Research area cnts"]][["males"]] <- get_research_area(data_all, groups_all[["males"]])
    Results[["Research area cnts"]][["females"]] <- get_research_area(data_all, groups_all[["females"]])

    # Future area counts
    Results[["Future area cnts"]][["All"]] <- get_future_area(data_all, groups_all[["All"]])
    Results[["Future area cnts"]][["males"]] <- get_future_area(data_all, groups_all[["males"]])
    Results[["Future area cnts"]][["females"]] <- get_future_area(data_all, groups_all[["females"]])

    return(Results)

} # get_all_Results()

get_indiv_Results <- function(data, q_names, correct, idx)
{
# get response accuracies from data for questions in q_names
# data: data frame, columns: questions, rows: respondents
# q_names: names of questions used to refer to columns in data
# correct: data frame with correct response codes, same column names as data
# idx: indices of respondents (rows of data) to take into account
# Returns: list with four elements, Results (counts) and Results_frac (fractions), sum_counts, sum_frac
# e.g. Results_out$frac[[qq]] (qq: question name) will return for values for Corr/Err/NoI/Skp


    Results_out <- list() # to be returned by this function

    # sub-groups of questions
    # NOTE: indices refer to q_meth_names (not q_names)
    # for reference:
    # qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal
    # Analysis", phys = "Physics", stats = "Statistics")
    qgroup <- list(AllQs = c(1:18), linalg = c(12,13), calc = c(9,10,11), progr = c(14,15,16), signal = c(2,3,4,7,8), phys = c(17,18), stats = c(1,5,6))
    # qgroup <- list(AllQs = c(1:18))
    qgroup_names <- names(qgroup)
    
    # reduce data frame to relevant respondents
    data <- data[idx,]

    n_pp <- length(which(idx)) # number of participants here

	noidea <- 5 # response code for "No idea"

	# initialise data frame for results
	Results_counts <- as.data.frame(c(1:4)) # data frame for number of responses
	names(Results_counts)[1] <- q_names[1]

	# response categories: Correct, Error, No Idea, Skipped
    resp_cats <- c("Cor", "Err", "NoI", "Skd")
	row.names(Results_counts) <- resp_cats

    # new data frame for fraction of responses
	Results_frac <- Results_counts

    # indices to participants with corr/incorr/ni/skp responses per question
    Results_inds <- list()

    # data frame for results for individual participants across questions
    Results_indiv <- list()
    r_categs <- c("Cor", "Err", "NoI") # response categories
    for (qq in qgroup_names)
    {
        for (rr in r_categs)
        {
            Results_indiv[[qq]][[rr]] <- matrix(0,n_pp,1) # initiatlise
        }
    }

    for (QQ in qgroup_names)
    {
        QQ_inds <- unlist(qgroup[QQ]) # indices to questions in this group
        n_qq <- length(QQ_inds) # number of questions considered
        a_qq <- 1/n_qq - 0.0000000001 # glm() didn't work if some values exactly 1 ###

        q_names_now <- q_names[QQ_inds] # names of questions in this group

    	for (qq in q_names_now)
    	{
    		values_now <- data[[qq]] # get data for current question from data frame
    		correct_now <- correct[[qq]] # this is the correct response for this question (1-4)
    		resp_vals <- c(1,2,3,4) # all four response options
    		incorr_now <- resp_vals[-correct_now] # values of incorrect responses (removes one element from vector)

            # indices of participants with correct responses
            cor_inds <- which( data[[qq]] == correct_now )
            # ... incorrect responses
            err_inds <- which( data[[qq]] %in% incorr_now)
            # ... no idea responses
            noi_inds <- which( data[[qq]] == noidea )
            # ... skipped responses
            skd_inds <- which( is.na( data[[qq]] ) )

            # indices to particants with corr/err/no/skd responses
            Results_inds[[qq]][["Cor"]] <- list(cor_inds)
            Results_inds[[qq]][["Err"]] <- list(err_inds)
            Results_inds[[qq]][["NoI"]] <- list(noi_inds)
            Results_inds[[qq]][["Skd"]] <- list(skd_inds)

            # update individual results
            Results_indiv[[QQ]][["Cor"]][cor_inds] <- Results_indiv[[QQ]][["Cor"]][cor_inds] + a_qq
            Results_indiv[[QQ]][["Err"]][err_inds] <- Results_indiv[[QQ]][["Err"]][err_inds] + a_qq
            Results_indiv[[QQ]][["NoI"]][noi_inds] <- Results_indiv[[QQ]][["NoI"]][noi_inds] + a_qq

    		# all responses
    		Results_counts["Cor",qq] <- length( cor_inds ) # reponse correct	
    		Results_counts["Err",qq] <- length( err_inds ) # incorrect responses
    		Results_counts["NoI",qq] <- length( noi_inds ) # response "no idea"
    		Results_counts["Skd",qq] <- length( skd_inds ) # response skipped
    		# print( sprintf("Correct: %f   Err: %f   No idea: %f   Skipped: %f\n", Results["Cor",qq], Results["Err",qq], Results["NoI",qq], Results["Skd",qq]) )

    		n_good_resp <- Results_counts["Cor",qq] + Results_counts["Err",qq] + Results_counts["NoI",qq] # number of people who responded to this question
    		n_resp <- n_good_resp + Results_counts["Skd",qq] # all responses, incl. skipped

    		# turn into fractions
    		Results_frac[[qq]] <- Results_counts[[qq]]/n_resp
    	} # qq
    } # QQ

    # summary of results across all questions
    Sum_counts <- as.data.frame(c(1:4)) # data frame for number of responses
    names(Sum_counts) <- "sum"
    row.names(Sum_counts) <- resp_cats
    for (rr in resp_cats)
    {
        Sum_counts[rr, "sum"] <- sum( Results_counts[rr,] )        
    }
    all_counts <- sum( Sum_counts)
    Sum_frac <- Sum_counts / all_counts

    # summary of results across sub-groups of questions
    Sum_qgroup_counts <- as.data.frame(c(1:4)) # data frame for number of responses
    names(Sum_qgroup_counts)[1] <- qgroup_names[1]
    row.names(Sum_qgroup_counts) <- c("Cor", "Err", "NoI", "Skd")
    Sum_qgroup_frac <- Sum_qgroup_counts
    for (qq in qgroup_names)
    {
        for (rr in resp_cats)
        {
            Sum_qgroup_counts[rr, qq] <- sum(Results_counts[rr,qgroup[[qq]]])
        }
        all_counts <- sum(Sum_qgroup_counts[, qq])
        Sum_qgroup_frac[, qq] <- Sum_qgroup_counts[, qq] / all_counts
    }
    
	Results_out$counts <- Results_counts
	Results_out$frac <- Results_frac
    Results_out$sum_counts <- Sum_counts
    Results_out$sum_frac <- Sum_frac
    Results_out$sum_qgroup_frac <- Sum_qgroup_frac

    Results_out$inds <- Results_inds
    Results_out$indiv <- Results_indiv

	return( Results_out )
} # get_indiv_Results


get_training_needs <- function(data_ori, idx)
{
# plot responses for question "Training needs"    
# data_ori: data frame, rows: respondents
# idx: indices of respondents (rows of data) to take into account
# Returns: list with six elements, Results (fractions)

    # response options:   
    # 1: a lot
    # 2: significantly
    # 3: a little
    # 4: not at all
    # 5: don't know
    # 6: skipped (NA)

    # get data for valid respondents
    data <- data_ori[["Training needs"]][idx]
    len <- length(data)

    ### get data into vector for bar plot
    res_vec <- matrix(0,6,1) # vector: number of response options

    for (cc in c(1:5))
    {
        res_vec[cc] <- length(which(data==cc)) / len
    }

    res_vec[6] <- length(which(is.na(data))) / len

    Results <- res_vec

    return( Results )
} # get_training_needs()


get_research_area <- function(data_ori, idx)
{
# get counts for question "Research area"    
# data_ori: data frame, rows: respondents
# idx: indices of respondents (rows of data) to take into account
# Returns: list with six elements, Results

    # response options:   
    # 1: Undergraduate student
    # 2: PhD student
    # 3: Post-doc
    # 4: Other (please specify)
    # added/changed below: 4: master's, 5: research assistant

    # get data for valid respondents
    data <- data_ori[["Research area"]][idx]
    len <- length(data)

    ### get data into vector for bar plot
    res_vec <- matrix(0,6,1) # vector: number of response options

    # Take care of "Other" option (master's, RAs)
    for (uu in 1:len)
    {
        # skipped?
        if ( is.na(data[[uu]]) )
        {
            data[[uu]] = 6
        }
        else
        {
            # master
            if (data[[uu]]=="1.000000")
            {
                data[[uu]] = 1
            } 
            else if ( data[[uu]]=="2.000000" | any(grep('phd', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 2
            }
            else if (data[[uu]]=="3.000000")
            {
                data[[uu]] = 3
            }
            else if ( any(grep('master', tolower(data[[uu]])) == 1) |
                                any(grep('Msc', tolower(data[[uu]])) == 1) |
                                any(grep('MPhil', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 4
            }
            else if ( any(grep('research assistant', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 5
            }
        }
    }

    

    for (cc in c(1:6))
    {
        res_vec[cc] <- length(which(data==cc))
        res_vec[cc] <- res_vec[cc] / len # fraction
    }

    # res_vec[1] = length(which(groups[["researcher_undgrad"]]))
    # res_vec[2] = length(which(groups[["researcher_phd"]]))
    # res_vec[3] = length(which(groups[["researcher_postdoc"]]))
    # res_vec[4] = length(which(groups[["researcher_resass"]]))
    # res_vec[5] = length(which(groups[["researcher_skipped"]]))

    Results <- res_vec

    return( Results )

} # get_research_area()


get_future_area <- function(data_ori, idx)
{
# plot responses for question "Future area"    
# data_ori: data frame, rows: respondents
# idx: indices of respondents (rows of data) to take into account
# Returns: list with seven elements, Results

    # response options:   
    # 1: Psychology
    # 2: Basic cognitive science
    # 3: Basic cognitive neuroscience
    # 4: Clinical neuroscience or neuropsychology
    # 5: Don't know
    # 6: Other (please specify)
    # 7: skipped

    # get data for valid respondents
    data <- data_ori[["Future area"]][idx]
    len <- length(data)

    ### get data into vector for bar plot
    res_vec <- matrix(0,7,1) # vector: number of response options

    # Take care of "Other" option
    for (uu in 1:len)
    {
        # skipped?
        if ( is.na(data[[uu]]) )
        {
            data[[uu]] = 7
        }
        else
        {
            if (data[[uu]]=="1.000000")
            {
                data[[uu]] = 1
            } 
            else if ( data[[uu]]=="2.000000")
            {
                data[[uu]] = 2
            }
            else if ( (data[[uu]]=="3.000000") | any(grep('neuro', tolower(data[[uu]])) == 1) |
                        any(grep('imaging', tolower(data[[uu]])) == 1) )
            {
                data[[uu]] = 3
            }
            else if (data[[uu]]=="4.000000")
            {
                data[[uu]] = 4
            }
            else if (data[[uu]]=="5.000000")
            {
                data[[uu]] = 5
            }
            else # Other
            {
                data[[uu]] = 6
            }                       
        }
    }

    for (cc in c(1:7))
    {
        res_vec[cc] <- length(which(data==cc))
        res_vec[cc] <- res_vec[cc] / len # fraction
    }   

    Results <- res_vec

    return( Results )

} # get_future_area()



plot_general_questions <- function(data, groups, my_title, bar_names, bar_legend)
{
# plot data to bar graph for "Training needs" question
# data: list of data frames with data to plot
# groups: string, indices to data for respondent group(s) to plot
# my_title: string, title for plot
# bar_names: list of string, names of individual bars in plot
# bar_legend: list of strings, what to use as legend in bar graphs, for items in "groups"

    n_groups <- length(groups)

    if (n_groups==9) {
        colors <- c("black", "violetred4", "blue", "grey40", "orange1", "lightblue1", "grey80", "darkred", "darkblue")
    } else if (n_groups==6) {
        colors <- c("violetred4", "blue", "orange1", "lightblue1", "darkred", "darkblue")
    }
    else if (n_groups==3) {
         colors <- c("black", "violetred4", "blue")
    }
    else if (n_groups==2) {
         colors <- c("violetred4", "blue")
    }
    else {
        colors <- c()
    }    

    n_bars <- length(data[[1]])-1 # -1 to skip "skipped"
    
    ### get data into matrices for bar plots
    data_mats <- list() # matrices to plot for counts and fractions

    data_mats <- matrix(0,n_groups,n_bars) # number of n_groups x n_bars, in %
        
    # create matrices n_groups x n_names (e.g. male/female x 6)
    g_cnt <- 0
    for (gg in groups) # groups of respondents, e.g. male/female
    {
        g_cnt <- g_cnt + 1
        data_mats[g_cnt,] <- 100*matrix(data[[gg]][1:n_bars],1,n_bars)
    }
   
    # bar plot with labels
    bardat <- barplot(data_mats, names=bar_names, beside=T, cex.axis=2, col=colors, cex.names=1, legend=bar_legend)
    # arrows(bardat,Dat+SD, bardat, Dat, angle=90, code=1, length=0)
    title( my_title )

    # output values on screen
    cat('\n')
    print(sprintf('Figure %s', my_title))
    data_mats <- round(data_mats, digits=2) # round fractions to second digit
    for (gg in 1:n_groups)       
    {
        print(sprintf('%s: %.2f %.2f %.2f %.2f %.2f', groups[gg], data_mats[gg,1], data_mats[gg,2], data_mats[gg,3], data_mats[gg,4], data_mats[gg,5]))
    }
} # plot_general_questions()


plot_bargraphs <- function(data, groups, quest, restype, my_title, bar_legend)
{
# plot data to bar graph
# data: list of data frames with data to plot
# groups: string, indices to data for respondent group(s) to plot
# quest: string, the question (or subgroups of questions) for which results to be plotted
# restype: type of response to be plotted (e.g. "counts", "frac", "sum_counts")
# my_title: string, title for plot
# bar_legend: what to use as legend in bar graphs, for items in "groups"

    n_groups <- length(groups)
    
    if (n_groups==9) {
        colors <- c("black", "violetred4", "blue", "grey40", "orange1", "lightblue1", "grey80", "darkred", "darkblue")
    } else if (n_groups==6) {
        colors <- c("violetred4", "blue", "orange1", "lightblue1", "darkred", "darkblue")
    }
    else if (n_groups==3) {
         colors <- c("black", "violetred4", "blue")
    }
    else if (n_groups==2) {
         colors <- c("violetred4", "blue")
    }
    else {
        colors <- c()
    }

    # response types to plot (assumed to be present in this sequence in data)
    names <- c("Corr", "Err", "No idea", "Skipped")

    # which response categories to plot (e.g. no "skipped")
    to_plot <- c(1,2,3)
    n_plot <- length(to_plot)
    
    ### get data into matrices for bar plots
    data_mats <- list() # matrices to plot for counts and fractions

    for (rr in restype) # counts and fractions
    {
        data_mats <- matrix(0,n_groups,n_plot) # number of n_groups x n_bars
        
        # create matrices n_groups x n_names (e.g. male/female x Cor/Err/NoI/Skp) for counts and frac
        g_cnt <- 0
        for (gg in groups) # groups of respondents, e.g. male/female
        {
            g_cnt <- g_cnt + 1
            data_mats[g_cnt,1:n_plot] <- 100*matrix(data[[gg]][[rr]][[quest]][to_plot],1,n_plot) # as %
        }
            
        ### error bars are NONSENSE at the moment, SD doesn't make sense
        # SD <- sd(Dat)
        # bar plot with labels        
        bardat <- barplot(data_mats, names=names[to_plot], beside=T, cex.axis=2, col=colors, cex.names=2, legend=bar_legend)
        # legend("topright", legend=bar_legend, cex = 1, ncol=2)

        # output values on screen
        cat('\n')
        print(sprintf('Figure %s', my_title))
        data_mats <- round(data_mats, digits=2) # round fractions to second digit
        for (gg in 1:n_groups)       
        {
            print(sprintf('%s: %.2f %.2f %.2f', groups[gg], data_mats[gg,1], data_mats[gg,2], data_mats[gg,3]))
        }

        title( my_title )
    }
} # plot_bargraphs()


plot_demographics <- function(demos)
{
# plot bargraphs for demographic information from demographics()
# demos: dataframe with demographics from demographics()
# will write to Demographics.pdf

    pdf_name <- sprintf("%s/Demographics.pdf", fig_outdir) # avoid some problems with long filenames
    pdf(pdf_name, onefile=TRUE)
    print ( sprintf("Plotting to PDF: %s", pdf_name) )


    # number of male/female respondents
    my_names <- c('Total', 'Males', 'Females') # used for x-labelling in figure

    group_names <- c('N_all', 'N_males', 'N_females') # indices to 'demos'

    colors <- c("black", "violetred4", "blue")

    # data_mat <- matrix(c(demos[group_names]), 1, length(group_names))
    data_mat <- as.numeric(demos[group_names])

    bardat <- barplot(data_mat, names=my_names, beside=F, cex.axis=2, col=colors, cex.names=2, las=2)

    title('# Participants')


    # number of respondents with different undergraduate degrees
    # my_names <- c('UG\nM', 'UG\nF', 'PhD\nM', 'PhD\nF', 'PD\nM', 'PD\nF', 'MS\nM', 'MS\nF', 'RA\nM', 'RA\nF')
    my_names <- c('UG', 'PhD', 'PD', 'RA')

    group_names <- c('n_ugrad_males', 'n_ugrad_females', 'n_phd_males', 'n_phd_females', 'n_pd_males', 'n_pd_females', 'n_ra_males', 'n_ra_females')

    colors <- c("black", "grey40")

    # data_mat <- matrix(c(demos[group_names]), 1, length(group_names))
    data_mat <- as.numeric(demos[group_names])

    # turn into matrix for grouped plotting
    data_mats <- matrix(0,2,4)
    data_mats[1,] <- data_mat[seq(1,8,2)]
    data_mats[2,] <- data_mat[seq(2,8,2)]

    legendtxt = c('males', 'females')

    bardat <- barplot(data_mats, names=my_names, beside=T, cex.axis=2, col=colors, cex.names=1, las=1, horiz=F, legend.text=legendtxt)

    title('# Participants')


    # number of respondents in different current research positions
    my_names <- c('Psych', 'Meth', 'Biol')

    group_names <- c('n_psych_males', 'n_psych_females', 'n_meth_males', 'n_meth_females', 'n_biol_males', 'n_biol_females')

    # data_mat <- matrix(c(demos[group_names]), 1, length(group_names))
    data_mat <- as.numeric(demos[group_names])

    data_mats <- matrix(0,2,3) 
    data_mats[1,] <- data_mat[seq(1,6,2)]
    data_mats[2,] <- data_mat[seq(2,6,2)]

    bardat <- barplot(data_mats, names=my_names, beside=T, cex.axis=2, col=colors, cex.names=1, las=1, horiz=F, legend.text=legendtxt)

    title('# Participants')

    group_names <- c('age_psych_males', 'age_psych_females', 'age_meth_males', 'age_meth_females', 'age_biol_males', 'age_biol_females')

    # data_mat <- matrix(c(demos[group_names]), 1, length(group_names))
    data_mat <- as.numeric(demos[group_names])

    data_mats <- matrix(0,2,3) 
    data_mats[1,] <- data_mat[seq(1,6,2)]
    data_mats[2,] <- data_mat[seq(2,6,2)]

    bardat <- barplot(data_mats, names=my_names, beside=T, cex.axis=2, col=colors, cex.names=1, las=1, horiz=F, legend.text=legendtxt)

    title('Age')


    dev.off()

}


get_dep_var <- function(Results_sub, qq, categ)
{
# create dependent variable for logistic_regression
# Results_sub: sub structure of Results (e.g. Results[["sex"]][["All"]])
# qq: string, name of questions
# categ: string, response category "Cor"/"Err"/"NoI"/"Skd"
    n <- nrow(Results_sub[["indiv"]][["AllQs"]][["Cor"]]) # number of participants, doesn't depend on qq or categ
    tmp <- matrix(0,n,1)
    tmp[ Results_sub[["inds"]][[qq]][[categ]][[1]] ] <- 1
    dv <- factor( tmp )
    return( dv )
} # get_dep_var()



binomial_regression <- function(dv, iv, family="binomial")
{
# compute logistic regression using glm and family="binomial"
# result computed for dv vs columns in iv
# dv: data frame, dependent variable
# iv: data frame, independent variables
# family: string, family of error distribution for glm()
# Returns: stat_list (list)
    
    stat_list <- list()
    
    # combine in data frame
    data_glm <- cbind(dv, iv)

    # formula for glm(): dv versus first column
    frm <- paste("dv ~ ", names(iv)[1])

    n_names <- length(names(iv))
    for (nn in c(2:n_names)) # if more than one column in iv
    {
        frm <- paste(frm, "+ ", names(iv)[nn])
    }

    print("Binomial GLM formula:")
    print(frm)

    frm <- as.formula(frm)

    # compute logistic regression model
    glm_out <- glm(formula=frm, data=data_glm, family=family)

    # get p-value
    p_fit <- coef(summary(glm_out))[,4]
    coef_fit <- exp(coef(glm_out))

    stat_list[["glm_out"]] <- glm_out
    stat_list[["p"]] <- p_fit
    stat_list[["coef"]] <- coef_fit

    return(stat_list)
} # binomial_regression()

multinomial_regression <- function(dv, iv_groups)
{
# compute multinomial logistic regression using multinom from nnet
# result computed for dv vs first column in iv
# dv: data frame, dependent variable
# iv_groups: dict of data frames with independent variables
#            each dict contains group of variables whose significant is
#            to be tested separately in model comparison
# Returns: stat_list (list)

# still WIP
    
    stat_list <- list()

    group_names <- names(iv_groups)
    n_groups <- length(iv_groups)
    
    # combine in data frame
    data_glm <- dv
    for (gg in group_names)
    {
        data_glm <- cbind(data_glm, iv_groups[gg])
    }

    # formulas for regression: dv versus other columns
    frm <- vector("list", n_groups+1)

    # create different models with different predictors for model comparison
    for (mm in c(1:(n_groups+1)))
    {
        print(mm)
        frm[mm] <- "dv ~ "
    }
   
    for (ff in c(1:(n_groups+1))) # per formula
    {
        pred_idx <- c(1:n_groups) # which predictor groups to include
        if (ff>1) # include everything for first formula
        {
            pred_idx <- pred_idx[-(ff-1)] # remove appropriate predictor group
        }

        n_preds <- length(pred_idx)

        for (pp in c(1:n_preds)) # across chosen predictor groups
        {
            print(pp)
            n_iv <- length(iv_groups[group_names[pred_idx[pp]]])
            print(group_names[pred_idx[pp]])
            # beware of [[]]
            iv_names <- names(iv_groups[[group_names[pred_idx[pp]]]])
            print(iv_names)
       
            # add predictor groups to appropriate formulas
            for (ii in c(1:n_iv))
            {
                if ((pp>1) || (ii>1))
                {
                    frm[ff] <- paste(frm[ff], "+ ")
                }
                frm[ff] <- paste(frm[ff], iv_names[ii])               
            }
        }
    }
   
    print( sprintf("Formula %s vs %s", frm[1], frm[2]) )

    frm1 <- as.formula(frm[[1]])
    frm2 <- as.formula(frm[[2]])

    # testing multinom ###
    print("Multinomial regression.")
    # mnr_out <- multinom(formula=frm, data=data_glm)
    # refLevel specifies the element of response variable to use as reference

    # http://dwoll.de/rexrepos/posts/regressionMultinom.html#model-comparisons---likelihood-ratio-tests
    vglm_out1 <- vglm(formula=frm1, data=data_glm, family=multinomial(refLevel=1))
    vglm_out2 <- vglm(formula=frm2, data=data_glm, family=multinomial(refLevel=1))

    vglm_stats <- lrtest(vglm_out1, vglm_out2)

    return(vglm_stats)
} # multinomial_regression()



ordered_logistic_regression <- function(dv, iv_groups)
{
# compute ordered logistic regression using polr from MASS
# (result computed for dv vs first column in iv ???)
# dv: data frame, dependent variable
# iv_groups: dict of data frames with independent variables
#            each dict contains group of variables whose significance is
#            to be tested separately in model comparison
# Returns: output of polr()

# still WIP
# https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
    
    stat_list <- list()

    group_names <- names(iv_groups)
    n_groups <- length(iv_groups)
    
    # combine dependent and independent variables in data frame
    data_glm <- dv
    for (gg in group_names)
    {
        data_glm <- cbind(data_glm, iv_groups[gg])
    }
   
    # add predictor groups to appropriate formulas
    frm <- "dv ~ "
    for (ii in c(1:n_groups))
    {
        if (ii>1)
        {
            frm <- paste(frm, "+ ")
        }
        frm <- paste(frm, names(iv_groups[[group_names[ii]]]))
    }
   
    print( sprintf("Formula %s", frm) )

    frm1 <- as.formula(frm)

    # testing multinom ###
    print("Ordered logistic regression.")

    model <- polr(formula=frm1, data=data_glm, Hess=TRUE)

    return(model)
} # ordered_logistic_regression()


plot_Results <- function(Results, fig_outdir)
{
# plot results to figures (PDF)

    # SUMMARY PLOTS across ALL QUESTIONS
    # restype <- c("sum_counts", "sum_frac") # summary across all questions
    restype <- c("sum_frac")

    pdf_name <- sprintf("%sSummaries_General.pdf", fig_outdir) # avoid some problems with long filenames
    pdf(pdf_name, onefile=TRUE)
    print ( sprintf("Plotting to PDF: %s", pdf_name) )

    # All
    # Results$sex$All$sum_frac$sum
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
    groups <- c("Undgrad All", "Undgrad males", "Undgrad females", "PhD All", "PhD males", "PhD females",
                    "Postdoc All", "Postdoc males", "Postdoc females")
    bar_legend <- c("Undgrad All", "Undgrad males", "Undgrad females", "PhD All", "PhD males", "PhD females",
                    "Postdoc All", "Postdoc males", "Postdoc females")
    plot_bargraphs(Results[["Research area"]], groups, "sum", restype, "Researcher type", bar_legend)

    # Training needs
    print ("Training needs")
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    bar_names <- c("A lot", "Significantly", "A little", "Not at all", "Don't know")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend)

    groups <- c("Undgrad All", "PhD All", "Postdoc All")
    bar_legend <- c("Undgrad All", "PhD All", "Postdoc All")
    bar_names <- c("A lot", "Significantly", "A little", "Not at all", "Don't know")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend)

    groups <- c("Undgrad All", "Undgrad males", "Undgrad females", "PhD All", "PhD males", "PhD females",
                    "Postdoc All", "Postdoc males", "Postdoc females")
    bar_legend <- c("Undgrad All", "Undgrad males", "Undgrad females", "PhD All", "PhD males", "PhD females",
                    "Postdoc All", "Postdoc males", "Postdoc females")
    bar_names <- c("A lot", "Significantly", "A little", "Not at all", "Don't know")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend)

    groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
    bar_legend <- c("Undergrads Psychology M", "Undergrads Psychology F", "Undergrads Methods M", "Undergrads Methods F", "Undergrads Biology M", "Undergrads Biology F")
    bar_names <- c("A lot", "Significantly", "A little", "Not at all", "Don't know")
    plot_general_questions(Results[["Training needs"]], groups, "Training needs", bar_names, bar_legend)

    # Researcher type counts
    print ("Researcher type cnts")
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    bar_names <- c("Undergraduate", "PhD", "Post-Doc", "Master's", "Res'ch Ass't")
    plot_general_questions(Results[["Research area cnts"]], groups, "Researcher type", bar_names, bar_legend)

    # Future area
    print ("Future area cnts")
    groups <- c("All", "males", "females")
    bar_legend <- c("All", "males", "females")
    bar_names <- c("Psych", "Cog Sci", "Cog N'sci", "Cli N'sci", "DK", "Other")
    plot_general_questions(Results[["Future area cnts"]], groups, "Future area", bar_names, bar_legend)

    dev.off()

    # SUMMARY PLOTS across SUB-GROUPS OF QUESTIONS
    pdf_name <- sprintf("%sSummaries_QuestionGroups.pdf", fig_outdir) # avoid some problems with long filenames
    pdf(pdf_name, onefile=TRUE)
    print ( sprintf("Plotting to PDF: %s", pdf_name) )

    # restype <- c("sum_counts", "sum_frac") # summary across all questions
    restype <- c("sum_qgroup_frac")

    # subgroups of questions
    qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal Analysis",
                    phys = "Physics", stats = "Statistics")

    for (qq in names(qgroup_names))
    {
        # gender
        groups <- c("All", "males", "females")
        bar_legend <- c("All", "males", "females")
        title <- sprintf("Gender | %s", qgroup_names[qq])
        print(title)
        plot_bargraphs(Results[["sex"]], groups, qq, restype, title, bar_legend)

        # degree
        groups <- c("ugrad_group_psych", "ugrad_group_meth", "ugrad_group_biol")
        bar_legend <- c("Undergrads Psychology", "Undergrads Methods", "Undergrads Biology")
        title <- sprintf("Undergraduate Degree | %s", qgroup_names[qq])
        print(title)
        plot_bargraphs(Results[["degree"]], groups, qq, restype, title, bar_legend)

        # degree-by-sex
        groups <- c("ugrad_group_psych_males", "ugrad_group_psych_females", "ugrad_group_meth_males", "ugrad_group_meth_females", "ugrad_group_biol_males", "ugrad_group_biol_females")
        bar_legend <- c("Undergrads Psychology M", "Undergrads Psychology F", "Undergrads Methods M", "Undergrads Methods F", "Undergrads Biology M", "Undergrads Biology F")
        title <- sprintf("Degree-by-Gender | %s", qgroup_names[qq])
        print(title)
        plot_bargraphs(Results[["deg_sex"]], groups, qq, restype, title, bar_legend)

        # expertise
        groups <- c("expert_yes", "expert_sortof", "expert_no")
        bar_legend <- c("Expert", "Sort of Expert", "No Export")
        title <- sprintf("Expertise | %s", qgroup_names[qq])
        print(title)
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

} # plot_Results()


get_Stats <- function(Results, groups_all)
{
# compute logistic regression stats for results from get_all_Results()
# Results: list with results from get_all_Results(), entries are output from get_indiv_Results()
# groups_all: list with indices of respondents (rows of data) for different groups
# Returns: stat_list, list of results from logistic regression models
    
    ### STATISTICS
    stat_list <- list() # collect results from logistic regression

    # response categories
    # categs <- c("Cor", "Err", "NoI")
    categs <- c("Cor")

    n_pps <- length(groups_all$All)

    # factor undergraduate degree
    iv_ugrad <- matrix(0,n_pps,1)
    iv_ugrad[which(groups_all[["ugrad_group_psych"]])] = 1
    iv_ugrad[which(groups_all[["ugrad_group_biol"]])] = 2
    iv_ugrad[which(groups_all[["ugrad_group_meth"]])] = 3

    iv_ugrad <- data.frame( iv_ugrad )

    # factor researcher type
    iv_restyp <- matrix(0,n_pps,1)
    iv_restyp[which(groups_all[["researcher_undgrad"]])] = 1
    iv_restyp[which(groups_all[["researcher_phd"]])] = 2
    iv_restyp[which(groups_all[["researcher_postdoc"]])] = 3
    iv_restyp[which(groups_all[["researcher_resass"]])] = 4
    iv_restyp[which(groups_all[["researcher_skipped"]])] = 5

    iv_restyp <- data.frame( iv_restyp )

    # factor gender
    iv_gender <- 1*groups_all[["males"]]
    iv_gender <- data.frame(iv_gender)

    ## STATS (Logistic multiple regression)
    cat("\n######################\n")

    # INDEPENDENT VARIABLES
    iv <- cbind(iv_gender, iv_ugrad, iv_restyp)

    # classic binomial regression for individual questions
    # because dependent variable is 0 or 1

    cat("\n###\nIndividual questions.\n###\n")

    for (qq in q_meth_names)
    {
        cat(sprintf("\n###\n %s\n###\n", qq))

        for (cc in categs) # Cor|Err|NoI
        {
            # DEPENDENT VARIABLE
            dv <- get_dep_var(Results[["sex"]][["All"]], qq, cc)
            dv <- data.frame(dv)
            
            model <- binomial_regression(dv, iv)

            stat_list[[qq]][[cc]] <- model

            print( sprintf("Gender - Coef: %f, p: %f", model[["coef"]][2], model[["p"]][2]) )
            print( sprintf("Ugrad - Coef: %f, p: %f", model[["coef"]][3], model[["p"]][3]) )
            print( sprintf("Res Type - Coef: %f, p: %f", model[["coef"]][4], model[["p"]][4]) )
        }
    }

    # organise IVs in groups that can be tested in model comparison
    iv_groups <- list()
    # beware of [[]]
    iv_groups[['gend']] <- iv_gender
    iv_groups[['ugrad']] <- iv_ugrad
    iv_groups[['restyp']] <- iv_restyp

    # stats for subgroups of questions

    cat("\n###\nSub-groups of questions.\n###\n")

    # ordered logistic regression for subgroups of questions
    # because dependent variables can have multiple ordered levels

    qgroup_names <- list(linalg = "Linear Algebra", calc = "Calculus", progr = "Programming", signal = "Signal Analysis",
                    phys = "Physics", stats = "Statistics")

    for (qq in names(qgroup_names))
    {
        cat(sprintf("\n###\n %s\n###\n", qq))

        for (cc in categs) # Cor|Err|NoI
        {
            dv <- array( Results[["sex"]][["All"]][["indiv"]][[qq]][[cc]] )
            dv <- as.factor(dv)
            dv <- data.frame(dv)
            
            model <- ordered_logistic_regression(dv, iv_groups)        

            ## store table
            (ctable <- coef(summary(model)))

            ## calculate and store p values
            p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

            ## combined table
            (ctable <- cbind(ctable, "p value" = p))

            print(ctable)

            stat_list[[qq]][[cc]] <- ctable

            # # confidence intervals
            # (ci <- confint(model)) # default method gives profiled CIs
            
            # print(ci)
        }
    }

    cat("\n###\nAcross all questions.\n###\n")

    # ordered logistic regression for subgroups of questions
    # because dependent variables can have multiple ordered levels

    for (cc in categs)
    {
        cat("\n")
        print( cc )

        dv <- array( Results[["sex"]][["All"]][["indiv"]][["AllQs"]][[cc]] )
        dv <- as.factor(dv)
        dv <- data.frame(dv)

        model <- ordered_logistic_regression(dv, iv_groups)    

        ## store table
        (ctable <- coef(summary(model)))

        ## calculate and store p values
        p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

        ## combined table
        (ctable <- cbind(ctable, "p value" = p))

        print(ctable)

        stat_list[[qq]][[cc]] <- ctable

        # confidence intervals
        (ci <- confint(model)) # default method gives profiled CIs
        
        print(ci)
    }

    return( stat_list )

} # get_Stats()