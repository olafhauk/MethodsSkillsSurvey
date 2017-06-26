prune_respondents <- function(data_all, qnames)
# remove respondents depending on skipped responses
# data: dataframe from survey data
# qnames: names of knowledge questions

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

    print( sprintf("Consented: %d - Gender responses: %d - Ugrad responses: %d - Enough responses: %d",
                                                                    n_consent, n_gender, n_ugrad, n_na) )
    print( sprintf("Remaining respondents: %d", n_all) )

    print( sprintf("Number of respondents who skipped all knowledge questions: %d", n_skip_all))

    # some awkward entries
    if (data$Age[[34]] == "40br")
    {
        data$Age[[34]] = "40.000000" # I assume typo
    }

    return( data )
}


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
        groups[["researcher_master"]][uu] <- FALSE
        groups[["researcher_resass"]][uu] <- FALSE
        groups[["researcher_skipped"]][uu] <- FALSE
        # skipped?
        if ( is.na(res_type[[uu]]) )
        {
            groups[["researcher_skipped"]][uu] <- TRUE
        }
        else
        {
            # master
            if (res_type[[uu]]=="1.000000")
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
            else if ( any(grep('master', tolower(res_type[[uu]])) == 1) |
                                any(grep('Msc', tolower(res_type[[uu]])) == 1) |
                                any(grep('MPhil', tolower(res_type[[uu]])) == 1) )
            {
                groups[["researcher_master"]][uu] <- TRUE
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
    groups[["researcher_master_males"]] = groups[["researcher_master"]] & groups[["males"]]
    groups[["researcher_master_females"]] = groups[["researcher_master"]] & groups[["females"]]
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
}


get_Results <- function(data, q_names, correct, idx)
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
} # get_Results


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
}


get_research_area <- function(data_ori, idx)
{
# plot responses for question "Research area"    
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

    Results <- res_vec

    return( Results )

}


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
            # master
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

}



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

    n_bars <- length(data[[1]])
    
    ### get data into matrices for bar plots
    data_mats <- list() # matrices to plot for counts and fractions

    data_mats <- matrix(0,n_groups,n_bars) # number of n_groups x n_bars
        
    # create matrices n_groups x n_names (e.g. male/female x 6)
    g_cnt <- 0
    for (gg in groups) # groups of respondents, e.g. male/female
    {
        g_cnt <- g_cnt + 1
        data_mats[g_cnt,] <- matrix(data[[gg]],1,n_bars)
    }
   
    # bar plot with labels
    bardat <- barplot(data_mats, names=bar_names, beside=T, cex.axis=2, col=colors, cex.names=1, legend=bar_legend)
    # arrows(bardat,Dat+SD, bardat, Dat, angle=90, code=1, length=0)
    title( my_title )
}


plot_bargraphs <- function(data, groups, quest, restype, my_title, bar_legend)
{
# plot data to bar graph, for raw counts and fractions separately
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
            data_mats[g_cnt,1:n_plot] <- matrix(data[[gg]][[rr]][[quest]][to_plot],1,n_plot)
        }
            
        ### error bars are NONSENSE at the moment, SD doesn't make sense
        # SD <- sd(Dat)
        # bar plot with labels        
        bardat <- barplot(data_mats, names=names[to_plot], beside=T, cex.axis=2, col=colors, cex.names=2, legend=bar_legend)
        # legend("topright", legend=bar_legend, cex = 1, ncol=2)
        title( my_title )
    }
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
}



binomial_regression <- function(dv, iv, family="binomial")
{
# compute logistic regression using glm and family="binomial"
# result computed for dv vs first column in iv
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

    # get interesting values for gender/ugrad/researcher type
    coef_gender <- coef_fit[2]
    p_gender <- p_fit[2]
    coef_ugrad <- coef_fit[3]
    p_ugrad <- p_fit[3]
    coef_restyp <- coef_fit[4]
    p_restyp <- p_fit[4]

    print( sprintf("Gender - Coef: %f, p: %f", coef_gender, p_gender) )
    print( sprintf("Ugrad - Coef: %f, p: %f", coef_ugrad, p_ugrad) )
    print( sprintf("Res Type - Coef: %f, p: %f", coef_restyp, p_restyp) )

    return(stat_list)
}

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
    print("combine")
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
   
    print("make formulas")
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
}