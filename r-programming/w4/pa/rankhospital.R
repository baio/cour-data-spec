rankhospital <- function(state, outcome, num = "best") {
	##Read outcome data
        data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character",
        na.string="Not Available")
        set <- data[, c("State", "Hospital.Name",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        )]
        names(set) <- c("state", "hospital", "heart attack", "heart failure", "pneumonia")
        good <- complete.cases(set)
        set <- set[good, ]
                 
        ## Check that state and outcome are valid
        if (!is.element(state, set$state)){
                stop("invalid state")
        }
        if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with lowest 30-day death
        set[, outcome] <- as.numeric(set[, outcome])
        res_set <- set[set$state == state, c("hospital", outcome)]
	names(res_set) <- c("hospital", "outcome")
        print(head(res_set))
        
	ranked_set <- res_set[with(res_set, order(outcome, hospital)), ]

	print(head(ranked_set))
	#lower_outcome = min(res_set[outcome])
        #print(lower_outcome)
        #lower_hospitals <- res_set[res_set[outcome] == lower_outcome, c("hospital")]
	row <- num
	if (row == "best")
	{
		row = 0
	}
	else if (row == "worst")
	{
		row = nrow(ranked_set)
	}
	
	if (row > nrow(ranked_set))
	{
		NA
	}
	else
	{	
		print(row)        	
		ranked_set[row, "hospital"]
	}
        
}

