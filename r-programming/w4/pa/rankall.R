rankall <- function(outcome, num = "best") {
        ##Read outcome data
        data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character",
        na.string="Not Available")
        set <- data[, c("State", "Hospital.Name",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        )]
        
	names(set) <- c("state", "hospital", "heart attack", "heart failure", "pneumonia")
        #good <- complete.cases(set)
                
        if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
	
	set <- set[!is.na(set[outcome]), ]
        
	## Return hospital name in that state with lowest 30-day death
        set[, outcome] <- as.numeric(set[, outcome])
        res_set <- set[, c("hospital", "state",  outcome)]
        names(res_set) <- c("hospital", "state", "outcome")
        print(head(res_set))
        
        ranked_set <- res_set[with(res_set, order(state, outcome, hospital)), ]

        print(head(ranked_set)) 

	getRow <- function(group)
	{
		
		row <- num
		if (row == "best")
	        {
                	row = 0
	        }
        	else if (row == "worst")
	        {
        	        row = nrow(group)
	        }

        	if (row > nrow(group))
	        {
        	       x <- group[1, c("hospital", "state")]
		       x[1, "hospital"] = NA		       		       
	               x
	        }
		else
	        {                	
		        group[row, c("hospital", "state")]
        	}
	}
	
	s <- split(ranked_set, ranked_set$state)
	l <- lapply(s, getRow)
	r <- Reduce(function(...) merge(..., all=T), l)
	r[with(r, order(state)), ]

}
