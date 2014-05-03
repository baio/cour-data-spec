best <- function(state, outcome) {
	##Read outcome data
	data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character", 
	na.string="Not Available")
	set <- data[, c("State", "Hospital.Name",
	"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
	"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
	"Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	)]
	names(set) <- c("state", "hospital", "heart attack", "heart failure", "pneumonia")
	good <- complete.cases(set)
	set <- set[good, ]
	#head(good)
	#head(set) 
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
	print(head(res_set))
	lower_outcome = min(res_set[outcome]) 
	print(lower_outcome)
	lower_hospitals <- res_set[res_set[outcome] == lower_outcome, c("hospital")]
	
	
	## rate
	sort(x)
}
