# Let's use Scott's excellent Synthetic Control Example on male incarceration. 
# Synthetic Controls in R are easy to implement thanks to the Synth and SCtools packages. 
# But choices need to be made! Anyone can make soup. Doesn't mean it will taste good. 
rm(list=ls())
library(pacman)
p_load(tidyverse, haven, Synth, devtools, SCtools)
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_13")
read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

texas <- read_data("texas.dta") %>%
  as.data.frame(.)

# Now you need to prepare the data. 
# foo	 = The dataframe with the panel data.

# predictors	 <- A vector of column numbers or column-name character strings that identifies the predictors' 
#columns. All predictors have to be numeric.

# predictors.op	 <- A character string identifying the method (operator) to be used on the predictors. 
# Default is "mean". rm.na = T is hardwired into the code. See *Details*.

# special.predictors	
# A list object identifying additional numeric predictors and their associated pre-treatment
# years and operators (analogous to “predictors.op” above). See *Details*.

# dependent	
#A scalar identifying the column number or column-name character string that corresponds to 
# the numeric dependent (outcome) variable.

#unit.variable	
#A scalar identifying the column number or column-name character string associated unit numbers.
#The unit.varibale has to be numeric.

#unit.names.variable	
#A scalar or column-name character string identifying the column with the names of the units. This 
#variable has to be of mode character.


#time.variable	
# A scalar identifying column number or column-name character string associated with period (time) data. 
#The time variable has to be numeric.

# treatment.identifier	
# A scalar identifying the “unit.variable” number or a character string giving the “unit.name ”of 
# the treated unit. If a character is supplied, a unit.names.variable also has to be supplied to identify 
#the treated unit.

#controls.identifier	
#A scalar identifying the “unit.variable” numbers or a vector of character strings giving 
#the “unit.name”s of control units. If a character is supplied, a unit.names.variable also has to be 
#supplied to identify the control units unit.

#time.optimize.ssr	
#A numeric vector identifying the periods of the dependent variable over which the loss
#function should be minimized (i.e. the periods over which mean squared prediction error (MSPE) ,
#that is the sum of squared residuals between treated and the synthetic control unit, are minimized.

#time.plot	
#A vector identifying the periods over which results are to be plotted with gaps.plot and path.plot.


dataprep_out <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1993,
  special.predictors = list(
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1993,
  time.plot = 1985:2000
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)

# This next part takes a long time to run. 
placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3)

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)
