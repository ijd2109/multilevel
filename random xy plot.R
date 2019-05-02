###################################################################
# xy.randomplot()                         ijd2109@tc.columbia.edu #
# --------------------------------------------------------------- #
# Wrapper around xy.plot: Enter x and data as you would           #
# for xyplot() from the {lattice} package; add additional         #
# arguments "groups", and "number.of.groups" to reflect           #
# the name of the grouping factor and the number of               #
# plots to plot, respectively.                                    #
# --------------------------------------------------------------- #
# Outputs an xyplot of as many scatterplots as groups specified,  #
# comprised of randomly selected groups, which are sampled        #
# independently from all groups in the entire dataset each time.  #
###################################################################
xy.randomplot <- function(x, data, groups, number.of.groups=4) {
  if (missing(groups))
    stop("What is the name of the grouping variable? 
         Try entering it as the third argument")
  if (class(groups) != "character")
    stop("arg:groups must be a character string 
         containing a variable name")
  arg1 <- x
  list <- sample(unique(data[,groups]),size = number.of.groups)
  newdata <- data.frame()
  for (n in 1:number.of.groups) {
    newdata <- rbind(newdata, data[data[,groups] == list[n],])
  }
  return(xyplot(arg1, newdata))
}