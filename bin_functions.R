# By: James Tan

# Date: 3/7/2017

# Functions to bin data over variables and add bins to dataframe

binColumn <- function(data, column, num_quantiles, include.lowest = FALSE, right = TRUE, dig.lab = 3) {
    # bin a single column in a data frame
    
    breakpoints <- stats::quantile(data[, column], seq(0, 1, 1/num_quantiles), na.rm = TRUE)
    breakpoints <- unique(round(breakpoints, 9))
    num_bins <- length(breakpoints)
    breakpoints[num_bins] <- breakpoints[num_bins] + 1e-09
    data[, paste(column, "bin", sep = "_")] <- cut(data[, column], breakpoints, include.lowest = include.lowest, right = right, 
        labels = NULL, dig.lab = dig.lab)
    
    return(list(data = data, num_bins = num_bins))
}

#' @title Create Bins for Specific Columns in a Data Frame
#'
#' @description
#' \code{binData} returns a data frame with the bins as extra columns and
#' calculates the number of bins created for each column. The bins are
#' created by quantiles on the data with the quantiles specified by num_quantiles and
#' returned as a factor.
#' 
#' @param data data frame with the data and columns in it
#' @param columns names of the input columns which are to be binned
#' @param num_quantiles numeric or vector of numerics, represent the number of
#' quantiles to split all or each of the input columns by order into (will not
#' be the final number of bins since many will be identical)
#' @param include.lowest logical, indicating if an ‘x[i]’ equal to the lowest
#' (or highest, for right = FALSE) ‘breaks’ value should be included.
#' @param right logical, indicating if the intervals should be closed on the
#' right (and open on the left) or vice versa.
#' @param dig.lab integer which is used when labels are not given. It determines
#' the number of digits used in formatting the break numbers.
#' 
#' @return \code{binColumn} returns a list with the following components:
#' 
#' @return \item{data}{data frame with the bins as additional columns}
#' @return \item{num_bins}{vector of the number of bins for each column}
#' 
#' @note This function can be called on its own but it's mainly meant to be 
#' 
#' @export
binData <- function(data, columns, num_quantiles, include.lowest = FALSE, right = TRUE, dig.lab = 3) {
    # construct bins for multiple columns in a data frame
    
    num_bins <- c()
    
    if (length(columns) > 1 & length(columns) == length(num_quantiles)) {
        # when given matching num_quantiles and startpoints for specific columns
        
        for (i in 1:length(columns)) {
            c <- columns[i]
            nb <- num_quantiles[i]
            binReturn <- binColumn(data, c, nb, include.lowest, right, dig.lab)
            data <- binReturn$data
            num_bins <- c(num_bins, binReturn$num_bins)
        }
        
    } else if (length(num_quantiles) == 1) {
        # case when given 1 numbin and startpoint for all columns
        
        for (c in columns) {
            binReturn <- binColumn(data, c, num_quantiles, include.lowest, right, dig.lab)
            data <- binReturn$data
            num_bins <- c(num_bins, binReturn$num_bins)
        }
        
    } else {
        stop("Number of columns, num_quantiles, and startpoints did not match")
    }
    
    return(list(data = data, num_bins = data.frame(columns, num_bins)))
}
