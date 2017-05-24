#' This function calculates Benford's Low expected versus observed values.
#' @param x is a vector with numeric values
#' @param main is a main title with default value of "Variable of Interest"
#' @param legend is a legends in the plot.
#' @return plot of expectied and observed values of 1 to 9 intigers.
#' @export
benford <- function(x, main="Variable of Interest", legend=c("Benford's Law", "Variable of Interest"))
{
        # A simple example using the audit data from Rattle.
    suppressPackageStartupMessages(require(rattle)) #Package required to Benford's Law plot
    suppressPackageStartupMessages(require(colorspace))
        expect <- unlist(lapply(1:9, function(x) log10(1 + 1/x))) #Create Benford's Law expectd values
        ds <- rbind(data.frame(dat=x, grp="All")) #Creating dataframe with values to be ploted
        # Generate the data specifically for the plot. A matrix with the expected values from Benford's law and the percent of initial digist observed in the dataset.
        ds <- t(as.matrix(data.frame(expect=expect,
                                     x=calcInitialDigitDistr(x, sp="none", digit=1))))
        # Display the plot.
        plot(1:9, ds[1,], type="b", pch=19, col=rainbow_hcl(1), ylim=c(0,max(ds)), axes=FALSE, xlab="Initial Digit", ylab="Probability")
        axis(1, at=1:9)
        axis(2)
        points(1:9, ds[2,], col=rainbow_hcl(2)[2], pch=19, type="b")
        # Add a legend to the plot.
        legend("topright", legend, bty="n",  col=rainbow_hcl(2), pch=19)
        # Add a title to the plot.
        title(main=main)
}