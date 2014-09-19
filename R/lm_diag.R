#' linear model function diagnostic with custom output
#' @param formula for linear model with y outcome ~ relation and covariates divided with + and interacinos with : and/or *
#' @param dataset is the data for lm model
#' @return returns three diagnostics graphs plotted in base plotting systime.
#' @export
lm_diag<-function(formula, dataset){
        #run model and print specific output
        model1<-lm(formula=formula, data=dataset)
        stats<-round(c(summary(model1)$fstatistic[c(1,3)], 
                       summary(model1)$sigma, 
                       summary(model1)$r.squared, 
                       summary(model1)$adj.r.squared),3)
        names(stats)<-c("F","DF", "Sigma","Rsq","AdjRsq")
        l1<-list(round(summary(model1)$coefficients,3), stats)
        names(l1)<-c("Coefficients","Stats")
        print(l1)
        #run specific diagnostic visualisation
        par(mfrow=c(1,3), pch=19)
        hist(model1$residuals, main="Histogram of residuals", xlab="", col="lightblue", border = "white")
        plot(model1, 1)
        plot(model1, 2)
}