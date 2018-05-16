#' A function that plots missingness
#' requires `reshape2`
#' @param x dataframe class object
#' @export 


missing_df <- function(x){
        require(reshape2)
        require(ggplot2)
        require(dplyr)
        x %>% 
                is.na %>%
                melt %>%
                ggplot(data = .,
                       aes(x = Var2,
                           y = Var1)) +
                geom_raster(aes(fill = value)) +
                scale_fill_grey(name = "",
                                labels = c("Present","Missing")) +
                theme_minimal() + 
                theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
                labs(x = "Variables in Dataset",
                     y = "Rows / observations")
}