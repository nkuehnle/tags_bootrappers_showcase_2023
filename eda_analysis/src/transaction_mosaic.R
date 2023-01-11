require("ggplot2", "ggmosaic")
transactionMosaic <- function(df, tau, sublabel){
  title <- paste("Appraisal Offer vs Purchase Price ", "(", sublabel, ")", sep="")
  subtitle <- paste("Kendall's τ", "=", sprintf(tau, fmt = "%0.4f"), sep=" ")
  subtitle <- paste("(", subtitle, ")", sep="")
  g <- ggplot(data = df) +
    geom_mosaic(aes(x = product(price, appraisal_offer), fill=price, offset=0)) + 
    labs(title=paste(title, subtitle, sep="\n")) +
    theme(axis.text.x = element_text(angle = 60, hjust=1),
          axis.text.y = element_text(angle = 0, vjust=1),
          plot.title = element_text(hjust = 0.5))
  return(g)
}