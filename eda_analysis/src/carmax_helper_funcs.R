######################################################
####### Get midpoint and upper/lower bounds of #######
###### ordinal labels, i.e. mileage and pricing ######
######################################################
getMidpointAndRange <- function(labels){
  # Remove nuisance characters/split into boundaries
  library("stringr")
  labels <- gsub("[k+$miles]", "", labels)
  labels <- str_split_fixed(labels, " to ", 2)
  bounds <- matrix(as.numeric(labels), ncol=2)
  midpoints <- (bounds[,1]+bounds[,2])/2
  
  retDF <- data.frame(lower=bounds[,1], midpoint=midpoints, upper=bounds[,2])
  return(retDF)
}

###############################################
####### Add uniform jitter to intervals #######
####### Add exponential jitter to edges #######
###############################################
jitterValues <- function( 
    midpoints,
    lower.lims,
    upper.lims,
    end.width,
    soft.min = NA
    ){
  # Create empty vector for jittered values
  jitterVals <- rep(NA, length(midpoints))
  
  # Loop over each value and sample from the appropriate distribution
  for (i in 1:length(midpoints)){
    # Check if this is the high end of the data
    if (is.na(upper.lims[i])){
      # Draw from exponential dist, add to the lower limit for this price range
      jitterVals[i] <- rexp(1, rate=1/end.width) + lower.lims[i]
    } else {
      # Check if this is an interval range or the low end
      if (lower.lims[i] > 0){
        # Draw from uniform distribution
        jitterVals[i] <- runif(1, min=lower.lims[i], max=upper.lims[i])
      } else {
        if (is.na(soft.min)){
          # Draw from uniform distribution
          jitterVals[i] <- runif(1, min=lower.lims[i], max=upper.lims[i])
        } else {
          # Values will exponentially decay
          rval <- rexp(1, rate=1/soft.min) 
          rval <- min(rval, upper.lims[i]) # Bound by the max for this range
          jitterVals[i] <- upper.lims[i] - rval # Subtract from max end
        }
      }
    }
  }
  jitterVals[jitterVals < 0] <- 0
  return(jitterVals)
}

#############################################
######### Datatype fixing functions #########
#############################################
mileage.as.ordinal <- function(data){
  # Mileage levels
  mileage <- unique(data$mileage)
  mileage <- data.frame(mileage=mileage)
  mileage[,c("mileageLower", "mileageMid", "mileageUpper")] <- getMidpointAndRange(mileage$mileage)
  mileage.levels <- mileage[order(mileage$mileageLower),"mileage"]
  data$mileage <- factor(data$mileage, levels = mileage.levels)
  # Mileage appraised levels
  mileage_appraisal <- unique(data$mileage_appraisal)
  mileage_appraisal <- data.frame(mileage_appraisal=mileage_appraisal)
  mileage_appraisal[,c("mileage_appraisalLower", "mileage_appraisaleMid", "mileageAppraisalUpper")] <- getMidpointAndRange(mileage_appraisal$mileage_appraisal)
  mileage_appraisal.levels <- mileage[order(mileage_appraisal$mileage_appraisalLower),"mileage_appraisal"]
  data$mileage_appraisal <- factor(data$mileage_appraisal,levels = mileage_appraisal.levels)
  
  data$mpg_city <- as.numeric(data$mpg_city)
  data$mpg_highway <- as.numeric(data$mpg_highway)
  data$fuel_capacity <- as.numeric(data$fuel_capacity)
  data$mpg_city_appraisal <- as.numeric(data$mpg_city_appraisal)
  data$mpg_highway_appraisal <- as.numeric(data$mpg_highway_appraisal)
  data$fuel_capacity_appraisal <- as.numeric(data$fuel_capacity_appraisal)
  return(data)
}

price.as.ordinal <- function(data){
  data$appraisal_offer <- factor(as.factor(data$appraisal_offer),
                                 levels=c("$0k to $5k",
                                          "$5k to $10k",
                                          "$10k to $15k",
                                          "$15k to $20k",
                                          "$20k to $25k",
                                          "$25k to $30k",
                                          "$30k to $35k",
                                          "$40k+"))
  data$price <- factor(as.factor(data$price),
                       levels=c("$0 to $15k",
                                "$15k to $20k",
                                "$20k to $25k",
                                "$25k to $30k",
                                "$30k to $35k",
                                "$35k to $40k",
                                "$40k to $45k",
                                "$45k to $50k",
                                "$50k to $55k",
                                "$55k to $60k",
                                "$65k to $70k",
                                "$70k+"))
  return(data)
}

cylinders.as.ordinal <- function(data){
  # Cylinder levels
  cyl.levels <- c(0, 2, 3, 4, 5, 6, 7, 10, 12, 16)
  data$cylinders <- factor(data$cylinders,
                           levels = cyl.levels)
  data$cylinders_appraisal <- factor(data$cylinders_appraisal,
                                     levels = cyl.levels)
  return(data)
}

engine.as.ordinal <- function(data){
  engine.levels <- c('0', '0.6L',
                     '1.0L', '1.2L', '1.3L', '1.4L', '1.5L', '1.6L', "1.7", '1.8L', '1.9L',
                     '2.0L', '2.1L', '2.2L', '2.3L', '2.4L', '2.5L', '2.6L', '2.7L', '2.8L', '2.9L',
                     '3.0L', '3.1L', '3.2L', '3.3L', '3.4L', '3.5L', '3.6L', '3.7L', '3.8L', '3.9L',
                     '4.0L', '4.1L', '4.2L', '4.3L', '4.4L', '4.5L', '4.6L', '4.7L', '4.8L',
                     '5.0L', '5.2L', '5.3L', '5.4L', '5.5L', '5.6L', '5.7L', '5.8L', '5.9L',
                     '6.0L', '6.1L', '6.2L', '6.3L', '6.4L', '6.6L', '6.7L','6.8L',
                     '7.0L', '8.3L')
  # Consider further contracting these into smaller
  # categories or using a continuous variable...
  data$engine <- factor(data$engine,
                        levels = engine.levels)
  data$engine_appraisal <- factor(data$engine_appraisal,
                                  levels = engine.levels)
  return(data)
}

#############################################
####### Data loading + fix data types #######
#############################################
load.carmax.data <- function(
    data.path,
    seed = 20396928, # Random seed, i.e. for Jittering
    jitter = FALSE, # Whether to add numeric jitter to pricing data
    replace.file = FALSE # Replace original data with data containing new columns
    ){
  set.seed(seed)
  data <- read.csv(data.path, na.strings=c("null",""))
  
  # Get the midpoint/boundaries for each price interval and create random jitter
  # jitterPrice --> jittered price
  # jitterAppraisal --> jittered appraisal_offer
  if (("jitterPrice" %in% colnames(data)) | !jitter){
    # Pass
  } else {
    # Get price midpoints and jitters
    priceLabels <- unique(data$price)
    prices <- data.frame(price=priceLabels)
    prices[,c("priceLower", "priceMid", "priceUpper")] <- getMidpointAndRange(prices$price)
    data <- merge(data, prices, on="price")
    data$jitterPrice <- jitterValues(midpoints = data$priceMid,
                                     lower.lims = data$priceLower,
                                     upper.lims = data$priceUpper,
                                     end.width = 7.5, # Scale parameter for rexp for last pricing bin ($70k+))
                                     soft.min = 2) # Jitter so that values increase exponentially for the first interval
  }
  if (("jitterAppraisal" %in% colnames(data)) | !jitter){
    # Pass
  } else {
    # Get appraisal offer midpoints and jitters  
    appraisalPriceLabels <- unique(data$appraisal_offer)
    appraisalPrices <- data.frame(appraisal_offer=appraisalPriceLabels)
    appraisalPrices[,c("appraisalLower", "appraisalMid", "appraisalUpper")] <- getMidpointAndRange(appraisalPrices$appraisal_offer)
    data <- merge(data, appraisalPrices, on="appraisal_offer")
    data$jitterAppraisal <- jitterValues(midpoints = data$appraisalMid,
                                         lower.lims = data$appraisalLower,
                                         upper.lims = data$appraisalUpper,
                                         end.width = 7.5*(40/70), # Scale parameter for rexp for last pricing bin ($40k+))
                                         soft.min = NA) # Add uniform jitter to first interval
  }

  ### Ensure ordinal variables are set as factors w/ correct level order
  data <- price.as.ordinal(data)
  data <- mileage.as.ordinal(data)
  data <- cylinders.as.ordinal(data)
  data <- engine.as.ordinal(data)
  data$market <- as.factor(data$market)
  
  # Consider continuous version of engine size
  data$engineCont <- gsub("L", "", data$engine)
  data$engineCont <- as.numeric(data$engineCont)
  data$engine_appraisalCont <- gsub("L", "", data$engine_appraisal)
  data$engine_appraisalCont <- as.numeric(data$engine_appraisalCont)
  
  
  ### All other categorical to factor
  data$online_appraisal_flag <- as.factor(data$online_appraisal_flag)
  data$trim_descrip <- as.factor(data$trim_descrip)
  data$trim_descrip_appraisal <- as.factor(data$trim_descrip_appraisal)

  if (replace.file){
    print(paste("Overwriting data at ", data.path))
    write.csv(data, data.path, row.names=F) 
  }
  return(data)
}

####################################################
####### Split appraisal and purchase entries #######
####################################################
carmax.data.toLong <- function(
    data,
    additional_cols = NA
    ){
  warning("This function causes some NAs in pricing--need to fix. Also does not handle mileage or other ordinal factors.")
  purchase_cols <- c("price","model_year","mileage","make","model",
                     "trim_descrip","body","color","engine","cylinders",
                     "mpg_city","mpg_highway","horsepower","fuel_capacity",
                     "priceLower", "priceMid", "priceUpper", "jitterPrice",
                     "engineCont", "market")
  
  appraisal_cols <- c("appraisal_offer","model_year_appraisal", "mileage_appraisal",
                      "make_appraisal", "model_appraisal", "trim_descrip_appraisal",
                      "body_appraisal", "color_appraisal", "engine_appraisal",
                      "cylinders_appraisal", "mpg_city_appraisal",
                      "mpg_highway_appraisal", "horsepower_appraisal",
                      "fuel_capacity_appraisal", "appraisalLower", "appraisalMid",
                      "appraisalUpper", "jitterAppraisal", "engine_appraisalCont", "market")

  mapper <- data.frame(purchase_cols=purchase_cols, appraisal_cols=appraisal_cols)
  
  if (class(additional_cols) == "data.frame"){
    mapper <- rbind(mapper, additional_cols)
  }
  
  mapper <- mapper[mapper$purchase_cols %in% colnames(data),]
  
  purchaseData <- data[,mapper$purchase_cols]
  appraisalData <- data[,mapper$appraisal_cols]
  
  names(appraisalData) <- mapper$purchase_cols[match(names(appraisalData), mapper$appraisal_cols)]
  
  purchaseData$itransaction <- "purchase"
  appraisalData$itransaction <- "appraisal"
  
  newData <- rbind(purchaseData, appraisalData)
  firstInterval <- c("$0k to $5k", "$5k to $10k", "$10 to $15k")
  lastInterval <- c("$35k to $40k", "$40k to $45k", "$45k to $50k", "$50k to $55k", "$55k to $60k", "$65k to $70k", "$70k+")
  newData[newData$price %in% firstInterval, "price"] <- "$0 to $15k"
  newData[newData$price %in% lastInterval, "price"] <- "$40k+"
  
  newData$price <- factor(data$price,
                          levels=c("$0 to $15k","$15k to $20k","$20k to $25k",
                                   "$25k to $30k", "$30k to $35k", "$40k+"))
  
  
  return(newData)
}