##############################################################
# Historgram of single vector with normal curve and mean
##############################################################

Hist_Norm <- function(var){
  graphics.off()
  name.temp <- deparse(substitute(var))
  name <- str_extract(name.temp, '\\b\\w+$')
  x <- var
  h0 <- hist(x, plot = F)
  h <- hist(x, col="lightblue", main = name, ylim = c(0, (max(h0$counts)+(max(h0$counts)*.25))))
  xfit<-seq(min(na.omit(x)),max(na.omit(x)),length=40)
  yfit<-dnorm(xfit,mean=mean(x, na.rm = T),sd=sd(x, na.rm = T))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="Blue", lwd=2)
  segments(x0 = mean(x, na.rm = T), y0 = 0, x1 = mean(x, na.rm = T), y1 = max(h0$counts), lwd = 4, col = "red", lty = 2)
  text(x = mean(x, na.rm = T), y = (max(h0$counts)+(max(h0$counts)*.1)),
       labels = paste0("mean = ", format(round(mean(x, na.rm = T), 3), nsmall = 3)), cex = 1.5)
}
# Example
Hist_Norm(mtcars$mpg)

##############################################################
# Normal Histogram vs Central Limit Theorum Historgram
##############################################################
'Histogram of a single vector compared to a user define number
of sample means (Central Limit Theorum Histogram) '

CLT_Hist <- function(var, n){
  graphics.off()
  name.temp <- deparse(substitute(var))
  name <- str_extract(name.temp, '\\b\\w+$')
  x <- var
  h0 <- hist(x, plot = F)
  par(mfrow = c(1,2))
  h <- hist(x, col="lightblue", main = name, ylim = c(0, (max(h0$counts)+(max(h0$counts)*.25))))
  xfit<-seq(min(na.omit(x)),max(na.omit(x)),length=40)
  yfit<-dnorm(xfit,mean=mean(x, na.rm = T),sd=sd(x, na.rm = T))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="Blue", lwd=2)
  segments(x0 = mean(x, na.rm = T), y0 = 0, x1 = mean(x, na.rm = T), y1 = max(h0$counts), lwd = 4, col = "red", lty = 2)
  text(x = mean(x, na.rm = T), y = (max(h0$counts)+(max(h0$counts)*.1)),
       labels = paste0("mean = ", format(round(mean(x, na.rm = T), 3), nsmall = 3)), cex = 1.5)
  CLT <- replicate(n, mean(sample(x, .25 * length(x)), na.rm = T))
  h1 <- hist(CLT, plot = F)
  h <- hist(CLT, col="lightblue", main = paste0("CLT - ", name), ylim = c(0, (max(h1$counts)+(max(h1$counts)*.25))))
  xfit<-seq(min(na.omit(CLT)),max(na.omit(CLT)),length=40)
  yfit<-dnorm(xfit,mean=mean(CLT, na.rm = T),sd=sd(CLT, na.rm = T))
  yfit <- yfit*diff(h$mids[1:2])*length(CLT)
  lines(xfit, yfit, col="Blue", lwd=2)
  segments(x0 = mean(CLT, na.rm = T), y0 = 0, x1 = mean(CLT, na.rm = T), y1 = max(yfit), lwd = 4, col = "red", lty = 2)
  text(x = mean(CLT, na.rm = T), y = (max(h1$counts)+(max(h1$counts)*.1)),
       labels = paste0("mean = ", format(round(mean(CLT, na.rm = T), 3), nsmall = 3)), cex = 1.5)
}
# Example
CLT_Hist(mtcars$mpg, 1000)

##############################################################
# Total Data Density Histogram with mean
##############################################################
'Creates a Density Histogram with Normal Curve and Mean value for All numerical
variables (defined as variable with more than 10 unique values)'

Hist_Den <- function(data){
  graphics.off()
  for(i in names(data)){
    if(nlevels(as.factor(data[[i]])) < 10){
      data[[i]] <- as.factor(data[[i]])
    }
  }
  PNVN <- function(data){
    number_cols <- list()
    for(i in names(data)){
      if(is.numeric(data[[i]])){
        number_cols[i] <- class(data[[i]])
      }
    }
    return(names(number_cols))
  }
  par(mfrow = c(ifelse(length(PNVN(data)) <= 3, 1, ifelse(length(PNVN(data)) == 4, 2, ceiling(length(PNVN(data))/3))),
                ifelse(length(PNVN(data)) >= 3, ifelse(length(PNVN(data)) == 4, 2, 3), ifelse(length(PNVN(data)) == 2, 2, 1))))
  for(i in PNVN(data)){
    h0 <- hist(data[[i]], plot = F)
    h <- hist(na.omit(data[[i]]), col = "lightblue", main = i, xlab = i, prob = T, ylim = c(0, (max(h0$density)+(max(h0$density)*.25))))
    xfit <- seq(min(na.omit(data[[i]])), max(na.omit(data[[i]])), length = 40)
    yfit <- dnorm(xfit, mean = mean(data[[i]], na.rm = T), sd = sd(data[[i]], na.rm = T))
    lines(xfit, yfit, col = "blue", lwd = 2)
    segments(x0 = mean(data[[i]], na.rm = T), y0 = 0, x1 = mean(data[[i]], na.rm = T), y1 = max(h0$density), lwd = 4, col = "red", lty = 2)
    text(x = mean(data[[i]], na.rm = T), y = (max(h0$density)+(max(h0$density)*.1)),
         labels = paste0("mean = ", format(round(mean(data[[i]], na.rm = T), 3), nsmall = 3)), cex = 1.5)
  }
}

# Example
Hist_Den(mtcars)

##############################################################
# Total Data Frequency Histogram with mean
##############################################################
'Creates a Frequency Histogram with Normal Curve and Mean value for All numerical
variables (defined as variable with more than 10 unique values)'

Hist_Freq <- function(data){
  graphics.off()
  for(i in names(data)){
    if(nlevels(as.factor(data[[i]])) < 10){
      data[[i]] <- as.factor(data[[i]])
    }
  }
  PNVN <- function(data){
    number_cols <- list()
    for(i in names(data)){
      if(is.numeric(data[[i]])){
        number_cols[i] <- class(data[[i]])
      }
    }
    return(names(number_cols))
  }
  par(mfrow = c(ifelse(length(PNVN(data)) <= 3, 1, ifelse(length(PNVN(data)) == 4, 2, ceiling(length(PNVN(data))/3))),
                ifelse(length(PNVN(data)) >= 3, ifelse(length(PNVN(data)) == 4, 2, 3), ifelse(length(PNVN(data)) == 2, 2, 1))))
  for(i in PNVN(data)){
    h0 <- hist(data[[i]], plot = F)
    h <- hist(na.omit(data[[i]]), col = "lightblue", main = i, xlab = i, prob = F, ylim = c(0, (max(h0$counts)+(max(h0$counts)*.25))))
    xfit <- seq(min(na.omit(data[[i]])), max(na.omit(data[[i]])), length = 40)
    yfit <- dnorm(xfit, mean = mean(data[[i]], na.rm = T), sd = sd(data[[i]], na.rm = T))
    yfit <- yfit * diff(h$mids[1:2]) * length(na.omit(data[[i]]))
    lines(xfit, yfit, col = "blue", lwd = 2)
    segments(x0 = mean(data[[i]], na.rm = T), y0 = 0, x1 = mean(data[[i]], na.rm = T), y1 = max(h0$counts), lwd = 4, col = "red", lty = 2)
    text(x = mean(data[[i]], na.rm = T), y = (max(h0$counts)+(max(h0$counts)*.1)),
         labels = paste0("mean = ", format(round(mean(data[[i]], na.rm = T), 3), nsmall = 3)), cex = 1.5)
  }
}

# Example
Hist_Freq(mtcars)
