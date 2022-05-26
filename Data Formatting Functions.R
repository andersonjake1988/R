##############################################################
# Package Loading
##############################################################
library(mosaic)
library(tidyverse)

##############################################################
# Completed Formatting Functions
##############################################################

# Changing variables with less than 10 different values to Factors
mosaic::factorize() "works pretty well, but below will customize the cutoff for unique values"
for(i in names(test)){
  if(nlevels(as.factor(test[[i]])) < 10){
    test[[i]] <- as.factor(test[[i]])
  }
}

# Pull Numeric Vector Names from dataframe
PNVN <- function(data){
  number_cols <- list()
  for(i in names(data)){
    if(is.numeric(data[[i]])){
      number_cols[i] <- class(data[[i]])
    }
  }
  return(names(number_cols))
}

# Pull Numeric Columns from dataframe
PNV <- function(data){
  number_cols <- list()
  for(i in names(data)){
    if(is.numeric(data[[i]])){
      number_cols[i] <- class(data[[i]])
    }
  }
  return(data[,names(number_cols)])
}

# Extract only numbers from a mixed number/letter variable
numextract <- function(string){
  as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
}

# Full dataset number extract
numextract_tot <- function(data){
  for(i in names(data)){
    data[[i]] <- as.numeric(numextract(data[[i]]))
  }
  return(data)
}

# ggplot aesthetics
UNR <- function(){
  ggplot2::theme(text = ggplot2::element_text(color = "black", size = 15),
                 plot.title = ggplot2::element_text(face = "bold", color = "darkblue", margin = ggplot2::margin(b = 15)),
                 plot.subtitle = ggplot2::element_text(size = 10),
                 axis.ticks = ggplot2::element_line(size = 1.5),
                 axis.title = ggplot2::element_text(face = "bold", line = 2),
                 axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), color = "darkblue", size = 15),
                 axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), color = "darkblue", size = 15),
                 axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 15),color = "grey50", size = 15),
                 axis.text = ggplot2::element_text(color = "black"),
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),#, angle = 90),
                 axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 5)),
                 axis.line = ggplot2::element_line(colour = "black"),
                 axis.ticks.length = ggplot2::unit(3, "mm"),
                 plot.caption = ggplot2::element_text(color = "black"),
                 plot.background = ggplot2::element_rect(fill = "white"),
                 plot.margin = ggplot2::margin(t = 10, r = 50, b = 10, l = 10),
                 panel.background = ggplot2::element_rect(fill = "white"),
                 panel.border = ggplot2::element_rect(fill = "NA", color = "darkblue", size = 1.5),
                 legend.background = ggplot2::element_rect(color = "black", fill = "white"),
                 legend.key =  ggplot2::element_rect(fill = "white"),
                 legend.text = ggplot2::element_text(color = "black"),
                 legend.position = "bottom",
                 strip.background = ggplot2::element_rect(color = "blue", fill = "grey75", size = 2),
                 strip.text.y = ggplot2::element_text(size = 13, face = "bold"),
                 panel.grid = ggplot2::element_blank())
}

# ggplot linear regression statistics to geom_smooth layer
lm_text_pos <- function(y, x, data, xpos = NULL, ypos = NULL, alpha = 0.5, size = 2.5){
  form <- as.formula(paste0(y, ' ~ ', x))
  asdf <- summary(lm(form, data = data))
  Y_int <- round(asdf$coefficients[1], digits = 2)
  M <- round(asdf$coefficients[2], digits = 2)
  R <- round(asdf$adj.r.squared, digits = 3)
  P <- signif(asdf$coefficients[8], digits = 3)
  x_pos <- (((max(data[[x]])-min(data[[x]]))*.5)+min(data[[x]]))
  y_pos <- x_pos*M + Y_int *1.05
  form2 <- form2 <- paste(paste0("Y = ", M, "x", " + ", Y_int), 
                          paste('R^2 =', R, ',  p-value =', P), sep = '\n')
  if(is.null(xpos) & is.null(ypos)){
    return(annotate(geom = 'label', x = x_pos, y = y_pos, label = form2, 
                    size = size, alpha = alpha))
  } else if(!is.null(xpos) & is.null(ypos)){
    return(annotate(geom = 'label', x = xpos, y = y_pos, label = form2, 
                    size = size, alpha = alpha))
  } else if(is.null(xpos) & !is.null(ypos)){
    return(annotate(geom = 'label', x = x_pos, y = ypos, label = form2, 
                    size = size, alpha = alpha))
  } else if(!is.null(xpos) & !is.null(ypos)){
    return(annotate(geom = 'label', x = xpos, y = ypos, label = form2, 
                    size = size, alpha = alpha))
  }
}

ggplot(mtcars, aes(x = mpg, y = disp))+
  geom_point(size = 3)+
  geom_smooth(method = lm, se = F) +
  lm_text_pos(data = mtcars, x = 'mpg', y = 'disp', xpos = 27, size = 3)+
  UNR()
