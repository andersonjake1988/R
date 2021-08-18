#' A Li-Cor peak finding function
#'
#' This function provides user's of a Li-Cor Analyzer a simple way to extract and compile peak results such as peak CO2 emissions data.
#' It will loop through a folder containing the .txt files that the Li-Cor Analyzer outputs, and compiles the results into a single R dataframe.
#' the output of the this dataframe will give the user:
#'  the filename that the sample belongs too,
#'  the sample name and replicate number,
#'  the sum of the peak range,
#'  and the maximum peak value in each range.
#'
#' @param cut.off Cut off value used to define when the peak should start and end. Defaults to 2.
#' @param standard.sum Specifies whether or not the user wants a summary of the standard statistics in the output
#' @keywords LiCor, Peak, CO2, Li-Cor
#' @export
#' @examples
#' setwd(path.package("licor.peaks"))
#' output <- Licor_Peaks()


Licor_Peaks <- function(cut.off = 2, standard.sum = F){
  packages <- c("tidyverse")
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages], quiet = T)
  }
  invisible(lapply(packages, require, character.only = TRUE))
  Peaks <- function(x){
    output <- vector()
    for(i in 1:length(x)){
      ifelse(x[i] >= cut.off, output[i] <- x[i], output[i] <- NA)
    }
    output
  }
  numextract <- function(string){
    as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
  }
  filelist <- list.files(pattern = c(".txt", ".TXT"))
  output.raw <- data.frame()
  print("Looping through Folder:")
  progress_bar <- txtProgressBar(min = 0, max = length(filelist), style = 3)
  for(a in 1:length(filelist)){
    setTxtProgressBar(progress_bar, a)
    b <- read.table(filelist[a], header = T, sep = "\t", fill = T, strip.white = T, check.names = F)
    data.1 <- dplyr::mutate(b, Sample = rep(NA, nrow(b)), .before = 1)
    names(data.1) <- c("Sample", "Test", "Time", "CO2")
    data.2 <- dplyr::filter(data.1, Test != "--------------------------------------------------------------")
    data.3 <- data.2
    for(i in 1:nrow(data.2)){
      if(is.na(data.2[i,4])==T){
        data.3[i,1] <- as.character(data.2[i,2])
      } else {
        next
      }
    }
    file.annot <- dplyr::filter(data.3, is.na(CO2)) %>%
      dplyr::mutate("temp" = c(1:length(Sample))) %>%
      dplyr::mutate("Sample" = paste0(Sample, "/", temp))
    s <- 1
    for(i in 1:nrow(data.3)){
      if(is.na(data.3$CO2[i])){
        data.3$Sample[i] <- file.annot$Sample[1]
        file.annot <- file.annot[-1, ]
      } else {
        next
      }
    }
    data.4 <- na.omit(tidyr::fill(data.3, Sample, .direction = "down"))
    Preserve.order <- unique(data.4$Sample)
    options(dplyr.summarise.inform = FALSE)
    test.2 <- data.4 %>%
      dplyr::group_by(Sample) %>%
      dplyr::mutate("Sample" = factor(Sample, levels = Preserve.order)) %>%
      dplyr::summarise("Peaks" = Peaks(CO2)) %>%
      dplyr::arrange(Sample) %>%
      dplyr::mutate("Value" = !is.na(Peaks), "Replicate" = NA)
    test.2 <- cbind(test.2, "Time" = lubridate::as_datetime(data.4$Time))
    test.2 <- dplyr::arrange(test.2, Time)
    r <- 0
    for(i in 1:(length(test.2$Value)-1)){
      if(test.2$Value[i] == T & test.2$Value[i+1] == T){
        test.2$Replicate[i] <- r
      } else if(test.2$Value[i] == F & test.2$Value[i+1] == T){
        r <- r + 1
      } else if(test.2$Value[i] == F & test.2$Value[i+1] == F){
        test.2$Replicate[i] <- NA
      } else {
        test.2$Replicate[i] <- r
      }
    }
    for(i in 2:(length(test.2$Value)-1)){
      if(test.2$Value[i] == F & test.2$Value[i+1] == T){
        test.2$Peaks[i] <- cut.off
        test.2$Replicate[i] <- test.2$Replicate[i+1]
      } else if(test.2$Value[i] == F & test.2$Value[i-1] == T){
        test.2$Peaks[i] <- cut.off
        test.2$Replicate[i] <- test.2$Replicate[i-1]
      } else {
        next
      }
    }
    test.3 <- na.omit(test.2)
    test.3 <- dplyr::mutate(test.3, "Area" = 0)
    test.3$diff <- c(0, diff(test.3$Peaks))
    peaks <- test.3$Peaks-cut.off
    diff <- c(0, diff(peaks))
    for(i in 1:(length(test.3$Peaks)-1)){
      if(test.3$Replicate[i] == test.3$Replicate[i+1]){
        time <- as.numeric(difftime(test.3$Time[i+1], test.3$Time[i]))
        if(test.3$Value[i] == F & test.3$Value[i+1] == T){
        test.3$Area[i] <- (time * diff[i+1])/2
        } else if(test.3$Value[i] == F & test.3$Value[i+1] == F){
          test.3$Area[i] <- (time * diff[i])/2
        } else if(test.3$Value[i] == T){
        test.3$Area[i] <- (peaks[i] * time) + ((time * diff[i+1])/2)
        }
      } else{
        next
      }
    }
    test.4 <- test.3 %>%
              dplyr::group_by(Sample, Replicate)%>%
              dplyr::summarize("Area_Under_Curve" = sum(Area), "Peak" = max(Peaks), "Time_Peak_Start" = min(Time), "Time_Peak_End" = max(Time)) %>%
              dplyr::mutate("Subsample" = c(1:length(Sample))) %>%
              tidyr::unite(col = "Sample", Sample, Subsample, sep = "/") %>%
              dplyr::arrange(Time_Peak_Start)
    test.5 <- cbind(File_Name = filelist[a], test.4)
    test.5 <- tidyr::separate(test.5, Sample, c("Sample", "Annotation", "Replicate"), sep = "/")
    test.5 <- dplyr::mutate(test.5, "Order_Run" = rep(1, n()), .before = 3)
    g <- 1
    for(i in 2:nrow(test.5)){
      if(test.5$Sample[i] == test.5$Sample[i-1]){
        test.5$Order_Run[i] <- g
      } else {
        g <- g + 1
        test.5$Order_Run[i] <- g
      }
    }
    output.raw <- rbind(output.raw, test.5)
  }
  output <- output.raw %>%
            dplyr::group_by(Sample, Order_Run) %>%
            dplyr::mutate(Replicate = c(1:n())) %>%
            tidyr::unite("Sample", Sample, Replicate, sep = ". ") %>%
            dplyr::select(-Annotation) %>%
            dplyr::arrange(Time_Peak_Start)
  preserve.order <- unique(output$File_Name)
  output <- mutate(output, "Timespan_(s)" = as.numeric(difftime(Time_Peak_End, Time_Peak_Start)))
  output.1 <- dplyr::filter(output, stringr::str_detect(toupper(Sample), "CURVE"))
  output.1 <- dplyr::mutate(output.1, "Standard" = numextract(Sample))
  curve <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(curve) <- c("File_Name", "Y.intercept", "Slope", "R.squared")
  for(i in 1:length(unique(output.1$File_Name))){
    asdf <- summary(lm(Standard ~ Area_Under_Curve, data = filter(output.1, File_Name == unique(output.1$File_Name)[i])))
    Y <- as.numeric(asdf$coefficients[1])
    M <- as.numeric(asdf$coefficients[2])
    R <- as.numeric(asdf$r.squared)
    curve[i,] <- c(unique(output.1$File_Name)[i], Y, M, R)
  }
  curve[, 2:4] <- sapply(curve[, 2:4], as.numeric)
  output.0 <- filter(output, !str_detect(toupper(Sample), "CURVE"))
  inwork <- output %>%
            dplyr::group_by(File_Name) %>%
            dplyr::summarize(n = n())
  inwork2 <- output.0 %>%
             dplyr::group_by(File_Name) %>%
             dplyr::summarize(n = n())
  curve.2 <- dplyr::mutate(inwork, n2 = inwork2$n, "curve" = (n != n2))
  yes.curve <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(yes.curve) <- c("File_Name", "Y.intercept", "Slope", "R.squared")
  for(i in 1:length(curve.2$File_Name)){
    for(j in 1:length(curve$File_Name)){
      if(curve.2$File_Name[i] == curve$File_Name[j] & curve.2$curve[i]==T){
        yes.curve[i,] <- curve[j,]
      } else if(curve.2$curve[i]==F){
        yes.curve[i,] <- c(curve.2$File_Name[i], rep(NA,3))
      }
    }
  }
  yes.curve$File_Name <- factor(yes.curve$File_Name, levels = preserve.order)
  yes.curve <- dplyr::arrange(yes.curve, File_Name)
  curve.3 <- tidyr::fill(yes.curve, "File_Name", "Y.intercept", "Slope", "R.squared", .direction = "down") #View(curve.3)
  curve.3[, 2:4] <- sapply(curve.3[, 2:4], as.numeric)
  stand <- output.1 %>%
           group_by(File_Name, Standard) %>%
           summarise(Mean = mean(Area_Under_Curve), std.dev = sd(Area_Under_Curve))%>%
           mutate(COV = std.dev/Mean)
  sum.stat <- left_join(stand, curve, by = "File_Name")
  output$AUC_ppm <- 0
  output$Peak_ppm <- 0
  for(i in 1:nrow(output)){
    for(j in 1:nrow(curve.3)){
      if(output$File_Name[i] == curve.3$File_Name[j]){
        output$AUC_ppm[i] <- curve.3$Slope[j] * output$Area_Under_Curve[i] + curve.3$Y.intercept[j]
        output$Peak_ppm[i] <- curve.3$Slope[j] * output$Peak[i] + curve.3$Y.intercept[j]
      }
    }
  }
  output <- output[, c(1, 2, 3, 6, 7, 8, 4, 5, 9, 10)]
  output <- separate(output, Sample, c("Sample", "Replicate"), sep = ". ")
  if(standard.sum == T){
    View(sum.stat)
  }
  return(output)
}

#' A Plotting function used with output from Li-Cor Peak finding function
#'
#' This function provides a plotting framework for exploring the data output from the LiCor_Peaks() function
#'
#' @param data output from the LiCor_Peaks() function
#' @param file the name of the file the user wishes to look at
#' @param sample the specific sample the user wishes to look at
#' @param curve a logical argument that states whether the user wishes to look at the standard curve or not
#' @keywords LiCor, Peak, CO2, Li-Cor
#' @export
#' @examples
#' setwd(path.package("licor.peaks"))
#' output <- Licor_Peaks()
#' Plot.Licor(output, curve = T)
#' Plot.Licor(output, file = "vn_clear_071621.txt", curve = T)
#' Plot.Licor(output, file = "vn_clear_07292021.txt", curve = T)
#' Plot.Licor(output, file = "vn_veg_07292021.txt", curve = T)
#' Plot.Licor(output, file = "vn_darkveg_071621.txt")
#' Plot.Licor(output, sample = "NN_DARKVEG")
#' Plot.Licor(output, file = "vn_darkveg_071621.txt", sample = "NC_DARKVEG")

Plot.Licor <- function(data, file = NULL, sample = NULL, std.curve = F){
  UNR <- function(){
    theme(text = element_text(color = "black", size = 15),
          plot.title = element_text(face = "bold", color = "darkblue", margin = margin(b = 15)),
          plot.subtitle = element_text(size = 10),
          axis.ticks = element_line(size = 1.5),
          axis.title = element_text(face = "bold", line = 2),
          axis.title.x = element_text(margin = margin(t = 10), color = "darkblue", size = 15),
          axis.title.y = element_text(margin = margin(r = 10), color = "darkblue", size = 15),
          axis.title.y.right = element_text(margin = margin(l = 15),color = "grey50", size = 15),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(margin = margin(t = 15)),
          axis.text.y = element_text(margin = margin(r = 10)),
          axis.line = element_line(colour = "black"),
          axis.ticks.length = unit(2, "mm"),
          plot.caption = element_text(color = "black"),
          plot.background = element_rect(fill = "white"),
          plot.margin = margin(t = 10, r = 50, b = 10, l = 10),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = "NA", color = "darkblue", size = 1.5),
          legend.background = element_rect(color = "black", fill = "white"),
          legend.key =  element_rect(fill = "white"),
          legend.text = element_text(color = "black"),
          legend.position = "bottom",
          strip.background = element_rect(color = "blue", fill = "grey75", size = 2),
          strip.text.y = element_text(size = 13, face = "bold"),
          panel.grid = element_blank())
  }
  numextract <- function(string){
    as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
  }
  ghost <- dplyr::filter(data, !str_detect(toupper(Sample), "CURVE") & !str_detect(toupper(Sample), "CHECK"))
  ghost$Replicate <- as.numeric(ghost$Replicate)
  if(is.null(sample) & is.null(file) & std.curve == F){
    rando.group <- sample(unique(data$File_Name), size = 1)
    samp.run <- sample(unique(dplyr::filter(ghost, File_Name == rando.group)$Order_Run), size = 1)
    verify <- dplyr::filter(ghost, File_Name == rando.group, Order_Run == samp.run)
    mean.ppm <- mean(verify$AUC_ppm)
    ggplot(verify, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify$File_Name, "\nSample:  ", tolower(verify$Sample), "\nOrder Run:  ", verify$Order_Run)) +
      geom_text(label = verify$Replicate, nudge_y = (mean.ppm/log(verify$AUC_ppm)*.40))+
      UNR()
  } else if(!is.null(file) & is.null(sample) & std.curve == F){
    filt <- dplyr::filter(ghost, File_Name == file)
    samp.run <- sample(unique(filt$Order_Run), size = 1)
    verify.1 <- dplyr::filter(filt, Order_Run == samp.run)
    mean.ppm <- mean(verify.1$AUC_ppm)
    ggplot(verify.1, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify.1$File_Name, "\nSample:  ", tolower(verify.1$Sample), "\nOrder Run:  ", verify.1$Order_Run)) +
      geom_text(label = verify.1$Replicate, nudge_y = (mean.ppm/log(verify.1$AUC_ppm)*.40))+
      UNR()
  } else if(is.null(file) & !is.null(sample) & std.curve == F){
    samp <- dplyr::filter(data, toupper(Sample) == toupper(sample))
    samp2 <- dplyr::filter(samp, File_Name == sample(unique(File_Name), 1))
    samp.run <- sample(unique(samp2$Order_Run), size = 1)
    verify.2 <- dplyr::filter(samp2, Order_Run == samp.run)
    mean.ppm <- mean(verify.2$AUC_ppm)
    ggplot(verify.2, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify.2$File_Name, "\nSample:  ", tolower(verify.2$Sample), "\nOrder Run:  ", verify.2$Order_Run)) +
      geom_text(label = verify.2$Replicate, nudge_y = (mean.ppm/log(verify.2$AUC_ppm)*.40))+
      UNR()
  } else if(!is.null(file) & !is.null(sample) & std.curve == F){
    verify.3 <- dplyr::filter(ghost, File_Name == file, toupper(Sample) == toupper(sample))
    mean.ppm <- mean(verify.3$AUC_ppm)
    ggplot(verify.3, aes(x = Time_Peak_Start, y = AUC_ppm)) +
      geom_col(color = "darkblue", fill = "dodgerblue")+
      ggtitle(paste0("File:  ", verify.3$File_Name, "\nSample:  ", tolower(verify.3$Sample), "\nOrder Run:  ", verify.3$Order_Run)) +
      geom_text(label = verify.3$Replicate, nudge_y = (mean.ppm/log(verify.3$AUC_ppm)*.40))+
      UNR()
  } else if(is.null(file) & is.null(sample) & std.curve == T){
    curv <- dplyr::filter(data, str_detect(toupper(Sample), "CURVE")) %>%
            dplyr::mutate(standard = numextract(Sample), .before = 3)
    check <- dplyr::filter(data, str_detect(toupper(Sample), "CHECK") )%>%
             dplyr::mutate(standard = numextract(Sample), .before = 3)
    asdf <- summary(lm(Area_Under_Curve ~ standard, data = curv))
    Y <- as.numeric(asdf$coefficients[1])
    M <- as.numeric(asdf$coefficients[2])
    R <- as.numeric(asdf$adj.r.squared)
    form <- data.frame(Y = Y, M = M, R = R)
    form2 <- paste0("Y = ", round(form$M, digits = 2), " * x", " + ", round(form$Y, digits = 2))
    r.squared <- paste0("adjusted R^2 = ", round(form$R, digits = 4))
    if(max(curv$standard) > max(check$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv, aes(x = standard, y = AUC_ppm),
                    method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = curv, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                   y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                   label = paste(form2, r.squared, sep = "\n"))) +
        ggtitle("Standard Curve for All Samples") +
        xlab("Standard ppm") +
        UNR()
    } else if(max(curv$standard) <= max(check$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv, aes(x = standard, y = AUC_ppm),
                    method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = check, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                    y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                    label = paste(form2, r.squared, sep = "\n"))) +
        ggtitle("Standard Curve for All Samples") +
        xlab("Standard ppm") +
        UNR()
    }
  } else if(!is.null(file) & is.null(sample) & std.curve == T){
    curv.2 <- dplyr::filter(data, str_detect(toupper(Sample), "CURVE"), File_Name == file) %>%
      dplyr::mutate(standard = numextract(Sample), .before = 3)
    check.2 <- dplyr::filter(data, str_detect(toupper(Sample), "CHECK"), File_Name == file)%>%
      dplyr::mutate(standard = numextract(Sample), .before = 3)
    asdf.2 <- summary(lm(Area_Under_Curve ~ standard, data = curv.2))
    Y.2 <- as.numeric(asdf.2$coefficients[1])
    M.2 <- as.numeric(asdf.2$coefficients[2])
    R.2 <- as.numeric(asdf.2$adj.r.squared)
    form.2 <- data.frame(Y = Y.2, M = M.2, R = R.2)
    form2.2 <- paste0("Y = ", round(form.2$M, digits = 2), " * x", " + ", round(form.2$Y, digits = 2))
    r.squared.2 <- paste0("adjusted R", "^", 2,  sep = "", " = ", round(form.2$R, digits = 4))
    if(max(curv.2$standard) > max(check.2$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv.2, aes(x = standard, y = AUC_ppm),
                    method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check.2, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv.2, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = curv.2, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                     y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                     label = paste(form2.2, r.squared.2, sep = "\n"))) +
        ggtitle(paste0("Standard Curve for ", curv.2$File_Name)) +
        xlab("Standard ppm") +
        UNR()
    } else if(max(curv.2$standard) <= max(check.2$standard)){
      ggplot()+
        geom_smooth(formula = y~x, data = curv.2, aes(x = standard, y = AUC_ppm),
                    method = lm, se = T, lwd = 1, color = "red", fullrange = T, fill = "dodgerblue") +
        geom_point(data = check.2, aes(x = standard, y = AUC_ppm, fill = "Check"), size = 4, shape = 21, alpha = .5) +
        geom_point(data = curv.2, aes(x = standard, y = AUC_ppm, fill = "Curve"), size = 3, shape = 24, alpha = .5) +
        scale_fill_manual(name = "Standard", values = c("grey70", "darkblue"), guide = guide_legend(override.aes = list(
          shape = c(21,24),
          size = c(3,3)))) +
        geom_text(data = check.2, aes(x = (((max(standard)-min(standard))*.25)+min(standard)),
                                      y = (((max(AUC_ppm)-min(AUC_ppm))*.75)+min(AUC_ppm)),
                                      label = paste(form2.2, r.squared.2, sep = "\n"))) +
        ggtitle(paste0("Standard Curve for ", curv.2$File_Name)) +
        xlab("Standard ppm") +
        UNR()
    }
  }
}



