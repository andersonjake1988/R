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
#' @param cut.off Cut off value used to define when the peak should start and end. Defaults to 3.
#' @keywords LiCor, Peak, CO2, Li-Cor
#' @export
#' @examples
#' setwd(path.package("LiCor.Peaks"))
#' output <- Licor_Peak()

Licor_Peak <- function(cut.off = 3){
  packages <- "tidyverse"
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages], quiet = T)
  }
  invisible(lapply(packages, require, character.only = TRUE))
  Peaks <- function(x){
    output <- vector()
    for(i in 1:length(x)){
      ifelse(x[i] >= cut.off, output[i] <- x[i], NA)
    }
    output
  }
  filelist <- list.files(pattern = c(".txt", ".TXT"))
  output.raw <- data.frame()
  print("Looping through Folder:")
  progress_bar <- txtProgressBar(min = 0, max = length(filelist), style = 3)
  for(a in 1:length(filelist)){
    setTxtProgressBar(progress_bar, a)
    b <- read.table(file(filelist[a]), header = T, sep = "\t", fill = T, strip.white = T, check.names = F)
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
    file.annot <- filter(data.3, is.na(CO2)) %>%
                  group_by(Sample)%>%
                  mutate("temp" = c(1:length(Sample))) %>%
                  mutate("Sample" = paste0(Sample, "/", temp))
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
              dplyr::summarize("Peaks" = Peaks(CO2)) %>%
              dplyr::arrange(Sample) %>%
              dplyr::mutate("Value" = !is.na(Peaks), "Replicate" = NA)
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
      test.3 <- na.omit(test.2)
      test.4 <- test.3 %>%
                dplyr::group_by(Sample, Replicate)%>%
                dplyr::summarize("Sum_Peak_Range" = sum(Peaks), "Peak" = max(Peaks)) %>%
                dplyr::mutate("Subsample" = c(1:length(Sample))) %>%
                tidyr::unite(col = "Sample", Sample, Subsample, sep = "/")
      test.5 <- cbind(File_Name = filelist[a], test.4)
      test.5 <- tidyr::separate(test.5, Sample, c("Sample", "Annotation", "Replicate"), sep = "/")
      test.5 <- mutate(test.5, "Order_Run" = rep(1, n()), .before = 3)
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
  output.final <- output.raw %>%
                  group_by(Sample, Order_Run) %>%
                  mutate(Replicate = c(1:n())) %>%
                  unite("Sample", Sample, Replicate, sep = "-") %>%
                  select(-Annotation)
  output.final
}




# setwd("~/R/Li-Cor Peak Data")
# output <- Licor_Peak()


ggplot(output, aes(x = Sum_Peak_Range, y = Peak))+
  annotation_custom(grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(size = 3, alpha = .7, shape = 21, fill = "darkred") +
  Tropical_Sunset()


