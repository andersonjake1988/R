##############################################################
# Package Loading
##############################################################
library(mosaic)
library(tidyverse)

##############################################################
# Completed Formatting Functions
##############################################################
# Function for installing/loading packages
load_packages <- function(packages = c("tidyverse", "lubridate", "mosaic", "scales", "kableExtra")){
  is.installed <- packages %in% .packages(all.available = TRUE)
  lapply(packages[!is.installed], install.packages)
  invisible(lapply(packages, library, character.only = TRUE, verbose = FALSE))
}

# function to make a list of a given length with generic counting labels (i.e. cohort$step1) or custom labeled steps (i.e. cohort$step1ai)
create_step_list <- function(n = length(names), names = seq(n), prefix = "step"){
  paste0(prefix, names) %>%
    factor(levels = paste0(prefix, names)) %>%
    split(rep(NA, n), .)
}

## function to make a table that automatically indents subcategories (i.e. 1, 1a, 1ai)
step_kable <- function(output_table, top_color = "lightblue", top_text_color = "black", kable_theme = "classic", 
                       kable_style = "hover", collapse_align = "middle", collapse = FALSE, title = NA, escape = FALSE){
  assertthat::assert_that(kable_theme %in% c("classic", "classic_2", "minimal", "material", "material_dark"), 
                          msg = 'kable_theme must be one the following: "classic", "classic_2", "minimal", "material", "material_dark"')
  assertthat::assert_that(all(kable_style %in% c("striped", "hover", "basic")), 
                          msg = 'kable_style must be one or more of the following: "striped", "hover", "basic"')
  assertthat::assert_that(all(collapse_align %in% c("middle", "top", "bottom")), 
                          msg = 'collapse_align must be one or more of the following: "middle", "top", "bottom"')
  step_breakdown <- function(steps){
    a <- str_extract(steps, "[a-zA-Z].*|\\*|\\+|\\-")
    rom <- str_extract(a, "i+.*|v+.*|x+.*")
    sym1 <- str_extract(rom, "\\-+.*|\\++.*|\\*+.*")
    sym2 <- str_extract(a, "\\-+.*|\\++.*|\\*+.*")
    b <- str_extract(steps, "[0-9].*|\\s") 
    for(i in seq(b)){
      if(!is.na(sym1[i])){
        b[i] <- sym1[i]
      } else if(!is.na(sym2[i])){
        b[i] <- sym2[i]
      } else if(!is.na(rom[i])){
        b[i] <- rom[i]
      } else if(!is.na(a[i])){
        b[i] <- a[i]
      }
    }
    return(b)
  }
  indent_pos <- function(step_breakdown){
    a <- str_extract(step_breakdown, "[a-zA-Z].*|\\-+.*|\\++.*|\\*+.*")
    rom <- str_extract(a, "i+.*|v+.*|x+.*")
    sym <- str_extract(rom, "\\-+.*|\\++.*|\\*+.*")
    b <- str_extract(step_breakdown, "[0-9]|\\s") 
    if(all(is.na(rom))){
      sort(c(which(!is.na(a)), which(!is.na(sym))))
    } else if(all(is.na(a))){
      sort(c(which(!is.na(rom)), which(!is.na(sym))))
    } else {
      sort(c(which(!is.na(a)), which(!is.na(rom))))
    }
  }
  if("Step" %in% colnames(output_table)){
    output_table$Step <- step_breakdown(output_table$Step)
    indent <- indent_pos(output_table$Step)
    theme <- match.fun(paste0("kable_", kable_theme))
    kable(output_table, align = ifelse(all(is.na(indent)), "clc", "llc"), caption = title, escape = escape) %>%
      add_indent(indent) %>%
      column_spec(1, width = "1.5cm", border_left = TRUE) %>%
      column_spec(2, border_left = TRUE, border_right = TRUE) %>%
      column_spec(3, width = "1.75cm", border_right = TRUE) %>%
      theme(kable_style, 
            row_label_position = "c") %>%
      row_spec(0, align = "c", color = top_text_color, background = top_color, bold = TRUE, font_size = 16) %>%
      row_spec(1:nrow(output_table), extra_css = "border-bottom: 1px solid", align = "middle") %>%
      {if(collapse == TRUE) collapse_rows(., columns = 1, valign = collapse_align) else .}  
  } else {
    theme <- match.fun(paste0("kable_", kable_theme))
    kable(output_table, align = "c", caption = title, , escape = escape) %>%
      theme(kable_style, 
            row_label_position = "c") %>%
      column_spec(1:length(output_table), border_right = TRUE, border_left = TRUE) %>%
      row_spec(0, align = "c", color = top_text_color, background = top_color, bold = TRUE, font_size = 16) %>%
      row_spec(1:nrow(output_table), extra_css = "border-bottom: 1px solid", align = "c") %>%
      {if(collapse == TRUE) collapse_rows(., columns = 1, valign = collapse_align) else .}   
  } 
}

## function to label and glimpse all tables in a list
glimpse_tbls <- function(list){
  for(i in seq(list)){
    print(names(list[i]))
    glimpse(list[[i]])
    cat('\n')
  }
}

## function to derive all combinations of strings in a vector of strings from a single to length n of combinations
all_combinations <- function(x, sep = ",", output = "vec", print = FALSE){
  assertthat::assert_that(output %in% c("vec", "df"),
                          msg = "output must be one of the following: 'vec' for vector, or 'df' for dataframe")
  combo <- seq(length(x))
  size <- 0
  for(i in combo){
    size <- sum(size, choose(length(x),i)) 
  }
  a <- vector()
  if(print){print(glue::glue("Total of {size} combinations:"))}
  for(i in combo){
    test <- as.list(as.data.frame(combn(x, i))) %>%
      map(~str_sort(.)) %>%
      map_chr(~str_flatten(., collapse = sep)) %>%
      unname()
    a <- c(a, test)
  }
  if(output == "vec"){
    return(a)
  } else if(output == "df"){
    a <- as.data.frame(a)
    names(a) <- NULL
    format(a, justify = "left")
  }
}

## function to search for a term in the column names of a list of tables 
var_search <- function(tbl, term){
  a <- tbl %>%
    map(., colnames) %>%
    unlist() %>%
    as.data.frame(row.names = names(.)) 
  names(a) <- "variable"
  b <- a %>%
    filter(grepl(term, variable, ignore.case = TRUE))
  return(b)
}

## Function used to fix copy and pasting operations to generate various outputs
paste_fixer <- function(x, split_by = "\n", output = "vector", code_style = "newline", num_add = TRUE, num_removal = "first_only",
                        comment_spacing = 2, comment_border = TRUE, border_length = 100, steps = NULL, str_wrap = 120){
  assertthat::assert_that(output %in% c("vector", "string", "code", "comment", "regex"),
                          msg = "output must be one of the following: 'vector', 'string', 'code', 'regex', or 'comment'")
  assertthat::assert_that(code_style %in% c("comma", "newline"),
                          msg = "code_style must be one of the following: 'comma', 'newline'")
  assertthat::assert_that(num_removal %in% c(TRUE, FALSE, "first_only"),
                          msg = "num_removal must be TRUE, FALSE, or 'first_only'")
  if(num_removal == TRUE){
    x <- gsub('[[:digit:]].', '', x)
  }
  
  
  if(output == "vector"){
    x %>%
      str_split(split_by) %>%
      map(str_squish) %>%
      unlist() %>%
      {if(num_removal == "first_only") str_replace_all(., "^\\d+\\s", "") else .}
  } else if (output == "string"){
    a <- x %>%
      str_split(split_by) %>%
      map(str_squish) %>%
      unlist() %>%
      {if(num_removal == "first_only") str_replace_all(., "^\\d+\\s", "") else .} %>%
      {if(num_add) paste0(seq(.), ". ", .) else .} %>%
      {if(!is.null(steps)) paste0(steps, ". ", .) else .}
    cat('\n')
    cat(a, sep = "\n")
    clipr::write_clip(a)
    cat('\n\n')
    cat('copied to clipboard')
  } else if (output == "comment"){
    b <- x %>%
      str_split(split_by) %>%
      map(str_squish) %>%
      unlist() %>%
      {if(num_removal == "first_only") str_replace_all(., "^\\d+\\s", "") else .} %>%
      {if(comment_border == TRUE & is.null(steps)) paste0(
        str_flatten(rep('#', border_length)), 
        '\n', 
        {if(num_add == TRUE) 
          paste(paste0("# ", seq(length(.)), "."), .) 
          else 
            paste0("# ", .)}, 
        '\n', 
        str_flatten(rep('#', border_length))) 
        else .} %>%
      {if(comment_border == TRUE & !is.null(steps)) paste0(
        str_flatten(rep('#', border_length)), 
        '\n', 
        paste(paste0("# ", steps, "."), .),
        '\n', 
        str_flatten(rep('#', border_length))) 
        else .} %>%
      {if(comment_border != TRUE & is.null(steps)) {if(num_add == TRUE) paste(paste0("# ", seq(length(.)), "."), .) 
        else paste0("# ", .)} 
        else .} %>%
      {if(comment_border != TRUE & !is.null(steps)) paste(paste0("# ", steps, "."), .) 
        else .} 
    cat('\n')
    cat(b, sep = str_flatten(rep('\n', comment_spacing + 1)))
    clipr::write_clip(paste(b, str_flatten(rep("\n ", comment_spacing))))
    cat('\n\n')
    cat('copied to clipboard')
  } else if(output == 'code' & code_style == "comma"){
    c <- suppressWarnings(x %>%
                            str_split(split_by) %>%
                            str_squish() %>%
                            str_replace_all("\"\\s", "\"") %>%
                            str_replace_all("\\s\"", "\"") %>%
                            str_replace_all("\",\"", "\", \"") %>%
                            str_replace_all('\\\\t', '') %>%
                            {if(num_removal == "first_only") str_replace_all(., "\"\\d+\\s", "\"") else .} %>%
                            strwrap(str_wrap))
    cat('\n')
    cat(c, sep = "\n")
    clipr::write_clip(c)
    cat('\n')
    cat('copied to clipboard')
  } else if(output == 'code' & code_style == "newline"){
    d <- suppressWarnings(x %>%
                            str_split(split_by) %>%
                            str_squish() %>%
                            str_replace_all("\"\\s", "\"") %>%
                            str_replace_all("\\s\"", "\"") %>%
                            str_replace_all("\",\"", "\",\n\"") %>%
                            str_replace_all('\\\\t', '') %>%
                            {if(num_removal == "first_only") str_replace_all(., "\"\\d+\\s", "\"") else .})
    cat('\n')
    cat(d)
    clipr::write_clip(d)
    cat('\n\n')
    cat('copied to clipboard')
  } else if (output == "regex"){
    e <- x %>%
      str_split(split_by) %>%
      map(str_squish) %>%
      unlist() %>%
      {if(num_removal == "first_only") str_replace_all(., "^\\d+\\s", "") else .} %>%
      str_flatten(collapse = "|") %>%
      paste0('\"', ., '\"')
    cat('\n')
    cat(e)
    clipr::write_clip(e)
    cat('\n\n')
    cat('copied to clipboard')
  }
}

## run a summary and convert to data frame
summary_to_df <- function(x){
  as_tibble(as.list(summary(x)))
}

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

# Class changing function
class_change <- function(data, var, class_fun){
  comb <- var[str_detect(var, ':')]
  var_sel <- var[!str_detect(var, ':')]
  for(i in comb){
    var0 <- unlist(str_split(i, ':'))
    var_sel <- c(var_sel, names(select(data, var0[1]:var0[2])))
  }
  data[var_sel] <- lapply(data[var_sel], class_fun)
  return(data)
}

# Filter rows containing NA's
find_na <- function(data, var = names(data), match = 'any'){
  if(match == 'any'){
    b <- data %>% filter_at(vars(var), any_vars(is.na(.)))
    return(b)
  } else if(match == 'all'){
    b <- data %>% filter_at(vars(var), all_vars(is.na(.)))
    return(b)
  }
}

# Equation Split sort and remove copies
EQ_spl_rmc <- function(data, eq_col){
  a <- str_split(data[[eq_col]], ' ~ ')
  b <- lapply(a, str_sort)
  c <- cbind(data, test2 = unlist(lapply(b, str_c, collapse = ' ')))
  d <- c[!duplicated(c$test2),]
  return(select(d, -test2))
}

# quiet function print calls
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

# ggplot aesthetics
UNR <- function(){
  ggplot2::theme(text = ggplot2::element_text(color = "black", size = 15),
                 plot.title = ggplot2::element_text(face = "bold", color = "darkblue", margin = ggplot2::margin(b = 15)),
                 plot.subtitle = ggplot2::element_text(size = 10),
                 axis.ticks = ggplot2::element_line(linewidth = 1.5),
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
                 panel.border = ggplot2::element_rect(fill = "NA", color = "darkblue", linewidth = 1.5),
                 legend.background = ggplot2::element_rect(color = "black", fill = "white"),
                 legend.key =  ggplot2::element_rect(fill = "white"),
                 legend.text = ggplot2::element_text(color = "black"),
                 legend.position = "bottom",
                 strip.background = ggplot2::element_rect(color = "blue", fill = "grey75", linewidth = 2),
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
  geom_point(size = 2)+
  geom_smooth(method = lm, se = F) +
  lm_text_pos(data = mtcars, x = 'mpg', y = 'disp', xpos = 27, size = 2.5)+
  UNR()
