# 3_helper.R
# Includes libraries, user-defined functions, and ggplot2 figure themes used to generate the Annual Report.
# See AR_helpfile.doc for information on file structure and workflow, column names and descriptions, etc.

# I. Libraries ----
if(!require("captioner"))   install.packages("packages/captioner_2.2.3.tar.gz") #numbering, ordering, & creating captions for tables and figures. #download the tar.gz from: https://cran.r-project.org/src/contrib/Archive/captioner/  
if(!require("data.table"))   install.packages("data.table") #caution: masks between, last in dplyr and dcast, melt in reshape2
if(!require("devtools"))  install.packages("devtools")
if(!require("getPass"))   install.packages("getPass")
if(!require("gridExtra"))   install.packages("gridExtra") #multipanelled figures
if(!require("kableExtra"))   install.packages("kableExtra") #markdown tables
if(!require("knitr"))   install.packages("knitr") #markdown
if(!require("lubridate"))  install.packages("lubridate")
#if(!require("maptools"))   install.packages("maptools")     # No longer on CRAN, replace with sf
if(!require("mosaic"))   install.packages("mosaic") #derivedFactor() and derivedVariable(), careful it masks some stats fxns like binomial.test() 
if(!require("patchwork"))  devtools::install_github("thomasp85/patchwork")
if(!require("reshape2"))   install.packages("reshape2") 
if(!require("RColorBrewer"))   install.packages("RColorBrewer") #color palettes in ggplot
# if(!require("rgeos"))   install.packages("rgeos") #deals with shapefiles      # No longer on CRAN, replace with sf
#if(!require("rgdal"))   install.packages("rgdal")  #deals with shapefiles, requires sp, will use proj.4 if installed  # No longer on CRAN, replace with sf
if(!require("scales"))  install.packages("scales")
if(!require("sf"))  install.packages("sf")
if(!require("tidyverse"))   install.packages("tidyverse") #dplyr, ggplot, tidyr, forcats
if(!require("viridis"))   install.packages("viridis") #color palettes in ggplot
if(!require("zoo"))  install.packages("zoo")

# II. Functions ----

permutation.fxn <- function(data.in, YN_var, gp_vec, n_rep){ 
# The purpose of this function is to answer the questions, are observed trips the same as unobserved trips?
  
  suppressMessages(require(lazyeval))
  
  #Relabel the permute field as YN
  data.in <- rename_(data.in, .dots = setNames(YN_var, 'YN'))
  
  #Assign the grouping variables to data:
  #Groups will always implicitly include TRIP_ID,
  #Identify the fields to group by
  gp_vec <- c(gp_vec, "TRIP_ID", "YN") 
  
  data.in <- group_by_(data.in, .dots = gp_vec)
  
  metrics.fxn <- 
    function(data){
      data.out <-
        data %>%
        summarize(vessel.length = max(LENGTH_OVERALL),
                  start.date = min(TRIP_TARGET_DATE),
                  end.date = max(LANDING_DATE),
                  areas = length(unique(REPORTING_AREA_CODE)),
                  species = length(unique(AGENCY_SPECIES_CODE[SOURCE_TABLE == "Y"])),
                  landed.catch = sum(as.numeric(WEIGHT_POSTED[SOURCE_TABLE == "Y"]))) %>%
        #days.fished has NAs!  this is bc kruzof and judy b act as cps.
        mutate(days.fished = as.numeric(end.date - start.date) + 1) %>% 
        select(-c(start.date, end.date))
      #Here I calculate N1 by first taking the weight of each species....
      N1.spp <- 
        data %>%
        filter(SOURCE_TABLE == "Y"
               & !is.na(AGENCY_SPECIES_CODE)) %>%
        group_by(TRIP_ID, AGENCY_SPECIES_CODE) %>%
        summarize(spp.wgt = sum(as.numeric(WEIGHT_POSTED[SOURCE_TABLE == "Y"]))
        )
      #and then the weight of the maximum by trip.....
      N1.max <- 
        N1.spp %>%
        group_by(TRIP_ID) %>%
        summarize(max.spp.wgt = max(spp.wgt))
      #..merge them and retain only the species record that is the max in the trip...         
      N1 <- merge(N1.spp, N1.max, all = TRUE)
      N1 <- filter(N1, spp.wgt == max.spp.wgt) %>% select(TRIP_ID, spp.wgt)
      rm(N1.max, N1.spp)
      #...and merge it with obs.effect.start
      data.out <- merge(data.out, N1, all=T)
      rm(N1)
      #...and ammend obs.effect.start to contain the new calculated metric.
      data.out <- 
        data.out %>% 
        mutate(N1 = round(spp.wgt/landed.catch,4)) %>% 
        select(-spp.wgt)
      
      return(data.out)
    }
  
  
  #SAMPLE
  resample.fxn <- 
    function(field, labs){
      new.labels = sample(labs)
      resample_diff = mean(field[new.labels == "Y"], na.rm = TRUE) - mean(field[new.labels == "N"], na.rm = TRUE)
      return(resample_diff)
    }
  
  #Assign the output to "(YN_Var).start"
  data.start <- metrics.fxn(data.in)
  
  #Melt and assign output to .melt
  data.melt <- melt(data.start,
                    id.vars = gp_vec, 
                    measure.vars = c("vessel.length", "days.fished", "species", "areas", "landed.catch", "N1")
  )
  
  #Identify grouping variables and assemble a common key
  annot_gp_vec <- names(data.melt)[!names(data.melt) %in% c("value", "TRIP_ID", "YN")]
  
  nx <- length(annot_gp_vec)
  
  for.key <- data.frame(data.melt[colnames(data.melt) %in% annot_gp_vec])
  
  KEY <- c(for.key, sep=".") # *FLAG*
  
  for.key$KEY <- do.call(paste, KEY)
  
  data.melt$KEY = do.call(paste, KEY)
  
  #Calculate actual differences
  
  #*FLAG* New version of dplyr retains underlying grouping structure and messes up Ntable object below.
  actuals.out <-
    group_by_(data.melt, .dots = annot_gp_vec) %>%
    summarize(Ytrips=length(unique(TRIP_ID[YN=='Y'])),
              Ntrips=length(unique(TRIP_ID[YN=='N'])),              
              obs.diff = mean(value[YN=="Y"], na.rm=TRUE) - mean(value[YN=="N"], na.rm=TRUE),
              Nmean = mean(value[YN=="N"], na.rm=TRUE)
    )

  Ntable <- actuals.out %>% ungroup() %>% select(STRATA, Ytrips, Ntrips) %>%  distinct() %>% data.frame()
  #Prepare for iterations
  summaryList <- unique(data.melt$KEY)
  
  CATEGORY_OUT <- vector("list",length(summaryList))
  temp_data <- vector("list",length(summaryList))
  imax <- length(CATEGORY_OUT)
  
  #Prepare progress bar
  pb <- txtProgressBar(min = 0, max = imax, style = 3)
  
  #run permutation 
  
  for (i in 1:length(summaryList)) {   
    temp_data[[i]] <- 
      data.melt %>% filter(KEY ==summaryList[i])
    
    temp <- data.table(perm_result = replicate(n_rep, 
                                               resample.fxn(temp_data[[i]]$value, 
                                                            temp_data[[i]]$YN)
    )
    )
    temp$KEY <- unique(temp_data[[i]]$KEY)
    
    CATEGORY_OUT[[i]] <- temp
    
    setTxtProgressBar(pb, i)  
    
  }
  
  #create more useful strata information by breaking key apart 
  permutation.out <- do.call("rbind", CATEGORY_OUT)  
  permutation.out <- merge(unique(for.key), permutation.out, all = TRUE)
  permutation.out <- select(permutation.out, -KEY)
  
  permutation.out <- merge(permutation.out, actuals.out)
  permutation.out$test.stat <- ifelse(abs(permutation.out$perm_result) >= abs(permutation.out$obs.diff), 1, 0)                                
  #Now make use of our percents to convert the results to relative terms
  permutation.out$perm_result_pct <- round((permutation.out$perm_result / permutation.out$Nmean)*100, 3)
  permutation.out$obs.diff_pct <-round((permutation.out$obs.diff / permutation.out$Nmean)*100, 3)
  
  #relabel for outputs
  permutation.out$variable <- 
    factor(permutation.out$variable, 
           levels=c("areas", "days.fished", "vessel.length", "species", "N1", "landed.catch"), 
           labels=c("NMFS areas", "Days fished", "Vessel length (ft)", "Species landed", "pMax species", "Landed catch (t)"))
  
  summary <- 
    data.frame(
      group_by_(permutation.out, .dots = annot_gp_vec) %>%
        summarize(obs.diff = round(unique(obs.diff),3),
                  mean.perm.diff = round(mean(na.omit(perm_result)),3),
                  obs.diff.pct = round(unique(obs.diff_pct),3),
                  mean.perm.diff.pct = round(mean(na.omit(perm_result_pct)),3),
                  iter = length(variable),
                  p = sum(test.stat/iter)
        )
    )
  
  tmp.txt <- paste(as.character(annot_gp_vec[annot_gp_vec != "variable"]), collapse = "+")
  form_you_la <- paste(tmp.txt, "variable", sep = "~")
  
  ptable <- dcast(summary, form_you_la, value.var = "p")
  ptable$Metric <- "p-value"
  
  ODtable <- dcast(summary, form_you_la, value.var = "obs.diff")
  ODtable$Metric <- "Observed difference"
  
  ODpct<-dcast(summary, form_you_la, value.var="obs.diff.pct")
  ODpct$Metric <- "OD (%)"
  
  summary.table <- 
    rbind(ODtable, ODpct, ptable) %>% 
    dplyr::arrange(STRATA) %>% 
    #Use the backticks because cols have spaces
    # FLAG (PG) Edit to match capitalization
    # dplyr::select(`Strata`=STRATA, Metric, `NMFS Areas`,
    #               `Days Fished`, `Vessel Length`,
    #               `Species Landed`, `pMax Species`,
    #               `Landed Catch`)
    dplyr::select(`Strata`=STRATA, Metric, `NMFS areas`,
                  `Days fished`, `Vessel length (ft)`,
                  `Species landed`, `pMax species`,
                  `Landed catch (t)`)
    
  
  detach(package:lazyeval)
  
  return(list(permutation.out, data.melt, summary, summary.table, Ntable))
}

# b. my_excel - used to make the list of statistically significant stat areas in the upper-lefthand corner of the probability maps

my_excel <- function(x,y) {
   for(i in 2:length(x)) x[i] = x[i-1] + y
  return(x)
}

# c. highlight_significant - used to highlight significant stat areas in the proportion maps

highlight_significant = function(significant)
{
  library(choroplethrMaps)
  #data(sub, package="choroplethrMaps", envir=environment())
  df = nmfs_prob[nmfs_prob$proportion %in% significant, ]
  geom_polygon(data=df, aes(long, lat, group = group), color = "grey40", fill = NA, size = .7, alpha=.5)
}

#d. wordlist - used for in-text lists of unknown length

wordlist <- function(w, oxford=T) {
  if(length(w)==1) return(w);
  if(length(w)==2) return(paste(w[1],"and",w[2]));
  paste0( paste(w[-length(w)], collapse=", "), 
          ifelse(oxford,",","")," and ", w[length(w)] )
}

#e. fig_nums, tbl_nums, and appendix_nums fxns created fromm captioner() fxn in 'captioner' library,
#which I've tweaked below (Changed separator from a colon to period) - these fxns are used for autonumbering
#figs and tables in text and creating captions.

captioner <- function (prefix = "Figure", auto_space = TRUE, levels = 1, 
          type = NULL, infix = ".") {
  check_class(prefix, "character")
  check_class(auto_space, "logical")
  check_class(levels, "numeric")
  check_class(infix, "character")
  if (is.null(type)) {
    type <- c(rep("n", times = levels))
  }
  else if (length(type) < levels) {
    type[(length(type) + 1):levels] <- "n"
  }
  else if (length(type) > levels) {
    type <- type[1:levels]
  }
  if (!all(type %in% c("n", "c", "C"))) {
    stop("Invalid 'type' value used.  Expecting 'n', 'c', or 'C'.")
  }
  if (auto_space) {
    prefix <- paste0(prefix, " ")   # Geoff changed paste() to paste0(), otherwise two-spaces get added!
  }
  force(levels)
  force(prefix)
  force(infix)
  OBJECTS <- list(name = NULL, caption = NULL, number = list(list()))
  OBJECTS$number[[1]][which(type == "n")] <- 1
  OBJECTS$number[[1]][which(type == "c")] <- "a"
  OBJECTS$number[[1]][which(type == "C")] <- "A"
  function(name, caption = "", display = "full", level = FALSE, 
           cite = FALSE, num = FALSE) {
    if (level > levels) {
      stop("Level too large.")
    }
    objects <- OBJECTS
    if (any(objects$name == name)) {
      obj_ind <- match(name, objects$name)
      if (objects$caption[obj_ind] == "") {
        objects$caption[obj_ind] <- caption
      }
      else {
        caption <- objects$caption[obj_ind]
      }
    }
    else {
      obj_ind <- length(objects$name) + 1
      if (length(objects$number) == length(objects$name)) {
        if (level) {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind - 
                                                                   1]], level)
        }
        else {
          objects$number[[obj_ind]] <- increment(objects$number[[obj_ind - 
                                                                   1]], levels)
        }
      }
      objects$name[obj_ind] <- name
      objects$caption[obj_ind] <- caption
    }
    assign("OBJECTS", objects, envir = parent.env(environment()))
    obj_num <- paste(objects$number[[obj_ind]], collapse = infix)
    if (cite) {
      .Deprecated(new = "display", old = "cite")
      return(paste0(prefix, obj_num))
    }
    if (num) {
      .Deprecated(new = "display", old = "num")
      return(obj_num)
    }
    if (display == FALSE) {
      return(invisible())
    }
    #FLAG: Jane changed ": " to ". "
    else if (display == "full" || display == "f") {
      return(paste0(prefix, obj_num, ". \\-- ", caption))   # Geoff added double-hyphen, but requires '\\' or else it turns into em-dash.
    }
    else if (display == "cite" || display == "c") {
      return(paste0(prefix, obj_num))
    }
    else if (display == "num" || display == "n") {
      return(obj_num)
    }
    else {
      warning("Invalid display mode used.  Caption was still saved.")
      return(invisible())
    }
  }
}

fig_nums <- captioner(prefix = "Figure")

tbl_nums <- captioner(prefix = "Table")

appendix_tbl_nums <- captioner(prefix = "Table") #Numbers tables in the appendix.

appendix_fig_nums <- captioner(prefix = "Figure") #Numbers figures in the appendix.

#f.  interpret_trip_metrics - used to create the interpretive text for the
#permutation test results on six trip metrics (number of areas fished, vessel
#length, days fished, etc.). Creates character strings like "were 13.6% shorter
#in duration" that can be pasted together in generalized paragraph text using
# wordlist() (see function #d) within knit_expand()

interpret_trip_metrics <- function(dat) {
  dat.out <- dat %>% 
    mutate(obs.diff.pct = round(obs.diff.pct,1),
           obs.diff = round(obs.diff,2)) %>% 
    #Filter out super small effect sizes
    #filter(obs.diff > 0.0 & obs.diff.pct > 0.0) %>% 
    # filter(!c(obs.diff == 0.0 | obs.diff.pct == 0.0) ) %>% 
    mutate(interpretation = ifelse(variable %in% "NMFS areas",
                                   paste0("occurred in ", abs(obs.diff.pct), "% (", trimws(format(abs(obs.diff), nsmall=2, digits=2)), ") ",  
                                          ifelse(obs.diff.pct<0, "fewer", "more"), " areas"), 
                                   ifelse(variable %in% "Days fished",
                                          paste0("were ",  abs(obs.diff.pct), "% (", trimws(format(abs(obs.diff), nsmall=2, digits=2)), " days) ", 
                                                 ifelse(obs.diff.pct<0, "shorter", "longer"), " in duration"),
                                          ifelse(variable %in% "Vessel length (ft)",
                                                 paste0("occurred on vessels ", abs(obs.diff.pct), "% (", trimws(format(abs(obs.diff), nsmall=2, digits=2)), " ft) ", 
                                                        ifelse(obs.diff.pct<0, "shorter", "longer"), " in length"),
                                                 ifelse(variable %in% "Species landed",
                                                        paste0("landed ", abs(obs.diff.pct), "% (", trimws(format(abs(obs.diff), nsmall=2, digits=2)), ") ",
                                                               ifelse(obs.diff.pct<0, "fewer", "more"), " species"),
                                                        ifelse(variable %in% "pMax species",
                                                               paste0("landed catch that was ", abs(obs.diff.pct), "% ", 
                                                                      #This one is counterintuitive, but low values mean more diverse in the Hill's index
                                                                      ifelse(obs.diff.pct<0, "more", "less"), " diverse"),
                                                               ifelse(variable %in% "Landed catch (t)",
                                                                      paste0("landed catch that weighed ", abs(obs.diff.pct), "% (", trimws(format(abs(obs.diff), nsmall=2, digits=2)), " metric tons) ",
                                                                             ifelse(obs.diff.pct<0, "less", "more")),"NA")))))))
  return(dat.out)
}

#g. numbers2words - in order to follow AFSC style guide. Numbers < 10 should spelled out. -_-

#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

# h. channel_function - Function to facilitate connection to AFSC/FMA tables and views 
#                       from within the AFSC/FMA (Seattle) and at the AKRO-SF (Juneau)

channel.fxn <- function(location, db="AFSC"){
  
  if(!location %in% c("SEATTLE", "JUNEAU")){
    cat('Location not recognized (did you use all caps?)')
  }
  
  else{
    
    if(location == 'SEATTLE'){
      if(!require("odbc"))   install.packages("odbc", repos='http://cran.us.r-project.org')
      channel <- dbConnect(odbc::odbc(),"AFSC",
                           UID    = rstudioapi::askForPassword("Database user"),
                           PWD    = rstudioapi::askForPassword("Database password"))
    }
    
    if(location == 'JUNEAU' & db == 'AFSC'){
      if(!require("ROracle"))   install.packages("ROracle", repos='http://cran.us.r-project.org')
      channel <- dbConnect(drv = dbDriver('Oracle'),
                           username = rstudioapi::askForPassword("Database user"),
                           password = rstudioapi::askForPassword("Database password"),
                           dbname = "(DESCRIPTION =(ADDRESS = (PROTOCOL = TCP)(HOST = raja.afsc.noaa.gov)(PORT = 1521))(CONNECT_DATA =(SERVER = DEDICATED)(SID = afscp1)))")
    }
    
    if(location == 'JUNEAU' & db == 'AKRO'){
      if(!require("ROracle"))   install.packages("ROracle", repos='http://cran.us.r-project.org')
      channel <- dbConnect(drv = dbDriver('Oracle'),
                           username = rstudioapi::askForPassword("Database user"),
                           password = rstudioapi::askForPassword("Database password"),
                           dbname = "(DESCRIPTION =(ADDRESS = (PROTOCOL = TCP)(HOST = akr-j50.nmfs.local)(PORT = 2040))(CONNECT_DATA =(SERVER = DEDICATED)(service_name = AKR1)))")
    }
    
    cat("Use Syntax:\n<SQL QUERY OBJECT NAME> <- paste(<your sql query>) \n<YOUR OBJECT FROM QUERY> <- dbGetQuery(channel, <QUERY OBJECT NAME>)")
    return(channel)
  }
}

# III. Figure themes ----

# a. Publications ----

fig.theme <- 
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text.x = element_text(size = 14),
        axis.line = element_line(size = 0.5, color = "black"),
        legend.position = "bottom")

# b. Maps ----

map_theme <- theme(
  plot.title = element_text(size = rel(1.5)),
  axis.line=element_blank(),
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  panel.background=element_blank(),
  panel.border=element_blank(),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  plot.background=element_blank())

# d. Barplot theme

bar_theme <- theme_bw() + 
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1),
        panel.background = element_blank())
