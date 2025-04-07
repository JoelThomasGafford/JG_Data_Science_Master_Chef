



rm(list=ls(all=TRUE))

# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("rvest")
# install.packages("tibble")
# install.packages("matrixStats")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(reshape2)
library(rvest)
library(tibble)

setwd("G:/JG_Projects/Programming/RStudio/MC_MCJ_Stats")




# ---- Function: Result to Number ----
Result.To.Number <- function(n){
  ifelse(n == "ELIM" | n == "WDR", -2,
  ifelse(n == "LOW", -1,
  ifelse(n == "OUT", 0,
  ifelse(n == "IMM" | n == "IN" | n == "PT" | n == "NPT", 1,
  ifelse(n == "HIGH" | n == "RET", 2,
  ifelse(n == "WIN", 3,
  ifelse(n == "RUNNER-UP" | n == "RUNNERS-UP", -2,
  ifelse(n == "WINNER", 3, NA))))))))
}




# ---- Function: Replace middle NAs with zero but leave NAs at the end ----
Mid.NA.To.Zero <- function(Vec) {
  for (i in 1:length(Vec)) {
    # Check if the element is NA
    if (is.na(Vec[i])) {
      # Check if there are any numbers to the right
      if (any(!is.na(Vec[(i+1):length(Vec)]))) {
        Vec[i] <- 0
      }
    }
  }
  return(Vec)
}




# ---- Main Read Show function ----
Read.Show <- function(Link, Show, Location, Season, Year, Gender) {
  Show_Link <- Link
  Show_Page = read_html(Show_Link)
  # Eliminations
  Show_Elim = Show_Page %>% html_nodes("table") %>% .[3] %>%
    html_table() %>% .[[1]]
  # Set column names to the first row
  colnames(Show_Elim) <- Show_Elim[1, ]
  # Remove the first row
  Show_Elim <- Show_Elim[-1, ]
  # Zero-pad places column
  Show_Elim$Place <- sprintf("%02d", as.numeric(Show_Elim$Place))
  # Rename columns to avoid duplicate names
  colnames(Show_Elim)[3:ncol(Show_Elim)] <- paste(3:(ncol(Show_Elim))-2)
  # Rename "Contestant" to "Cont_First"
  Show_Elim <- Show_Elim %>% rename(Cont_First = Contestant)
  # Give unique IDs to each contestant
  Cont_ID <- paste(Show, Location, Season, Show_Elim$Cont_First, sep = "_")
  # add_column for Cont_ID
  Show_Elim <- add_column(Show_Elim, Cont_ID, .before = "Place")
  # Drop columns that contain entirely NA or empty fields
  Show_Elim <- Show_Elim %>%
    select_if(~!all(is.na(.) | . == ""))
  #View(Show_Elim)
  
  
  
  
  # Contestants
  Show_Cont = Show_Page %>% html_nodes("table") %>% .[2] %>%
    html_table() %>% .[[1]]
  # Remove bracketed numbers and preceding whitespace
  Cont_Full_Clean <- gsub("\\s*\\[\\d+\\]", "", Show_Cont$Contestant)
  Show_Cont <- add_column(Show_Cont, Cont_Full_Clean, .after = "Contestant")
  # First and Last names
  Cont_Last <- sapply(strsplit(Show_Cont$Cont_Full_Clean, "\\s+"), tail, n = 1)
  Cont_First <- Show_Elim$Cont_First
  Show_Cont <- add_column(Show_Cont, Cont_First, .after = "Contestant")
  Show_Cont <- add_column(Show_Cont, Cont_Last, .after = "Cont_First")
  # Extract Nick Name
  Cont_Nick <- noquote(str_extract(Show_Cont$Contestant, '"(.*?)"')) %>%
    # Remove extra backslashes and quotes
    gsub('[\\\\"]', '', .)
  # If there is no Nick Name, use First Name
  Show_Cont$Cont_First <- ifelse(is.na(Cont_Nick), Cont_First, Cont_Nick)
  # Remove Contestant field (it is replaced by "Cont_Full_Clean")
  Show_Cont$Contestant <- NULL
  # Give unique IDs to each contestant
  Cont_ID <- paste(Show, Location, Season, Show_Cont$Cont_First, sep = "_")
  Show_Cont <- add_column(Show_Cont, Cont_ID, .before = "Cont_First")
  # Place
  Show_Cont <- add_column(Show_Cont, Show_Elim[2], .before = "Cont_First")
  # Gender
  Gender <- Gender
  Show_Cont <- add_column(Show_Cont, Gender, .after = "Age")
  # Season
  Season <- rep(Season, (nrow(Show_Cont)))
  Show_Cont <- add_column(Show_Cont, Season, .before = "Place")
  # Location
  Location <- rep(Location, (nrow(Show_Cont)))
  Show_Cont <- add_column(Show_Cont, Location, .before = "Season")
  # Show
  Show <- rep(Show, (nrow(Show_Cont)))
  Show_Cont <- add_column(Show_Cont, Show, .before = "Location")
  # Result
  Result <- sapply(strsplit(Show_Cont$Status, "\\s+"), head, n = 1)
  Show_Cont <- add_column(Show_Cont, Result, .after = "Status")
  # "Runner-Up" standardize
  Show_Cont$Result <- sub("Runners[- ]?up|Runner[- ]up", "Runner-Up", 
                          Show_Cont$Result, ignore.case = TRUE)
  # Date: Extract the last two words or numbers
  Date <- sub(".*\\b(\\w+\\s+\\d+)\\b.*", "\\1", Show_Cont$Status)
  # Date: Drop digits from the end until there are only two digits
  Date <- gsub("(\\D*\\d{2})\\d*", "\\1", Date)
  # Date
  Show_Cont <- add_column(Show_Cont, Date, .after = "Result")
  Show_Cont$Date <- paste0(Show_Cont$Date, ", ", Year)
  # State
  # Remove the comma and space at the beginning of each string
  State <- sub(".*,\\s*", "", Show_Cont$Hometown)
  Show_Cont <- add_column(Show_Cont, State, .after = "Hometown")
  # City
  # Remove the comma and space at the end of each string
  City <- sub(",.*", "", Show_Cont$Hometown)
  Show_Cont <- add_column(Show_Cont, City, .after = "Hometown")
  # Drop columns if they exist: 
  # S09 - "Mentor"
  # S12 - "PreviousSeason", "PreviousSeasonPlacing"
  # S13 - "Region"
  Columns_Drop <- c("Mentor", "PreviousSeason", "PreviousSeasonPlacing", "Region")
  Columns_Exist <- Columns_Drop %in% names(Show_Cont)
  if (any(Columns_Exist)) {
    Show_Cont <- Show_Cont %>%
      select(-all_of(Columns_Drop[Columns_Exist]))
  }
  # Check if the column "Occupation" exists
  if(!"Occupation" %in% colnames(Show_Cont)) {
    # If it doesn't exist, create it and fill it with NAs
    Show_Cont$Occupation <- rep(NA, nrow(Show_Cont))
  }
  
  
  Age_Group <- cut(Show_Cont$Age, breaks = c(18, 25, 30, 35, 70),
                   labels = c("18-25", "25-30", "30-35", "35-70"), 
                   include.lowest = TRUE)
  Show_Cont <- add_column(Show_Cont, Age_Group, .after = "Age")
  
  
  #View(Show_Cont)
  
  
  
  
  Show_Number <- 
    # Select only ranks
    Show_Elim[1:nrow(Show_Elim), 4:ncol(Show_Elim)] %>%
    # Convert ranks to numbers
    Result.To.Number %>%
    # Mid.NA.To.Zero
    apply(., 1, Mid.NA.To.Zero) %>%
    # Transpose results
    t(.)
  
  
  

  Show_Cumsum <-
    Show_Number %>%
    # Get cumulative sums
    apply(1, cumsum) %>%
    # Transpose results
    t() %>%
    # Rename rows and columns
    #`colnames<-` (1:(ncol(.))) %>%
    #`rownames<-` (1:(nrow(.))) %>%
    # Column bind places
    as.data.frame()

  
  
  
  # Final_Point_X
  Final_Point_X <- apply(Show_Cumsum, 1, function(row) {
    Last_Num <- max(which(!is.na(row)))
    if (is.null(Last_Num)) {
      return(NA)
    } else {
      return(Last_Num)
    }
  })
  # Final_Point_Y
  Final_Point_Y <- apply(Show_Cumsum, 1, function(row) tail(row[!is.na(row)], 1))
  # Column bind Cont_IDs
  Show_Cumsum <- cbind(Show_Elim[1], Show_Cumsum)
  # Final_Point_X and Y to Show_Cont
  Show_Cont <- add_column(Show_Cont, Final_Point_X, .after = "Place")
  Show_Cont <- add_column(Show_Cont, Final_Point_Y, .after = "Final_Point_X")
  
  
  
  
  Show_Melt <-
    Show_Cumsum %>%
    # Melt by place
    melt(id.vars = 'Cont_ID') %>%
    # Merge by
    merge(Show_Cont, by.x = "Cont_ID", by.y = "Cont_ID")
  #View(Show_Melt)
  
  
  
  
  Show_List <- list(Show_Elim, Show_Cont, Show_Number, Show_Cumsum, Show_Melt)
  names(Show_List) <- c("Elim", "Cont", "Number", "Cumsum", "Melt")
  return(Show_List)
}




# ---- Vectors for Read Show automatic function ----
Seasons_Total = 13
# Define the base link
Link_Base <- "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_"
Links_Seasons <- character(Seasons_Total)
# Links defined by a for loop
for (i in 1:Seasons_Total) {
  Links_Seasons[i] <- paste0(Link_Base, i)
}
Links_Seasons
# Show repeated 
Shows <- rep("MC", Seasons_Total)
# Location repeated
Locations <- rep("America", Seasons_Total)
# Seasons sequence, zero-padded, and "S" pasted at the beginning
Seasons <- paste0("S", (sprintf("%02d", as.numeric(1:13))))
# Years skip 2020 because of Covid 19
Years_A <- 2010:2019
Years_B <- 2021:2023
Years <- c(Years_A, Years_B)
# Gender vectors for each season
MC_America_S01_Gender <- factor(c("F", "M", "M", "F", "M", "M", "M", "F", "F", "M", "M", "F", "F", "F"))
MC_America_S02_Gender <- factor(c("F", "M", "M", "F", "M", "F", "M", "F", "F", "M", "M", "F", "F", "M", "M", "M", "F", "M"))
MC_America_S03_Gender <- factor(c("F", "M", "F", "M", "F", "M", "F", "F", "M", "M", "F", "F", "M", "M", "F", "M", "M", "F"))
MC_America_S04_Gender <- factor(c("M", "F", "F", "F", "M", "F", "M", "M", "F", "F", "M", "M", "F", "M", "M", "F", "M", "F", "F"))
MC_America_S05_Gender <- factor(c("F", "F", "M", "M", "M", "F", "M", "M", "F", "F", "M", "F", "F", "M", "M", "M", "M", "F", "M", "F", "F", "F"))
MC_America_S06_Gender <- factor(c("F", "M", "M", "M", "F", "F", "M", "F", "F", "M", "F", "M", "F", "M", "M", "F", "F", "M", "M", "F", "F", "M"))
MC_America_S07_Gender <- factor(c("M", "F", "M", "F", "M", "F", "M", "M", "M", "F", "F", "M", "M", "F", "M", "F", "F", "F", "M", "F"))
MC_America_S08_Gender <- factor(c("M", "F", "M", "F", "M", "F", "M", "M", "F", "M", "F", "M", "F", "M", "M", "F", "M", "F", "F", "M"))
MC_America_S09_Gender <- factor(c("M", "F", "M", "F", "M", "M", "F", "F", "F", "F", "F", "M", "M", "M", "M", "F", "M", "M", "F", "M", "F", "F", "M", "M"))
MC_America_S10_Gender <- factor(c("F", "F", "M", "M", "F", "M", "M", "F", "M", "M", "M", "F", "M", "F", "F", "M", "M", "F", "F", "M"))
MC_America_S11_Gender <- factor(c("F", "F", "F", "M", "M", "M", "F", "M", "F", "M", "M", "M", "F", "F", "F"))
MC_America_S12_Gender <- factor(c("F", "M", "M", "F", "F", "M", "F", "M", "M", "F", "M", "M", "F", "F", "M", "M", "F", "M", "F", "M"))
MC_America_S13_Gender <- factor(c("M", "F", "F", "F", "M", "F", "F", "M", "F", "F", "M", "M", "F", "M", "M", "F", "F", "M", "M", "F"))
# Gender List from Gender vectors
Genders_List <- list(MC_America_S01_Gender,
                     MC_America_S02_Gender,
                     MC_America_S03_Gender,
                     MC_America_S04_Gender,
                     MC_America_S05_Gender,
                     MC_America_S06_Gender,
                     MC_America_S07_Gender,
                     MC_America_S08_Gender,
                     MC_America_S09_Gender,
                     MC_America_S10_Gender,
                     MC_America_S11_Gender,
                     MC_America_S12_Gender,
                     MC_America_S13_Gender)




# ---- Read Show function automatic ----
for (i in 1:13){
  Read.Show(Link = Links_Seasons[i],
            Show = Shows[i],
            Location = Locations[i],
            Season = Seasons[i],
            Year = Years[i],
            Gender = Genders_List[[i]])
}




# ---- Read Show function manual ----
Show_MC_S01 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_1",
                         Show = "MC",
                         Location = "America",
                         Season = "S01",
                         Year = "2010",
                         Gender = MC_America_S01_Gender)

Show_MC_S02 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_2",
                         Show = "MC",
                         Location = "America",
                         Season = "S02",
                         Year = "2011",
                         Gender = MC_America_S02_Gender)

Show_MC_S03 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_3",
                         Show = "MC",
                         Location = "America",
                         Season = "S03",
                         Year = "2012",
                         Gender = MC_America_S03_Gender)

Show_MC_S04 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_4",
                         Show = "MC",
                         Location = "America",
                         Season = "S04",
                         Year = "2013",
                         Gender = MC_America_S04_Gender)

Show_MC_S05 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_5",
                         Show = "MC",
                         Location = "America",
                         Season = "S05",
                         Year = "2014",
                         Gender = MC_America_S05_Gender)

Show_MC_S06 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_6",
                         Show = "MC",
                         Location = "America",
                         Season = "S06",
                         Year = "2015",
                         Gender = MC_America_S06_Gender)

Show_MC_S07 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_7",
                         Show = "MC",
                         Location = "America",
                         Season = "S07",
                         Year = "2016",
                         Gender = MC_America_S07_Gender)

Show_MC_S08 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_8",
                         Show = "MC",
                         Location = "America",
                         Season = "S08",
                         Year = "2017",
                         Gender = MC_America_S08_Gender)

Show_MC_S09 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_9",
                         Show = "MC",
                         Location = "America",
                         Season = "S09",
                         Year = "2018",
                         Gender = MC_America_S09_Gender)

Show_MC_S10 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_10",
                         Show = "MC",
                         Location = "America",
                         Season = "S10",
                         Year = "2019",
                         Gender = MC_America_S10_Gender)

Show_MC_S11 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_11",
                         Show = "MC",
                         Location = "America",
                         Season = "S11",
                         Year = "2021",
                         Gender = MC_America_S11_Gender)

Show_MC_S12 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_12",
                         Show = "MC",
                         Location = "America",
                         Season = "S12",
                         Year = "2022",
                         Gender = MC_America_S12_Gender)

Show_MC_S13 <- Read.Show(Link = "https://en.wikipedia.org/wiki/MasterChef_(American_TV_series)_season_13",
                         Show = "MC",
                         Location = "America",
                         Season = "S13",
                         Year = "2023",
                         Gender = MC_America_S13_Gender)




# ---- Merge all Main data frames ----
Show_Merged <- rbind(Show_MC_S01[[5]],
                     Show_MC_S02[[5]],
                     Show_MC_S03[[5]],
                     Show_MC_S04[[5]],
                     Show_MC_S05[[5]],
                     Show_MC_S06[[5]],
                     Show_MC_S07[[5]],
                     Show_MC_S08[[5]],
                     Show_MC_S09[[5]],
                     Show_MC_S10[[5]],
                     Show_MC_S11[[5]],
                     Show_MC_S12[[5]],
                     Show_MC_S13[[5]])
View(Show_Merged)




# 1 = Elim
# 2 = Cont
# 3 = Number
# 4 = Cumsum
# 5 = Melt
View(Show_MC_S01[[1]])
View(Show_MC_S01[[2]])
View(Show_MC_S01[[3]])
View(Show_MC_S01[[4]])
View(Show_MC_S01[[5]])




# ---- Show Unique ----
Show_Unique <- Show_Merged %>%
  distinct(Cont_ID, Final_Point_X, Final_Point_Y, .keep_all = TRUE)
View(Show_Unique)




# ---- Plot_A: Age_Group~Season ----
Show_Plot_A <- ggplot(data = Show_Merged, 
       aes(x = variable, 
           y = value,
           label = Cont_First
           )) + 
  geom_text(#position = position_dodge(width = .5),
            check_overlap = TRUE, 
            aes(label = Cont_First, 
                color = Result
            )) +
  geom_line(#position = position_dodge(width = 3),
            linewidth = 1,
            aes(
              #linewidth = Place,
              color = Result,
              group = Cont_ID)) +
  facet_grid(~Season)
print(Show_Plot_A)




# ---- Plot_B: Final Points ----
Show_Plot_B <- ggplot(data = Show_Unique, 
                      aes(x = Final_Point_X, 
                          y = Final_Point_Y,
                          color = Result)) +
  geom_text(position = position_dodge(width = 3),
    check_overlap = TRUE, 
    aes(label = Cont_First, 
        color = Result), size = 3.5) +
  geom_point() #+
  #geom_jitter(width = .5)
  #facet_grid(~Gender)
print(Show_Plot_B)




