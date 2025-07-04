
##############################
###### set up   #####
##############################


##### get packages
#### source: https://vbaliga.github.io/posts/2019-04-28-verify-that-r-packages-are-installed-and-loaded/

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("readxl", "dplyr","RCurl","ggplot2","DescTools","readr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## command to load packages I generally use

load_packages <- function(
  linear_regression = FALSE,
  imputation = FALSE,
  ordinal = FALSE,
  text = FALSE,
  weighting_surveys = FALSE,
  ggplot_advanced = FALSE
) {
  # Initialize an empty vector for packages
  packages <- c()
  
  # Linear regression packages
  if (linear_regression) {
    packages <- c(packages, "car", "sjstats", "sjPlot", "lme4", "broom", "emmeans", "effectsize")
  }
  
  # Imputation packages
  if (imputation) {
    packages <- c(packages, "mice", "miceRanger","mitools")
  }
  
  # Ordinal packages
  if (ordinal) {
    packages <- c(packages, "MASS", "kableExtra", "shapr","ordinal","brant")
  }
  
  # Text packages
  if (text) {
    packages <- c(packages, "tidytext", "SnowballC")
  }
  
  # Weighting surveys packages
  if (weighting_surveys) {
    packages <- c(packages, "survey", "srvyr", "svyweight", "autumn")
  }
  
  # ggplot advanced packages
  if (ggplot_advanced) {
    packages <- c(packages, "ggflags", "ggrepel", "ggpubr", "RColorBrewer")
  }
  
  # Default packages (always run last)
  default_packages <- c(
    "DescTools",
    "psych",
    "here",
    "jtools",
    "rstatix",
    "janitor",
    "scales",
    "forcats",
    "stringr",
    "extrafont",
    "tidyverse"
  )
  
  # Combine all packages with default packages last
  all_packages <- c(packages, default_packages)
  
  # Load all packages
  lapply(all_packages, library, character.only = TRUE)
}


##############################
###### data read_write   #####
##############################


#paste to clipboard, optionally rounding numeric vars
to.clipboard <- function(x, round_numeric = FALSE) {
  if (round_numeric) {
    x <- x %>%
      mutate(across(where(is.numeric), ~ round(., 4)))
  }
  clipr::write_clip(x)
}


#read all excel sheets
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


##############################
###### move/filter data  #####
##############################
              
#insert new row
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

#opposite of intersect w/ dplyr
outersect <- function(x, y) {
  sort(c(x[!x%in%y],
         y[!y%in%x]))
}
  
#not IN function  
`%!in%` <- function(x,y)!('%in%'(x,y))    

##############################
###### ggplotting        #####
##############################

#100pc coord cartesion
coord_y_percent <- coord_cartesian(ylim=c(0,1))


theme_cjp <- function(){ 
  
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks.x = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      text = element_text(family="Reddit Sans"),
      axis.text = element_text(family="Reddit Sans"),
      title = element_text(family="Reddit Sans")
    )
}

theme_cjp_hz <- function(){ 
  
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      #strip major y gridlines
      panel.grid.major.y = element_line( size=.1, color="black" ) ,
      panel.grid.major.x = element_blank() ,    
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks.x = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      text = element_text(family="Reddit Sans"),
      axis.text = element_text(family="Reddit Sans"),
      title = element_text(family="Reddit Sans"),
    )
}



#coordinate cartesian for percent graphs (0,1)              
coord_100pc <- coord_cartesian(ylim=c(0,1))

#flip legend for coord_flip to synchronize

coord_flip_legend_reverse <- function(data,...){
coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))

  

  
}
              
              
#colors - temp until I get a more robust solution for ggtheming
       
##reddit colors       

rc.color <- c("#FF4500","#FF5FC2","#FFBF0B","#AEEF0F","#00E2B7","#D82400","#D3168B","#F27300",  "#65B200",  "#08A59D","#FFDAD7","#FFE6F9","#FFF3C0","#F0FFB3","#E6FFFA")
rc.color.alt <- c("#FF4500","#FFDAD7","#D82400",  
                  "#FF5FC2","#FFE6F9","#D3168B",
                  "#FFBF0B","#FFF3C0","#F27300",
                  "#AEEF0F","#F0FFB3",  "#65B200",
                  "#00E2B7","#E6FFFA", "#08A59D"
                 )
rc.color.shade <-  c("#D82400","#D3168B","#F27300",  "#65B200",  "#08A59D")
rc.color.tint <- c("#FFDAD7","#FFE6F9","#FFF3C0","#F0FFB3","#E6FFFA")

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


              
              


##############################
#### pipe extra functions ####
##############################


summarize_binary_to_BinomCI <- function(data){
  
  data %>%
  summarize(total=n(),
            n=sum(value),
            prop=mean(value),
            prop_adj = BinomCI(n, total, conf.level = 0.95,method = "agresti-coull")[1],
            prop_low = BinomCI(n, total, conf.level = 0.95,method = "agresti-coull")[2],
            prop_upp = BinomCI(n, total, conf.level = 0.95,method = "agresti-coull")[3]
  )
  
}

#create a function that blends rowwise and mutate
rowwise_mutate  <- function(.data, ...) {
  .data %>%
    rowwise() %>%
    mutate(...)
}

##############################
###### calc    functions #####
##############################

              
              ##logit to probability - src: https://sebastiansauer.github.io/convert_logit2prob/
              
#logit2prob <- function(logit){
#  odds <- exp(logit)
#  prob <- odds / (1 + odds)
#  return(prob)
#}

              
##############################
##### weighing  functions ####
##############################

# Define the function to create sample margin formulas
create_raking_formulas <- function(raking_vars) {
  # Check if raking_vars is a character vector
  if (!is.character(raking_vars)) {
    stop("Input 'raking_vars' must be a character vector.")
  }
  
  # Check if raking_vars is empty
  if (length(raking_vars) == 0) {
    warning("Input 'raking_vars' is empty. Returning an empty list.")
    return(list())
  }
  
  # Use lapply to create a list of formulas
  # For each variable name in raking_vars, create a formula of the form ~variable_name
  sample_margins_formulas <- lapply(raking_vars, function(var) {
    as.formula(paste0("~", var))
  })
  
  # Set the names of the list elements to be the variable names
  names(sample_margins_formulas) <- raking_vars
  
  # Return the named list of formulas
  return(sample_margins_formulas)
}
              

 


##############################
###### datasets          #####
##############################

country_data <- read_csv( RCurl::getURL("https://raw.githubusercontent.com/carljpearson/cjp_r_helpers/refs/heads/main/country_data.csv"))
