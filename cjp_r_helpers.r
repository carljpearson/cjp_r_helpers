
##############################
###### set up   #####
##############################


##### get packages
#### source: https://vbaliga.github.io/posts/2019-04-28-verify-that-r-packages-are-installed-and-loaded/

## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("readxl", "dplyr","RCurl","ggplot2","DescTools","readr)

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
  #font <- "Inter"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks.x = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      #text = element_text(family="Open Sans")
    )
}

theme_cjp_hz <- function(){ 
  #font <- "Inter"   #assign font family up front
  
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
      #text = element_text(family="Open Sans"),
     # axis.text = element_text(color = "#040C4A")
    )
}



#coordinate cartesian for percent graphs (0,1)              
coord_100pc <- coord_cartesian(ylim=c(0,1))
              
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
###### calc    functions #####
##############################


summarize_binary_to_BinomCI <- function(data,value){
  data %>%
  summarize(total=n(),
            n=sum(value),
            prop=mean(value),
            prop_adj = BinomCI(n, total, conf.level = 0.95,method = "agresti-coull")[1],
            lower_ci = BinomCI(n, total, conf.level = 0.95,method = "agresti-coull")[2],
            upper_ci = BinomCI(n, total, conf.level = 0.95,method = "agresti-coull")[3]
  )
  
}




              ##logit to probability - src: https://sebastiansauer.github.io/convert_logit2prob/
              
#logit2prob <- function(logit){
#  odds <- exp(logit)
#  prob <- odds / (1 + odds)
#  return(prob)
#}

 


##############################
###### datasets          #####
##############################

country_data <- read_csv( RCurl::getURL("https://raw.githubusercontent.com/carljpearson/cjp_r_helpers/refs/heads/main/country_data.csv"))
