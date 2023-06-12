to.clipboard <- function(x){ 
  clipr::write_clip(x)
  }
  
  #custom theming
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
      #text = element_text(family="Open Sans"),
      axis.text = element_text(color = "#040C4A")
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

#100pc coord cartesion
coord_100pc <- coord_cartesian(ylim=c(0,1))


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

#coordinate cartesian for percent graphs (0,1)              
coord_100pc <- coord_cartesian(ylim=c(0,1))
              
#colors - temp until I get a more robust solution for ggtheming
       
##reddit colors       
rc.gray <- c("#7A9299")
rc.2.gray <- c("#BBC7CC","#7A9299")
rc.5.color <- c("#FFD635","#FFA800","#FF4500","#00CCC0","#02315C")
                rc.6.color <- c("#FFD635","#FFA800","#FF4500","#00CCC0",#FFF8B8,"#02315C")

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


              
              
#logit to probably - src: https://sebastiansauer.github.io/convert_logit2prob/
              
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#not IN function  
'%!in%' <- function(x,y)!('%in%'(x,y))         
