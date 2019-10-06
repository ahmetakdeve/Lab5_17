#' Sweden election 2014
#'
#' The function returns the result per municipality. The function can take 3 values. "00K" gives the result in the municipal elections, "00L" gives the result in the county elections per municipality and "00R" gives the result in the national elections per municipality.
#' 
#' @param the_code A The municipality code
#' 
#' @return A data frame with 20 columns containing results in both numbers and percentages for each party and 290 rows for each municipality in Sweden
#' 
#' @import utils xml2 tidyverse stringr tidyr methods dplyr
#' 
#' @export

votes_sum<-function(the_code){
  
  temp_file <- tempfile(tmpdir = tdir <- tempdir())
  download.file("https://data.val.se/val/val2014/valnatt/valnatt.zip", temp_file)
  xml_files <- unzip(temp_file, exdir = tdir)
  random_picked<-xml_files[1]
  selected_mun<-paste0(stringr::str_sub(random_picked,1,-8),the_code,stringr::str_sub(random_picked,-4))
  read_chosen <- read_xml(selected_mun)
  
  df <- bind_rows(lapply(xml_find_all(read_chosen, "//KOMMUN"), function(x) {
    
    # extract the attributes from the parent tag as a data.frame
    parent <- data.frame(as.list(xml_attrs(x)), stringsAsFactors=FALSE)
    
    # make a data.frame out of the attributes of the kids
    kids <- bind_rows(lapply(xml_children(x), function(x) as.list(xml_attrs(x))))
    
    # combine them
    cbind.data.frame(parent, kids, stringsAsFactors=FALSE)
    
  }))
  if(the_code=="00K"){
  df2<-df[,c(1,2,12,13,15)]
  }else{
    df2<-df[,c(1,2,10,11,13)]
  }
  colnames(df2)<-c("KOD","NAMN","PARTI","ROSTER","PROCENT")
  df2<-df2[!is.na(df2$PARTI),]
  parties<-c("M","C","FP","KD","S","V","MP","SD","FI")

  df2<-df2[df2$PARTI%in%parties,]

  df2<-stats::reshape(df2,idvar = c("KOD","NAMN"),timevar = "PARTI",direction = "wide")
  
  return(df2)
  
}

tmp<-votes_sum("00K")
