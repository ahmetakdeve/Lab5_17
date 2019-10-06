#' Sweden election 2014
#' 
#' Getting the Sweden election result from 2014. The function makes it possible to pick out a specific part of a municipality where you can how it has been voted in the municipal, county och national election in the specific part of the municipality.
#' 
#' @param mun_code A The municipality code for the municipality you want the result for. At the end of the municipality code you specify whether you want results for the municipal, county or national election by using "K", "L" or "R".
#' 
#' @return A data frame with 20 columns containing results in both numbers and percentages for each party
#' 
#' @import utils xml2 tidyverse stringr tidyr methods
#' 
#' @export

votes_spec<-function(mun_code){
  
  temp_file <- tempfile(tmpdir = tdir <- tempdir())
  download.file("https://data.val.se/val/val2014/valnatt/valnatt.zip", temp_file)
  xml_files <- unzip(temp_file, exdir = tdir)
  random_picked<-xml_files[10]
  selected_mun<-paste0(str_sub(random_picked,1,-10),mun_code,str_sub(random_picked,-4))
  read_chosen <- read_xml(selected_mun)
  
  df <- bind_rows(lapply(xml_find_all(read_chosen, "//VALDISTRIKT"), function(x) {
    
    parent <- data.frame(as.list(xml_attrs(x)), stringsAsFactors=FALSE)
    kids <- bind_rows(lapply(xml_children(x), function(x) as.list(xml_attrs(x))))
    cbind.data.frame(parent, kids, stringsAsFactors=FALSE)
    
  }))
  
  df2<-df[,c("KOD","NAMN","PARTI","PROCENT")]
  colnames(df2)<-c("KOD","NAMN","PARTI","PROCENT")
  df2<-df2[!is.na(df2$PARTI),]
  
  parties<-c("M","C","FP","KD","S","V","MP","SD","FI")
  
  df2<-df2[df2$PARTI%in%parties,]
  
  df2$PROCENT<-as.numeric(gsub(",", ".", gsub("\\.", "", df2$PROCENT)))
  
  return(df2)
}


