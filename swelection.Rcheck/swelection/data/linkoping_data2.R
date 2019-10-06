library(reshape2)
library(xml2)
library(tidyverse)
library(ggplot2)
library(stats)
library(tidyr)
library(magrittr)
library(methods)
library(dplyr)
votes_spec2<-function(mun_code){
  
  temp_file <- tempfile(tmpdir = tdir <- tempdir())
  utils::download.file("https://data.val.se/val/val2014/valnatt/valnatt.zip", temp_file)
  xml_files <- utils::unzip(temp_file, exdir = tdir)
  random_picked<-xml_files[10]
  selected_mun<-paste0(stringr::str_sub(random_picked,1,-10),mun_code,stringr::str_sub(random_picked,-4))
  read_chosen <- xml2::read_xml(selected_mun)
  
  df <- dplyr::bind_rows(lapply(xml2::xml_find_all(read_chosen, "//VALDISTRIKT"), function(x) {
    
    # extract the attributes from the parent tag as a data.frame
    parent <- data.frame(as.list(xml2::xml_attrs(x)), stringsAsFactors=FALSE)
    
    # make a data.frame out of the attributes of the kids
    kids <- dplyr::bind_rows(lapply(xml2::xml_children(x), function(x) as.list(xml2::xml_attrs(x))))
    
    # combine them
    cbind.data.frame(parent, kids, stringsAsFactors=FALSE)
    
  }))
  
  df2<-df[,c(1,2,7,8,10)]
  colnames(df2)<-c("KOD","NAMN","PARTI","ROSTER","PROCENT")
  df2<-df2[!is.na(df2$PARTI),]

  parties<-c("M","C","FP","KD","S","V","MP","SD","FI")

  df2<-df2[df2$PARTI%in%parties,]

  df2$PROCENT<-as.numeric(gsub(",", ".", gsub("\\.", "", df2$PROCENT)))
  
  return(df2)
}

linkoping_data2<-votes_spec2("0580K")
