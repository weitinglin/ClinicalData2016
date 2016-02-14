#set working directory
setwd("/Users/jasonlin/Documents/2016/2016 Practice/20160124")

#loading package
library(dplyr)
library(ggplot2)
library(reshape2)
library(RCurl)
library(XML)

raw_data<-readLines("testlab.html",encoding = "BIG-5")
raw_data<-iconv(raw_data,from ="BIG-5",to = "UTF-8")

#seperate the data sheet
pages<-grep("<table border",raw_data)
#===============================
pagecut<-function(pages,raw_data){
  output<-list()
  for( i in 1: length(pages)){
    if(i < length(pages)){
      a<-pages[i];b<-pages[(i+1)]-1
      output[[i]]<-raw_data[a:b]
    }else{
      a<-pages[i]
      output[[i]]<-raw_data[a:length(raw_data)]
    }
  }
  return(output)
}
#============================
#cut the data into 6 page with "<table"
pages<-pagecut(pages,raw_data)
#separate the 1st page's table into page1_table
pages1<-grep("Date",pages[[1]])
page1_table<-pagecut(pages1,pages[[1]])
#use XML package
index<-c(1,2,84:798)
tables_test_index <- readHTMLTable(pages[[1]][index],encoding="UTF-8",as.data.frame = TRUE)
#get the time 
page_1_date_line<-grep("<tr><td",unlist(page1_table))
time1_raw<-c(unlist(page1_table)[seq(3,(page_1_date_line-2),by=2)])
#===============function:filter_chr(time1_raw) no.2  need
filter_chr<-function(time1_raw){
  for(i in 1:length(time1_raw)){
    n<-regexpr("<tt>",time1_raw[i])+4
    time1_raw[i]<-substring(time1_raw[i],n[1],n[1]+7)
  }
  return(time1_raw)}
time1<-filter_chr(time1_raw)   ##the lab data time
#name
name_raw<-raw_data[grep("姓名",raw_data)[1]+2]
name<-substring(name_raw,(regexpr("tt",name_raw)+3),(regexpr("tt",name_raw)+5))

#patient_id
id_raw<-raw_data[grep("病歷號",raw_data)[1]+2]
id<-substring(id_raw,(regexpr("tt",id_raw)+3),(regexpr("tt",id_raw)+9))

#gender
gender_raw<-raw_data[grep("性別",raw_data)[1]+2]
gender<-substring(gender_raw,(regexpr("tt",gender_raw)[1]+3),(regexpr("tt",gender_raw)[1]+3))
#birthday
birth_raw<-raw_data[grep("生日",raw_data)[1]+2]
birth<-getlabname(birth_raw)
#try 1
names(tables_test_index)<-"table1"
index_table<-c(2,seq(4,28,by=4))
clean_test<-tables_test_index$table1[index_table]
sapply(clean_test[,1],function(x){nchar(as.character(x))})>0 -> rule_out
#remove the NA column
clean_test<-clean_test[rule_out,]
#remove duplicate data : normale plasma mean
!duplicated(clean_test[,1],fromLast = TRUE)&!duplicated(clean_test[,1]) -> duplicat_index
clean_test<-clean_test[duplicat_index,]
row.names(clean_test)<-clean_test[,1]
colnames(clean_test)[1]<-c("Lab_data")
colnames(clean_test)[2:8]<-time1
#
clean_test
clean_melt_test<-melt(clean_test,id.vars = "Lab_data")
clean_melt_test$ID<-id
clean_melt_test$BIRTH<-birth
clean_melt_test$GENDER<-gender
