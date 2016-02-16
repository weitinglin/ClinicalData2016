#set working directory
setwd("/Users/jasonlin/Documents/2016/2016 Practice/20160124")

#loading package
library(dplyr)
library(ggplot2)
library(reshape2)
library(XML)

#define the function
#==================================================================== Function
pagecut <- function(raw_data) {
  output<-list()
  #seperate the data sheet
  tables_index<-grep("table",raw_data)
  for( i in 1:(1/2*length(tables_index))){
      a<-tables_index[2*i-1];b<-tables_index[2*i]
      output[[i]]<-raw_data[a:b]
  }
  return(output)
 }
#==================================================================== Function 
tablecut<-function(pages,raw_data){
  output<-list()
  for( i in 1: length(pages)){
    if(i < length(pages)){
      a<-pages[i];b<-pages[(i+1)]-1
      output[[i]]<-raw_data[a:b]
    }else{
      a<-pages[i]
      output[[i]]<-raw_data[a:(length(raw_data)-1)] 
      #dont include the last line "/table" in the last block
    }
  }
  return(output)
}

#==================================================================== Function
filter_chr<-function(time1_raw){
  for(i in 1:length(time1_raw)){
    n<-regexpr("<tt>",time1_raw[i])+4
    time1_raw[i]<-substring(time1_raw[i],n[1],n[1]+7)
  }
  return(time1_raw)}

#==================================================================== Function
readhtmltable_index<-function(page_number,tables){
  index_list<-list()
  tables1<-grep("Date",tables[[page_number]])
  page1_table<-tablecut(tables1,tables[[page_number]])
  
  for ( no_table in 1:length(page1_table)) {
    lab_grep_index<-grep("<tr valign=top>",page1_table[[no_table]])[1]+tables1[no_table]
    lab_grep_index_last<-rev(grep("<tr valign=top>",page1_table[[no_table]]))[2]-2+tables1[no_table]
    index_list[[no_table]] <- c(1,2,lab_grep_index:lab_grep_index_last,length(tables[[page_number]]))
  }
  
  return (index_list)
}
#==================================================================== Main

#input the data
raw_data<-readLines("testlab.html",encoding = "BIG-5")
raw_data<-iconv(raw_data,from ="BIG-5",to = "UTF-8")

#==================== Fetch the General Data of the Patient

#Name
name_raw<-raw_data[grep("姓名",raw_data)[1]+2]
name<-substring(name_raw,(regexpr("tt",name_raw)+3),(regexpr("tt",name_raw)+5))

#Patient_ID
id_raw<-raw_data[grep("病歷號",raw_data)[1]+2]
id<-substring(id_raw,(regexpr("tt",id_raw)+3),(regexpr("tt",id_raw)+9))

#Gender
gender_raw<-raw_data[grep("性別",raw_data)[1]+2]
gender<-substring(gender_raw,(regexpr("tt",gender_raw)[1]+3),(regexpr("tt",gender_raw)[1]+3))

#Birthday
birth_raw<-raw_data[grep("生日",raw_data)[1]+2]
birth<-getlabname(birth_raw)

#==================== Preprocessing the lab data

#Cut the data into pages with "table"(write in the function)
tables<-pagecut(raw_data)

#Separate the page's table into page1_table

page_number<-3
index <- readhtmltable_index(page_number,tables)

# #Use XML package
no_table<-2 #if length(page1_table)==1
# lab_grep_index<-grep("<tr valign=top>",page1_table[[no_table]])[1]+tables1[no_table]
# lab_grep_index_last<-rev(grep("<tr valign=top>",page1_table[[no_table]]))[2]-2+tables1[no_table]
# 
# #If length(page1_table) == 1
# index<-c(1,2,lab_grep_index:length(tables[[page_number]]))  # need to fix due to the triple table in one page
# #If length(page1_table) != 1
# index<-c(1,2,lab_grep_index:lab_grep_index_last,length(tables[[page_number]]))


#use XML package to parse the table *****
tables_test_index <- readHTMLTable(tables[[page_number]][index[[2]]],encoding="UTF-8",as.data.frame = TRUE)



#get the time 
table_time <- function (page_number,tables) {
  time_list <- list() 
  tables1 <- grep("Date",tables[[page_number]])
  tryCatch ( page1_table <- tablecut(tables1,tables[[page_number]]) ,
            warning = function(w) { page1_table <- tablecut(tables1,tables[[page_number-1]])},
            error = function(e) { page1_table <- tablecut(tables1,tables[[page_number-1]])} )
  
  for ( no_table in 1 : length(page1_table)) {
    page_1_date_line <- grep("<tr><td",unlist(page1_table[[no_table]]))
    temporary<-unlist(page1_table[[no_table]])
    temporary<-temporary[-page_1_date_line]
    if ( grep("<tr valign=top>",temporary)[1] >3 ) {
     page_date_line <- grep("<tr valign=top>",temporary)[1]
     time1_raw <- c( temporary[seq( 3, (page_date_line-2), by = 2)])
     }else{
      page_date_line <- grep("<tr valign=top>",temporary)[2] 
      time1_raw <- c( temporary[seq( (grep("<tr valign=top>",temporary)[1]+2), (page_date_line-2), by = 2)])
    
    }
    
    ##the lab data time
    time_list[[no_table]] <- filter_chr(time1_raw)
      
  }
  return( time_list)
}
table_time(6,tables)
#preprocess 
names(tables_test_index)<-"table"
index_table<-c(2,seq(4,4*length(time1),by=4))
clean_test<-tables_test_index$table[index_table]
rule_out<-sapply(clean_test[,1],function(x){ if( is.na(x) == TRUE ){ sum(!is.na(x)) }else{nchar(as.character(x))}})>0 
#remove the NA column
clean_test<-clean_test[rule_out,]
#remove duplicate data : normale plasma mean
duplicat_index <- !duplicated(clean_test[,1],fromLast = TRUE)&!duplicated(clean_test[,1]) 
clean_test<-clean_test[duplicat_index,]
row.names(clean_test)<-clean_test[,1]
colnames(clean_test)[1]<-c("Lab_data")
colnames(clean_test)[2:(length(time1)+1)]<-time1
#
clean_melt_test<-melt(clean_test,id.vars = "Lab_data")
clean_melt_test$ID<-id
clean_melt_test$BIRTH<-birth
clean_melt_test$GENDER<-gender
