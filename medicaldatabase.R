#20160124 Medical txt mining

#set working directory
setwd("/Users/jasonlin/Documents/2016/2016 Practice/20160124")

#loading package
library(dplyr)
library(ggplot2)
library(reshape2)
library(RCurl)
library(XML)
#query the help
?XML
#input data
raw_data<-readLines("testlab.html",encoding = "BIG-5")
raw_data_test<-readLines("testlab.html",encoding = "BIG-5")  #for test
raw_data_test<-iconv(raw_data_test,from ="BIG-5",to = "UTF-8")
raw_data_test%>%head(n=1000L)
?tail
#convert the filecoding
#把文字編碼從big-5 change to utf-8
raw_data<-iconv(raw_data,from ="BIG-5",to = "UTF-8")

#preprocess data
##fetch the lab data value from the mess
getlabname<-function(raw){
  start<-regexpr("<tt>",raw)+4
  end<-regexpr("</tt>",raw)-1
  raw<-substring(raw,start,end)
  return(raw)
}
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

#separate the raw data into pages
#======================================================================Program Begin===============
MedicalDatabase_generator(raw_data) 
MedicalDatabase_generator<-function(raw_data){
pages<-grep("Date",raw_data)
#=======================================Define Function Space=====================================
#====function no.1
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

seperate_page<-pagecut(pages,raw_data)

#parse the lab data inside each pages
#==============================================For loog to process each page
for(i in 1:length(seperate_page)){
page_1<-seperate_page[[6]]



#get the time 
page_1_date_line<-grep("<tr>",page_1)[1]
time1_raw<-c(page_1[seq(3,(page_1_date_line-2),by=2)])
#===============function:filter_chr(time1_raw) no.2  need
filter_chr<-function(time1_raw){
  for(i in 1:length(time1_raw)){
    n<-regexpr("<tt>",time1_raw[i])+4
  time1_raw[i]<-substring(time1_raw[i],n[1],n[1]+7)
  }
  return(time1_raw)}
time1<-filter_chr(time1_raw)   ##the lab data time
##index generation
#find this page's lab width
lab_index<-grep("<tr valign=top>",page_1)
lab_column_wide<-page_1[lab_index[1]+4]
width_index<-regexpr("width=",lab_column_wide)
colwide_int<-(substring(lab_column_wide,width_index+6,width_index+7))
search_term<-paste("<td width=",colwide_int,sep="")  #in order to search the lab data value position
#clean the page_1 ,left only the lab data 
lab_data<-page_1[first(lab_index):last(lab_index)]
lab_data<-lab_data[1:grep("</table>",lab_data)]  # remove the tail of the data  
lab_data_head_index<-grep("<tr valign=top>",lab_data)
lab_data_fine_index<-grep(search_term,lab_data,fixed=F)
#function to separate the index number
#======================================function no.3:structure_list(lab_data_fine_index,lab_data_head_index)
structure_list<-function(lab_data_fine_index,lab_data_head_index){
data_list<-list()
for(i in 1:length(lab_data_head_index)){
  if(i == length(lab_data_head_index)){
    tem_list<-list(lab_data_fine_index[lab_data_fine_index>lab_data_head_index[i]])
    data_list[i]<-tem_list 
  } else {
  tem_list<-list(lab_data_fine_index[lab_data_fine_index>lab_data_head_index[i]&lab_data_fine_index<lab_data_head_index[i+1]])
  data_list[i]<-tem_list
  }
}
return(data_list)
}
#get the list of each lab item's value
data_list<-structure_list(lab_data_fine_index,lab_data_head_index)
#data_list[sapply(data_list,length)!=0]  #remove the zero factor list
data_list_modify_for_item<-data_list
data_list<-data_list[sapply(data_list,length)!=0]
#get the lab data which need to add na back
further_management<-grep(TRUE,(sapply(data_list_modify_for_item,length)<length(time1))&(sapply(data_list_modify_for_item,length)!=0))   # further management index
further_management_forcombine<-grep(TRUE,(sapply(data_list,length)<length(time1))&(sapply(data_list,length)!=0))


##
lab_data_list<-list()
lab_item<-list()
for(i in 1: length(data_list)){
  lab_data_list[[i]]<-getlabname(lab_data[data_list[[i]]])
  
}
for (i in 1:length(data_list_modify_for_item)){
lab_item[i]<-getlabname(lab_data[lab_data_head_index[i]+2])
}
# lab_data[data_list[[1]]] #data value
# lab_data[lab_data_head_index[1]+2] #  lab_name
# ##
lab_data_list<-lab_data_list[lapply(sapply(lab_data_list,nchar),sum)!=0] #remove the length 0 string
lab_item<-lab_item[sapply(lab_item,nchar)!=0]   #remove the length 0 string
##find the data item and added NA back to it

lab_raw_data<-list()
for(i in 1:length(lab_data_head_index)){
  if(i==length(lab_data_head_index)){
    start<-lab_data_head_index[i]
    end<-length(lab_data)
    lab_raw_data[[i]]<-lab_data[start:end]
  }else{
  start<-lab_data_head_index[i]
  end<-lab_data_head_index[i+1]-1
  lab_raw_data[[i]]<-lab_data[start:end]}
}

further_data<-lab_raw_data[further_management]   #problem lab data 

#=========================function:

##function:function 1
getcolspan<-function(raw){
  start<-regexpr("colspan=",raw)+8
  end<-regexpr(">",raw)-1
  raw<-substring(raw,start,end)
  return(raw)
}
##function:function 2
getfurtherna<-function(further_data){
al_test<-list(c(11,9,10,7,10,12,7),c(12,10,7,10,12,7),c(12,7,10,12,7),c(10,10,12,7),c(14,12,7),c(15,7),c(7))
getnanumber<-function(a_colspan,b_list){
  if (b_list!=6) {
  f=c(0,7:1);k=c(1:7,1:6,1:5,1:4,1:3,1:2,1);m=0;n=1+sum(f[1:(b_list+1)]);z=c(rep(1,7),rep(2,6),rep(3,5),rep(4,4),rep(5,3),rep(6,2),7)
  while(m!=a_colspan){
    n<-n+1
    m<-sum(al_test[[z[n]]][1:k[n]])
  }
  return(rep(FALSE,k[n]))
  }
  return(FALSE)
}
loop_list<-list()
index_for_loop<-paste("width=",colwide_int,sep="")
for (i in 1:length(further_data)){
  subloop<-c()
  for(j in 1:length(further_data[[i]])){
#     further_data[[i]] #
#     j<-20 #
#     i<-5 #
    if (grepl(index_for_loop,further_data[[i]][j])==TRUE){
      subloop<-c(subloop,TRUE)
    } else if (grepl("<td colspan=",further_data[[i]][j])==TRUE){
      a_colspan<-as.numeric(getcolspan(further_data[[i]][j]))
      b_list<-length(subloop)
      if(a_colspan>5 & a_colspan!=8){
      subloop<-c(subloop,getnanumber(a_colspan,b_list))
      }
    }
  }
  loop_list[[i]]<-subloop
}
return(loop_list)
}

#=========================function:getNaLab(further_data) no.4
# getNaLab<-function(further_data){
#   final_list<-list()
# for(i in 1:length(further_data)){
#   index<-grep("width",further_data[[i]])
#   temp<-c()
#   for(j in 1:length(index)){
#     start<-regexpr("width=", further_data[[i]][index[j]])[1]+6
#     end<-start+1
#     temp<-c(substring(further_data[[i]][index[j]],start,end),temp)
#   }
#   final_list[i]<-list(temp)
#   } 
#   return(final_list)
# }
# width_number<-sapply(getNaLab(further_data),rev)
##try another way to figureout what's problem
#=========================function:getcolspanLab(further_data) no.5
getcolspanLab<-function(further_data){
  final_list<-list()
  for(i in 1:length(further_data)){
    index<-grep("colspan",further_data[[i]])
    temp<-c()
    for(j in 1:length(index)){
      start<-regexpr("colspan=", further_data[[i]][index[j]])
      end<-regexpr("colspan=\\d+", further_data[[i]][index[j]])
      diff<-attr(end,"match.length")-attr(start,"match.length")
      attr(end,"match.length")
      temp<-c(substring(further_data[[i]][index[j]],start[1]+attr(start,"match.length"),start[1]+attr(start,"match.length")+diff-1),temp)
    }
    final_list[i]<-list(temp)
  } 
  return(sapply(final_list,rev))
 }
colspan_number<-getcolspanLab(further_data)


# ##get the colspan index
# colspan_index<-list()
# for(i in 1:length(further_data)){
#   colspan_index[[i]]<-grep("colspan",further_data[[i]])
# }
# ##get the width index
# # colwide_int from upper
# wid_index<-list()
#   for(i in 1:length(further_data)){
#     wid_index[[i]]<-grep(paste("width=",colwide_int,sep=''),further_data[[i]])
#   }
##
#========================function:na.index(colspan_number) no.6
# na.index<-function(colspan_number){
#   colspan_number<-sapply(colspan_number, as.numeric)    #turn numeric
#   colspan_number<-sapply(colspan_number,function(x) x[x>5&x!=8]) 
# na.list<-list()
# al<-list(c(11,9,10,7,10,12,7),c(12,10,7,10,12,7),c(12,7,10,12,7),c(10,10,12,7),c(14,12,7),c(15,7),c(7))
# for (i in 1:length(colspan_number)){
#   na.sub.list<-c()
#   for(j in 1:length(colspan_number[[i]])){
#     n=0;m=0;z=c(rep(1,7),rep(2,6),rep(3,5),rep(4,4),rep(5,3),rep(6,2),7)
#     k=c(1:7,1:6,1:5,1:4,1:3,1:2,1)
#     while(m!=colspan_number[[i]][j]){
#       n<-n+1
#       m<-sum(al[[z[n]]][1:k[n]])
#      }
#      na.sub.list<-c((z[n]:(z[n]+k[n]-1)) , na.sub.list) #
#      
#    }
#   na.list[[i]]<-na.sub.list
# 
#  }
# return(sapply(na.list,sort))
# }

##
#addbackna_labdata<-lab_data_list[further_management]
addbackna_labdata<-lab_data_list[further_management_forcombine]
#test_na_add_data<-na.index(colspan_number)
getfurther_na<-getfurtherna(further_data)
transform_na<-function(getfurther_na){
  list_number<-1:length(time1)
  total_list<-list()
  for (i in 1:length(getfurther_na)){
    total_list[[i]]<-list_number[!getfurther_na[[i]]]
  }
  return(total_list)
}
test_na_add_data<-transform_na(getfurther_na)
Add_NS_back<-function(test_na_add_data,addbackna_labdata) {
  refe<-list(1:length(time1)) ##
  reverse<-list()
  final_list<-list()
  for(i in 1:length(test_na_add_data)){
    refe<-c(1:length(time1))  ##
    reverse[[i]]<-setdiff(refe,as.vector(test_na_add_data[[i]]))  #only vector can use this function
  }
 for(i in 1:length(reverse)){
   new_list<-c()
   for(j in 1:length(reverse[[i]])){
      
     new_list[reverse[[i]][j]]<-addbackna_labdata[[i]][j]
     
   }
   new_list[length(time1)]<-NA
   final_list[[i]]<-new_list
 }
  return(final_list)
}
AddBack_dataset<-Add_NS_back(test_na_add_data,addbackna_labdata)

finalization_labdata<-function(lab_data_list,AddBack_dataset,further_management_forcombine){
for(i in 1:length(AddBack_dataset)){
  lab_data_list[further_management_forcombine[i]]<-AddBack_dataset[i]
}
  return(sapply(lab_data_list,as.numeric))
}
Final_Lab_Data<-finalization_labdata(lab_data_list,AddBack_dataset,further_management_forcombine)
##########

rownames(Final_Lab_Data)<-time1
colnames(Final_Lab_Data)<-lab_item

#########
Final_Lab_Data_df<-as.data.frame(Final_Lab_Data)
Final_Lab_Data_df$date<-rownames(Final_Lab_Data_df)
Final_Lab_Data_df_melt<-melt(Final_Lab_Data_df,id.vars = "date")
Total<-Final_Lab_Data_df_melt
Total$ID<-id
Total$Gender<-gender
Total$Birth<-birth
Total$Name<-name
glimpse(Total)
###demo
ggplot(Final_Lab_Data_df_melt)+geom_point(aes(x=date,y=value,colour=variable))
thm <- function() theme(text=element_text(size=20, family="STHeiti"))
ggplot(Final_Lab_Data_df_melt)+geom_point(aes(x=date,y=value,size=value))+facet_wrap(~variable)+thm()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########
if(i>1){
  Medical_database<-bind_rows(Final_Lab_Data_df_melt,Medical_database)
}else{
  Medical_database<-Final_Lab_Data_df_melt
}
 } # page for loop


return(Medical_database)
} # big  function 
lab_data[data_list[[1]]-1]%>%getlabname()
##Another Way to Deal with the Data with XML package
install.packages("XML")
library(XML)
library(xml2)
htm_data<-read_html("testlab.html",encoding = "BIG-5")
htm_data<-iconv(htm_data,from ="BIG-5",to = "UTF-8")

#xml parse
htm_parse<-xmlParse(htm_data,encoding="UTF-8")
htmfiles<-xmlRoot(htm_parse)
xmlChildren(htmfiles)
#explore the data structure
xmlName(htmfiles)
xmlSize(htmfiles)
names(htmfiles)
#digging further
htmfiles[]
#parse based on string html
test<-htmlTreeParse(htm_data,useInternalNodes = T)

#getNodeSet
notset<-getNodeSet(test,"//table")
notset_tr<-getNodeSet(htmfiles,"//tr")
noteset_tr_tt<-getNodeSet(htmfiles,"//tt")
notset_tr[[8]]
class(notset_tr)
noteset_tr_tt[30]
xmlAttrs(noteset_tr_tt)
