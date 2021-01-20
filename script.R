

library(tidyverse)
#### open all training sessions of one specific participants 
file_name <- fs::dir_ls("data") #read the name of document in data file (data file is in the 3rd_pilot project)

PP_name <- c("ACOydLe6dK") #define a variable as this participant' code
 
PP_file_name_row=grep(PP_name,file_name) #find the name that contains this participant' code because data file contains all participants data and we only analyze 
                                #one participants at a time

PP_file_name=file_name[PP_file_name_row] #all the file name of one specific participants


file_content <- list() #define a list to load all documents of one specific participants


for (i in seq_along(PP_file_name_row)) {
  file_content[[i]] <- read.csv(
    file = PP_file_name[i], sep = ""
  )
}                    # open all documents of one specific participants 

file_content <- setNames(file_content, PP_file_name)  # name each row of a list 


####create a number of variables (list)
SST_row=list() #create a list to load all calculated data
SST=list()
SST_go_ROW=list()
SST_go=list()
SST_correct_go_ROW=list()
SST_correct_go=list()
SST_stop_ROW=list()
SST_stop=list()
SST_stop_failed_stop_ROW=list()
SST_stop_failed_stop=list()
SST_number_of_correct_go=list()
SST_number_of_failed_stop=list()
SST_correct_mean_go=list()
SST_correct_sd_go=list()
SST_accuracy_rate_go=list()
SST_all_mean_go=list()
SST_all_sd_go=list()
RT1SD1=list()
RT1SD2=list()
RT2SD1=list()
RT2SD2=list()
SST_real_meanSSD=list()
SST_set_meanSSD=list()
SST_uncanceled_rate=list()
SST_nthRT=list()
SST_real_SSRT=list()
SST_set_SSRT=list()
SST_uncanceled_RT=list()
blockorder=list()


#calculate related dependent variables
for (n in seq_along(PP_file_name_row)){
  SST_row[[n]]<-which(file_content[[n]][16]== "stopgo1"|file_content[[n]][16]== "stopgo2"|file_content[[n]][16]== "stopgo1_1"|
                        file_content[[n]][16]== "stopgo1_2"|file_content[[n]][16]== "stopgo1_3"|file_content[[n]][16]== "stopgo1_4")
  SST[[n]] <- file_content[[n]][c(SST_row[[n]]),]
  SST_go_ROW[[n]] <- which(SST[[n]][,c(1)]=='go')
  SST_go[[n]] <- SST[[n]][c(SST_go_ROW[[n]]),]
  SST_correct_go_ROW[[n]] <- which(SST_go[[n]][,c(10)]==1)
  SST_correct_go[[n]] <- SST_go[[n]][c(SST_correct_go_ROW[[n]] ),]
  SST_stop_ROW[[n]] <- which(SST[[n]][,c(1)]=='nogo')
  SST_stop[[n]] <- SST[[n]][c(SST_stop_ROW[[n]]),]
  SST_stop_failed_stop_ROW[[n]] <- which(SST_stop[[n]][,c(15)]==0)
  SST_stop_failed_stop[[n]] <- SST_stop[[n]][c(SST_stop_failed_stop_ROW[[n]]),]
  SST_number_of_correct_go[[n]] <- length(SST_correct_go_ROW[[n]])
  SST_number_of_failed_stop[[n]] <- length(SST_stop_failed_stop_ROW[[n]])
  SST_correct_mean_go[[n]]=as.numeric(round(colMeans(SST_correct_go[[n]][,c(9),drop(FALSE)]),2))
  SST_correct_sd_go[[n]]=apply(SST_correct_go[[n]][,9,drop(FALSE)],2,sd)
  SST_accuracy_rate_go[[n]]=SST_number_of_correct_go[[n]]/length(SST_go_ROW[[n]])*100
  SST_all_mean_go[[n]]=as.numeric(round(colMeans(SST_go[[n]][,c(9),drop(FALSE)]),2))
  SST_all_sd_go[[n]]=apply(SST_go[[n]][,9,drop(FALSE)],2,sd)
  RT1SD1[[n]]=SST_all_mean_go[[n]]+SST_all_sd_go[[n]]
  RT1SD2[[n]]=SST_all_mean_go[[n]]+SST_all_sd_go[[n]]*2
  RT2SD1[[n]]=SST_all_mean_go[[n]]-SST_all_sd_go[[n]]
  RT2SD2[[n]]=SST_all_mean_go[[n]]-SST_all_sd_go[[n]]*2
  SST_real_meanSSD[[n]]=as.numeric(round(colMeans(SST_stop[[n]][,c(8),drop(FALSE)]),2))
  SST_set_meanSSD[[n]]=as.numeric(round(colMeans(SST_stop[[n]][,c(7),drop(FALSE)]),2))
  SST_uncanceled_rate[[n]]=SST_number_of_failed_stop[[n]]/length(SST_stop_ROW[[n]])
  SST_nthRT[[n]]=as.numeric(format(round(quantile(as.numeric (unlist(SST_go[[n]][,c(9),drop(FALSE)])), c(SST_uncanceled_rate[[n]])),2),nsmall=2))
  SST_real_SSRT[[n]]=SST_nthRT[[n]]-SST_real_meanSSD[[n]]
  SST_set_SSRT[[n]]=SST_nthRT[[n]]-SST_set_meanSSD[[n]]
  SST_uncanceled_RT[[n]]=as.numeric(round(colMeans(SST_stop_failed_stop[[n]][,c(13),drop(FALSE)]),2))
  blockorder[[n]]=as.numeric(file_content[[n]][c(1),c(29)])
  
}


data_for_plot=data.frame()    #create a variables(data frame) to contain ssd and uncanceled rate for each training sessions 
for (n in seq_along(PP_file_name_row)){
  data_for_plot[n,1]=data.frame(SSD=c(SST_set_meanSSD[[n]]))
  data_for_plot[n,2]=data.frame(uncanceled_rate=c(100*SST_uncanceled_rate[[n]]))
}  



#make a plot for next training session
par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(data_for_plot$SSD, data=data_for_plot, type="p",col = "red", ylim=c(300,550), xlim = c(1,10), xlab = "", lwd=3, ylab = "", main = 
       "")
axis(side=1, seq(0,10,1))
mtext("Training sessions", side=1, line=3, lwd=10, cex=2)
mtext("Mean SSD (ms)", side=2, line=2.5, lwd=10, col="red",cex=2)
mtext("Mean SSD and Uncanceled Rate in previous training sessions", side=3, line=2, lwd=10, cex=2)
text(data_for_plot$SSD, data=data_for_plot, labels = round(data_for_plot$SSD,0),lwd=10, col='red', cex=2,pos =1 )
par(new = TRUE)                             
plot(data_for_plot$uncanceled_rate, data=data_for_plot, type="p", col = "blue", xlab = "", ylab="",lwd=3, axes = FALSE,xlim = c(1,10))
axis(side = 4, pretty=c(0,60))
text(data_for_plot$uncanceled_rate, data=data_for_plot, labels = round(data_for_plot$uncanceled_rate,0),lwd=10,col = "blue",cex=2, pos =4 )
mtext("Uncanceled rate (%)", side = 4, line = 2.5, lwd=10, col='blue',cex=2) 


rm(SST_row,SST_go_ROW,SST_correct_go_ROW,SST_stop_ROW,SST_stop_failed_stop_ROW)








