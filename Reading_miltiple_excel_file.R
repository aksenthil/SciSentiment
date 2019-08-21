library(xlsx)
# library(xlsx2)
library("readxl")
setwd("D:/Text_mining/Auditreports/New folder1/")
files=list.files(pattern=".xls")
CRdata=data.frame()
dat=data.frame()
for(i in 1:length(files))
# i=1
{
  print(i)
  
  filename=files[i]
  print(filename)
  sheets <-excel_sheets(filename)
  # sheets <- readxl::excel_sheets(filename)
  
  for(x in sheets[grepl("critical", sheets) == T|grepl("Critical", sheets) == T]){
    print(x)
    data <- readxl::read_excel(filename, x,col_names = FALSE)
    # data <- read.xlsx(file = filename,x,header=FALSE)
    
  }
  print(filename)
  filename=gsub(".xlsx","",filename)
  # filename=gsub(" ","_",filename)
  # data=assign(x = filename,value = data)
  # filename=tolower(filename)
  rc=which( data == "Critical Observation"| data =="Critical Observations"|data=='Observation'|data== "CRITICAL OBSERVATION", arr.ind = TRUE)
  CRdata=data[, rc[2]]
  CRdata[,2]=filename
  colnames(CRdata) <- c("Critical_Observation","Branch")
  dat <- rbind(dat, CRdata)
}
input_dat <- rbind(dat2, dat)
# write.csv(input_dat,"input_for_critical_observation.csv")
AGREEMENTNO,PROPOSALID,STATUS,PRODUCTFLAG,CUSTOMERID,CUSTOMERNAME ,
CONSTID,AGE,INDV_CORP_FLAG,CUST_RATING,BRANCHID,FNAME,DOB,SEX,MARITAL_STATUS,
QUALIFICATION,INCOME_SOURCE,PROFESSIONCODE,TITLE,MC_STATUS,MAKERID,MAKERDATE,
AUTHID,AUTHDATE,ACCOTYPE,ACCOCATG,CIF_NO,DATELASTUPDT,CREDIT_FLAG,MINOR,MAS_CODE,CHOLA_GROUPID,UCIC