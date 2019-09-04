# library(DescTools)
# library(FinCal)
library(data.table)
library(dplyr)
library(magrittr)
library(useful)
library(optiRum)
library(plyr)

PMT <- function(rate, nper,pv, fv=0, type=0){
  pmt = ifelse(rate!=0,
               (rate*(fv+pv*(1+ rate)^nper))/((1+rate*type)*(1-(1+ rate)^nper)),
               (-1*(fv+pv)/nper )
  )
  
  return(pmt)
}

IPMT <- function(rate, per, nper, pv, fv=0, type=0){
  ipmt = -( ((1+rate)^(per-1)) * (pv*rate + PMT(rate, nper,pv, fv=0, type=0)) - PMT(rate, nper,pv, fv=0, type=0))
  return(ipmt)
}

PPMT <- function(rate, per, nper, pv, fv=0, type=0){
  ppmt = PMT(rate, nper,pv, fv=0, type=0) - IPMT(rate, per, nper, pv, fv=0, type=0)
  return(ppmt)
}

# discount.rate(n=5,pv=0,fv=600,pmt=-100,type=0)
# dif_ipmt=fread("file:///E:/Ipmt_ppmt/Combinations.csv",header = FALSE)
dif_ipmt=fread("file:///E:/Ipmt_ppmt/Combinations.csv", header = FALSE)

Header=dif_ipmt[1,1:19]


dif_ipmt=dif_ipmt[,c(1:19)]



`Disbursements Amt (Gross)`=dif_ipmt[V3 == "Disbursements Amt (Gross)"]
`Disbursements Amt (Gross)` <- with(`Disbursements Amt (Gross)`, `Disbursements Amt (Gross)`[order(`Disbursements Amt (Gross)`$V1, `Disbursements Amt (Gross)`$V2) , ])
variable=`Disbursements Amt (Gross)`[,1:4]
# variable=`Disbursements Amt (Gross)`[1:nrow(`Disbursements Amt (Gross)`),1:4]
`Disbursements Amt (Gross)` <- data.frame(sapply(`Disbursements Amt (Gross)`[,-c(1:4)], as.numeric))

`Interest Rate (Gross)`=dif_ipmt[V3 == "Interest Rate (Gross)"]
`Interest Rate (Gross)` <- with(`Interest Rate (Gross)`, `Interest Rate (Gross)`[order(`Interest Rate (Gross)`$V1, `Interest Rate (Gross)`$V2) , ])
`Interest Rate (Gross)` <- data.frame(sapply(`Interest Rate (Gross)`[,-c(1:4)], as.numeric))

`Tenure (Months)`=dif_ipmt[V3 == "Tenure (Months)"]
`Tenure (Months)` <- with(`Tenure (Months)`, `Tenure (Months)`[order(`Tenure (Months)`$V1, `Tenure (Months)`$V2) , ])
`Tenure (Months)`<- data.frame(sapply(`Tenure (Months)`[,-c(1:4)], as.numeric))

`Fee Income Service Charges %`=dif_ipmt[V3 == "Fee Income Service Charges %"]
`Fee Income Service Charges %` <- with(`Fee Income Service Charges %`, `Fee Income Service Charges %`[order(`Fee Income Service Charges %`$V1, `Fee Income Service Charges %`$V2) , ])
`Fee Income Service Charges %` <- data.frame(sapply(`Fee Income Service Charges %`[,-c(1:4)], as.numeric))

`Avg Ticket Size`=dif_ipmt[V3=='Avg Ticket Size']
`Avg Ticket Size` <- with(`Avg Ticket Size`, `Avg Ticket Size`[order(`Avg Ticket Size`$V1, `Avg Ticket Size`$V2) , ])
`Avg Ticket Size`<- data.frame(sapply(`Avg Ticket Size`[,-c(1:4)], as.numeric))

`Doc Charges per account (Rs.)`=dif_ipmt[V3=="Doc Charges per account (Rs.)"]
`Doc Charges per account (Rs.)` <- with(`Doc Charges per account (Rs.)`, `Doc Charges per account (Rs.)`[order(`Doc Charges per account (Rs.)`$V1, `Doc Charges per account (Rs.)`$V2) , ])
`Doc Charges per account (Rs.)`<- data.frame(sapply(`Doc Charges per account (Rs.)`[,-c(1:4)], as.numeric))


`Doc Charges Efficiency %`=dif_ipmt[V3=="Doc Charges Efficiency %"]
`Doc Charges Efficiency %` <- with(`Doc Charges Efficiency %`, `Doc Charges Efficiency %`[order(`Doc Charges Efficiency %`$V1, `Doc Charges Efficiency %`$V2) , ])
`Doc Charges Efficiency %`<- data.frame(sapply(`Doc Charges Efficiency %`[,-c(1:4)], as.numeric))

`NPDC Peneteration %`=dif_ipmt[V3=="NPDC Peneteration %"]
`NPDC Peneteration %` <- with(`NPDC Peneteration %`, `NPDC Peneteration %`[order(`NPDC Peneteration %`$V1, `NPDC Peneteration %`$V2) , ])
`NPDC Peneteration %`<- data.frame(sapply(`NPDC Peneteration %`[,-c(1:4)], as.numeric))

`NPDC Charges Efficiency %`=dif_ipmt[V3=="NPDC Charges Efficiency %"]
`NPDC Charges Efficiency %` <- with(`NPDC Charges Efficiency %`, `NPDC Charges Efficiency %`[order(`NPDC Charges Efficiency %`$V1, `NPDC Charges Efficiency %`$V2) , ])
`NPDC Charges Efficiency %`<- data.frame(sapply(`NPDC Charges Efficiency %`[,-c(1:4)], as.numeric))

`NPDC Charges per account (Rs.)`=dif_ipmt[V3=="NPDC Charges per account (Rs.)"]
`NPDC Charges per account (Rs.)` <- with(`NPDC Charges per account (Rs.)`, `NPDC Charges per account (Rs.)`[order(`NPDC Charges per account (Rs.)`$V1, `NPDC Charges per account (Rs.)`$V2) , ])
`NPDC Charges per account (Rs.)`<- data.frame(sapply(`NPDC Charges per account (Rs.)`[,-c(1:4)], as.numeric))

`Roll over PDC Charges Efficiency %`=dif_ipmt[V3=="Roll over PDC Charges Efficiency %"]
`Roll over PDC Charges Efficiency %` <- with(`Roll over PDC Charges Efficiency %`, `Roll over PDC Charges Efficiency %`[order(`Roll over PDC Charges Efficiency %`$V1, `Roll over PDC Charges Efficiency %`$V2) , ])
`Roll over PDC Charges Efficiency %`<- data.frame(sapply(`Roll over PDC Charges Efficiency %`[,-c(1:4)], as.numeric))

`Roll over PDC Charges per account (Rs.)`=dif_ipmt[V3=="Roll over PDC Charges per account (Rs.)"]
`Roll over PDC Charges per account (Rs.)` <- with(`Roll over PDC Charges per account (Rs.)`, `Roll over PDC Charges per account (Rs.)`[order(`Roll over PDC Charges per account (Rs.)`$V1, `Roll over PDC Charges per account (Rs.)`$V2) , ])
`Roll over PDC Charges per account (Rs.)`<- data.frame(sapply(`Roll over PDC Charges per account (Rs.)`[,-c(1:4)], as.numeric))

`Outstation Clearing Charges Efficiency %`=dif_ipmt[V3=="Outstation Clearing Charges Efficiency %"]
`Outstation Clearing Charges Efficiency %` <- with(`Outstation Clearing Charges Efficiency %`, `Outstation Clearing Charges Efficiency %`[order(`Outstation Clearing Charges Efficiency %`$V1, `Outstation Clearing Charges Efficiency %`$V2) , ])
`Outstation Clearing Charges Efficiency %`<- data.frame(sapply(`Outstation Clearing Charges Efficiency %`[,-c(1:4)], as.numeric))

`Outstation Clearing Charges per account (Rs.)`=dif_ipmt[V3=="Outstation Clearing Charges per account (Rs.)"]
`Outstation Clearing Charges per account (Rs.)` <- with(`Outstation Clearing Charges per account (Rs.)`, `Outstation Clearing Charges per account (Rs.)`[order(`Outstation Clearing Charges per account (Rs.)`$V1, `Outstation Clearing Charges per account (Rs.)`$V2) , ])
`Outstation Clearing Charges per account (Rs.)`<- data.frame(sapply(`Outstation Clearing Charges per account (Rs.)`[,-c(1:4)], as.numeric))

`Subvention Peneteration %`=dif_ipmt[V3=="Subvention Peneteration %"]
`Subvention Peneteration %` <- with(`Subvention Peneteration %`, `Subvention Peneteration %`[order(`Subvention Peneteration %`$V1, `Subvention Peneteration %`$V2) , ])
`Subvention Peneteration %`<- data.frame(sapply(`Subvention Peneteration %`[,-c(1:4)], as.numeric))

`Manufacturer Incentive %`=dif_ipmt[V3=="Manufacturer Incentive %"]
`Manufacturer Incentive %` <- with(`Manufacturer Incentive %`, `Manufacturer Incentive %`[order(`Manufacturer Incentive %`$V1, `Manufacturer Incentive %`$V2) , ])
`Manufacturer Incentive %`<- data.frame(sapply(`Manufacturer Incentive %`[,-c(1:4)], as.numeric))

`Dealer Peneteration %`=dif_ipmt[V3=="Dealer Peneteration %"]
`Dealer Peneteration %` <- with(`Dealer Peneteration %`, `Dealer Peneteration %`[order(`Dealer Peneteration %`$V1, `Dealer Peneteration %`$V2) , ])
`Dealer Peneteration %`<- data.frame(sapply(`Dealer Peneteration %`[,-c(1:4)], as.numeric))

`Dealer Payout %`=dif_ipmt[V3=="Dealer Payout %"]
`Dealer Payout %` <- with(`Dealer Payout %`, `Dealer Payout %`[order(`Dealer Payout %`$V1, `Dealer Payout %`$V2) , ])
`Dealer Payout %`<- data.frame(sapply(`Dealer Payout %`[,-c(1:4)], as.numeric))

`Sourcing Cost Incentive %`=dif_ipmt[V3=="Sourcing Cost Incentive %"]
`Sourcing Cost Incentive %` <- with(`Sourcing Cost Incentive %`, `Sourcing Cost Incentive %`[order(`Sourcing Cost Incentive %`$V1, `Sourcing Cost Incentive %`$V2) , ])
`Sourcing Cost Incentive %`<- data.frame(sapply(`Sourcing Cost Incentive %`[,-c(1:4)], as.numeric))

rm(dif_ipmt)

dis_amount=data.table(t(`Disbursements Amt (Gross)`))

int_no=data.frame(seq(1,ncol(dis_amount), length=ncol(dis_amount)))
int_no<- data.frame(sapply(int_no, as.numeric))

irr_rate=data.table(t(`Interest Rate (Gross)`))
tenure_no=data.table(t(`Tenure (Months)`))
int_no <- data.table(sapply(int_no, as.numeric))
dis_amount <- data.table(sapply(dis_amount, as.numeric))
irr_rate <- data.table(sapply(irr_rate, as.numeric))
tenure_no <- data.table(sapply(tenure_no, as.numeric))



`Disbursements file count`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) round((`Disbursements Amt (Gross)`[l,])/(`Avg Ticket Size`[l,])))
`Disbursements file count` <- data.frame(matrix(unlist(`Disbursements file count`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`PDC Peneteration %`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) 1-`NPDC Peneteration %`[l,])
`PDC Peneteration %` <- data.frame(matrix(unlist(`PDC Peneteration %`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Fee Income Service Charges - Net of tax`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) (`Disbursements Amt (Gross)`[l,])*(`Fee Income Service Charges %`[l,]))
`Fee Income Service Charges - Net of tax` <- data.frame(matrix(unlist(`Fee Income Service Charges - Net of tax`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Doc Charges`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) ((`Disbursements file count`[l,]) *(`Doc Charges per account (Rs.)`[l,]) * (`Doc Charges Efficiency %`[l,]))/100000)
`Doc Charges` <- data.frame(matrix(unlist(`Doc Charges`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`NPDC Charges`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) ((`Disbursements file count`[l,]) *(`NPDC Peneteration %`[l,]) * (`NPDC Charges Efficiency %`[l,]) * (`NPDC Charges per account (Rs.)`[l,]))/100000)
`NPDC Charges` <- data.frame(matrix(unlist(`NPDC Charges`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Roll over PDC Charges`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) ((`Disbursements file count`[l,])*(`PDC Peneteration %`[l,])*(`Roll over PDC Charges Efficiency %`[l,])*(`Roll over PDC Charges per account (Rs.)`[l,]))/100000)
`Roll over PDC Charges` <- data.frame(matrix(unlist(`Roll over PDC Charges`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Outstation Clearing Charges`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) ((`Disbursements file count`[l,])*(`PDC Peneteration %`[l,])*(`Outstation Clearing Charges Efficiency %`[l,])*(`Outstation Clearing Charges per account (Rs.)`[l,]))/100000)
`Outstation Clearing Charges` <- data.frame(matrix(unlist(`Outstation Clearing Charges`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Manufacture Incentives`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) (`Disbursements Amt (Gross)`[l,])*(`Subvention Peneteration %`[l,])*(`Manufacturer Incentive %`[l,]))
`Manufacture Incentives` <- data.frame(matrix(unlist(`Manufacture Incentives`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Sourcing Cost - External Payout`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) ((`Disbursements Amt (Gross)`[l,])*(`Dealer Peneteration %`[l,])*(`Dealer Payout %`[l,])))
`Sourcing Cost - External Payout` <- data.frame(matrix(unlist(`Sourcing Cost - External Payout`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Salaries Cost - Sales - Off Roll - Incentives`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) ((`Disbursements Amt (Gross)`[l,])*(`Sourcing Cost Incentive %`[l,])))
`Salaries Cost - Sales - Off Roll - Incentives` <- data.frame(matrix(unlist(`Salaries Cost - Sales - Off Roll - Incentives`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Unamort Upfron Income (Net)`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) (`Fee Income Service Charges - Net of tax`[l,] + `Doc Charges`[l,] + `NPDC Charges`[l,] + `Roll over PDC Charges`[l,] + `Outstation Clearing Charges`[l,] + `Manufacture Incentives`[l,])-(`Sourcing Cost - External Payout`[l,] + `Salaries Cost - Sales - Off Roll - Incentives`[l,]))
`Unamort Upfron Income (Net)` <- data.frame(matrix(unlist(`Unamort Upfron Income (Net)`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`Disbursements Amt (Net)`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) (`Disbursements Amt (Gross)`[l,])-(`Fee Income Service Charges - Net of tax`[l,] + `Doc Charges`[l,] +`NPDC Charges`[l,] + `Roll over PDC Charges`[l,]+ `Outstation Clearing Charges`[l,] + `Manufacture Incentives`[l,])+(`Sourcing Cost - External Payout`[l,] + `Salaries Cost - Sales - Off Roll - Incentives`[l,]))
`Disbursements Amt (Net)` <- data.frame(matrix(unlist(`Disbursements Amt (Net)`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`PMT_RATE`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) PMT(((`Interest Rate (Gross)`[l,])/12),`Tenure (Months)`[l,],`Disbursements Amt (Gross)`[l,],0))
`PMT_RATE` <- data.frame(matrix(unlist(`PMT_RATE`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
`PMT_RATE` <- data.frame(sapply(`PMT_RATE`, as.numeric))
`Disbursements Amt (Net)` <- data.frame(sapply(`Disbursements Amt (Net)`, as.numeric))
`Interest Rate (EIR)`=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) RATE(unlist(`Tenure (Months)`[l,]),unlist(`PMT_RATE`[l,]),unlist(`Disbursements Amt (Net)`[l,]))*12)
`Interest Rate (EIR)` <- data.frame(matrix(unlist(`Interest Rate (EIR)`), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))

`New Fee Income Service Charges - Net of tax`=`Fee Income Service Charges - Net of tax`/`Unamort Upfron Income (Net)`
`New Doc Charges`= `Doc Charges`/`Unamort Upfron Income (Net)`
`New NPDC Charges`=`NPDC Charges`/`Unamort Upfron Income (Net)`
`New Roll over PDC Charges`= `Roll over PDC Charges`/`Unamort Upfron Income (Net)`
`New Outstation Clearing Charges`= `Outstation Clearing Charges`/`Unamort Upfron Income (Net)`
`New Manufacture Incentives`= `Manufacture Incentives`/`Unamort Upfron Income (Net)`
`New Sourcing Cost - External Payout`= `Sourcing Cost - External Payout`/`Unamort Upfron Income (Net)`
`New Salaries Cost - Sales - Off Roll - Incentives`= `Salaries Cost - Sales - Off Roll - Incentives`/`Unamort Upfron Income (Net)`
`New Unamort Upfron Income (Net)` = `Unamort Upfron Income (Net)`/`Unamort Upfron Income (Net)`

dis_amount2=data.table(t(`Disbursements Amt (Net)`))
irr_rate2=data.table(t(`Interest Rate (EIR)`))
tenure_no2=data.table(t(`Tenure (Months)`))

dis_amount2 <- data.table(sapply(dis_amount2, as.numeric))
irr_rate2 <- data.table(sapply(irr_rate2, as.numeric))
tenure_no2 <- data.table(sapply(tenure_no2, as.numeric))

IPMT_breakup=data.table()
`Final IPMT - Fee Income Service Charges - Net of tax`=data.table()
`Final IPMT - Doc Charges`=data.table()
`Final IPMT - NPDC Charges`=data.table()
`Final IPMT - Roll over PDC Charges`=data.table()
`Final IPMT - Outstation Clearing Charges`=data.table()
`Final IPMT - Manufacture Incentives`=data.table()
`Final IPMT - Sourcing Cost - External Payout`=data.table()
`Final IPMT - Salaries Cost - Sales - Off Roll - Incentives`=data.table()



for (i in 1:ncol(tenure_no)){
  print(i)
  # i=1
  result_IPMT=lapply(1:nrow(dis_amount), function(j) -(IPMT(unlist((irr_rate[,..i])/12),j,unlist(tenure_no[,..i]),unlist(dis_amount[,..i]))))
  result_IPMT <- data.table(matrix(unlist(result_IPMT), nrow=nrow(dis_amount), byrow=T),stringsAsFactors=FALSE)
  
  result_IPMT1=lapply(1:nrow(tenure_no), function(j) -(IPMT(unlist((irr_rate2[,..i])/12),j,unlist(tenure_no2[,..i]),unlist(dis_amount2[,..i]))))
  result_IPMT1 <- data.table(matrix(unlist(result_IPMT1), nrow=nrow(dis_amount2), byrow=T),stringsAsFactors=FALSE)
  result_IPMT=data.table(t(result_IPMT))
  result_IPMT1=data.table(t(result_IPMT1))

  result_IPMT=lapply(1:nrow(dis_amount), function(m) ifelse(int_no>unlist(tenure_no[m,..i]),0,unlist(result_IPMT[,..m])))
  result_IPMT <- data.table(matrix(unlist(result_IPMT), nrow=nrow(dis_amount), byrow=T),stringsAsFactors=FALSE)
  
  result_IPMT1=lapply(1:nrow(dis_amount2), function(m) ifelse(int_no>unlist(tenure_no2[m,..i]),0,unlist(result_IPMT1[,..m])))
  result_IPMT1 <- data.table(matrix(unlist(result_IPMT1), nrow=nrow(dis_amount2), byrow=T),stringsAsFactors=FALSE)
  # result_IPMT=data.table(t(result_IPMT))
  # result_IPMT1=data.table(t(result_IPMT1))
  
  result_IPMT=lapply(1:nrow(dis_amount), function(l) shift(result_IPMT[,..l], n=l-1, fill=0, type="lag"))
  result_IPMT <- data.table(matrix(unlist(result_IPMT), nrow=nrow(dis_amount), byrow=T),stringsAsFactors=FALSE)
  # result_IPMT=data.table(t(result_IPMT))
  # result_IPMT1=data.table(t(result_IPMT1))
  result_IPMT1=lapply(1:nrow(dis_amount2), function(l) shift(result_IPMT1[,..l], n=l-1, fill=0, type="lag"))
  result_IPMT1 <- data.table(matrix(unlist(result_IPMT1), nrow=nrow(dis_amount2), byrow=T),stringsAsFactors=FALSE)
  
  
  # 
  diff_IPMT_break=lapply(1:nrow(`result_IPMT1`), function(l) (result_IPMT1[l,])-result_IPMT[l,])
  diff_IPMT_break <- data.table(matrix(unlist(diff_IPMT_break), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  diff_IPMT_break=as.data.frame(diff_IPMT_break)
  
  `IPMT - Fee Income Service Charges - Net of tax`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Fee Income Service Charges - Net of tax`[i,l]))
  `IPMT - Fee Income Service Charges - Net of tax` <- data.table(matrix(unlist(`IPMT - Fee Income Service Charges - Net of tax`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  `IPMT - Doc Charges`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Doc Charges`[i,l]))
  `IPMT - Doc Charges` <- data.table(matrix(unlist(`IPMT - Doc Charges`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  

  `IPMT - NPDC Charges`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New NPDC Charges`[i,l]))
  `IPMT - NPDC Charges` <- data.table(matrix(unlist(`IPMT - NPDC Charges`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  
  `IPMT - Roll over PDC Charges`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Roll over PDC Charges`[i,l]))
  `IPMT - Roll over PDC Charges` <- data.table(matrix(unlist(`IPMT - Roll over PDC Charges`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  
  
  `IPMT - Outstation Clearing Charges`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Outstation Clearing Charges`[i,l]))
  `IPMT - Outstation Clearing Charges` <- data.table(matrix(unlist(`IPMT - Outstation Clearing Charges`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  
  `IPMT - Manufacture Incentives`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Manufacture Incentives`[i,l]))
  `IPMT - Manufacture Incentives` <- data.table(matrix(unlist(`IPMT - Manufacture Incentives`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  `IPMT - Sourcing Cost - External Payout`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Sourcing Cost - External Payout`[i,l]))
  `IPMT - Sourcing Cost - External Payout` <- data.table(matrix(unlist(`IPMT - Sourcing Cost - External Payout`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  
  `IPMT - Salaries Cost - Sales - Off Roll - Incentives`=lapply(1:nrow(`result_IPMT1`), function(l) (diff_IPMT_break[l,])*(`New Salaries Cost - Sales - Off Roll - Incentives`[i,l]))
  `IPMT - Salaries Cost - Sales - Off Roll - Incentives` <- data.table(matrix(unlist(`IPMT - Salaries Cost - Sales - Off Roll - Incentives`), nrow=nrow(result_IPMT1), byrow=T),stringsAsFactors=FALSE)
  
  
  diff_IPMT_break=data.table(t(colSums(diff_IPMT_break))) 
  `IPMT - Fee Income Service Charges - Net of tax`=data.table(t(colSums(`IPMT - Fee Income Service Charges - Net of tax`)))
  `IPMT - Doc Charges`=data.table(t(colSums(`IPMT - Doc Charges`))) 
  
  `IPMT - Roll over PDC Charges`=data.table(t(colSums(`IPMT - Roll over PDC Charges`))) 
  `IPMT - NPDC Charges`=data.table(t(colSums(`IPMT - NPDC Charges`)))
  `IPMT - Outstation Clearing Charges`=data.table(t(colSums(`IPMT - Outstation Clearing Charges`)))
  `IPMT - Manufacture Incentives`=data.table(t(colSums(`IPMT - Manufacture Incentives`)))
  `IPMT - Sourcing Cost - External Payout`=data.table(t(colSums(`IPMT - Sourcing Cost - External Payout`))) 
  `IPMT - Salaries Cost - Sales - Off Roll - Incentives`=data.table(t(colSums(`IPMT - Salaries Cost - Sales - Off Roll - Incentives`)))
  
  IPMT_breakup=rbindlist(list(IPMT_breakup,diff_IPMT_break))
  `Final IPMT - Fee Income Service Charges - Net of tax`=rbindlist(list(`Final IPMT - Fee Income Service Charges - Net of tax`,`IPMT - Fee Income Service Charges - Net of tax`))
  `Final IPMT - Doc Charges`=rbindlist(list(`Final IPMT - Doc Charges`,`IPMT - Doc Charges`))
  `Final IPMT - Roll over PDC Charges`=rbindlist(list(`Final IPMT - Roll over PDC Charges`,`IPMT - Roll over PDC Charges`))
  `Final IPMT - NPDC Charges`=rbindlist(list(`Final IPMT - NPDC Charges`,`IPMT - NPDC Charges`))
  `Final IPMT - Outstation Clearing Charges`=rbindlist(list(`Final IPMT - Outstation Clearing Charges`,`IPMT - Outstation Clearing Charges`))
  `Final IPMT - Manufacture Incentives`=rbindlist(list(`Final IPMT - Manufacture Incentives`,`IPMT - Manufacture Incentives`))
  `Final IPMT - Sourcing Cost - External Payout`=rbindlist(list(`Final IPMT - Sourcing Cost - External Payout`,`IPMT - Sourcing Cost - External Payout`))
  `Final IPMT - Salaries Cost - Sales - Off Roll - Incentives`=rbindlist(list(`Final IPMT - Salaries Cost - Sales - Off Roll - Incentives`,`IPMT - Salaries Cost - Sales - Off Roll - Incentives`))
  
}





# diff_IPMT=lapply(1:nrow(`Disbursements Amt (Gross)`), function(l) (IPMT_second[l,])-IPMT_first[l,])
# 
# `IPMT - New Book -Differential` <- data.table(matrix(unlist(diff_IPMT), nrow=nrow(`Disbursements Amt (Gross)`), byrow=T))
# `IPMT - New Book - Interest rate (Gross)` <- data.frame(IPMT_first)
# `IPMT - New Book - Interest rate (EIR)` <- data.frame(IPMT_second)
# # variable[,3]="Disbursements file count"


# `Disbursements file count`=cbind(Account="Disbursements file count", `Disbursements file count`)
variable[,3]="Disbursements file count"
`Disbursements file count` <- cbind(variable, `Disbursements file count`)

variable[,3]="PDC Peneteration %"
`PDC Peneteration %` <- cbind(variable, `PDC Peneteration %`)

variable[,3]="Fee Income Service Charges - Net of tax"
`Fee Income Service Charges - Net of tax` <- cbind(variable, `Fee Income Service Charges - Net of tax`)

variable[,3]="Doc Charges"
`Doc Charges` <- cbind(variable, `Doc Charges`)

variable[,3]= "NPDC Charges"
`NPDC Charges` <- cbind(variable, `NPDC Charges`)

variable[,3]="Roll over PDC Charges"
`Roll over PDC Charges` <- cbind(variable, `Roll over PDC Charges`)

variable[,3]="Outstation Clearing Charges"
`Outstation Clearing Charges` <- cbind(variable, `Outstation Clearing Charges`)

variable[,3]="Manufacture Incentives"
`Manufacture Incentives` <- cbind(variable, `Manufacture Incentives`)
variable[,3]="Sourcing Cost - External Payouts"
`Sourcing Cost - External Payout` <- cbind(variable, `Sourcing Cost - External Payout`)
variable[,3]="Salaries Cost - Sales - Off Roll - Incentives"
`Salaries Cost - Sales - Off Roll - Incentives` <- cbind(variable, `Salaries Cost - Sales - Off Roll - Incentives`)
variable[,3]="Unamort Upfron Income (Net)"
`Unamort Upfron Income (Net)` <- cbind(variable, `Unamort Upfron Income (Net)`)
variable[,3]="Disbursements Amt (Net)"
`Disbursements Amt (Net)` <- cbind(variable, `Disbursements Amt (Net)`)
variable[,3]="PMT_RATE"
`PMT_RATE` <- cbind(variable, `PMT_RATE`)
variable[,3]="Interest Rate (EIR)"
`Interest Rate (EIR)` <- cbind(variable, `Interest Rate (EIR)`)
# variable[,3]="IPMT - New Book - Interest rate (Gross)"
# `IPMT - New Book - Interest rate (Gross)` <- cbind(variable, `IPMT - New Book - Interest rate (Gross)`)
# variable[,3]="IPMT - New Book - Interest rate (EIR)"
# `IPMT - New Book - Interest rate (EIR)` <- cbind(variable, `IPMT - New Book - Interest rate (EIR)`)
# variable[,3]="IPMT - New Book -Differential"
# `IPMT - New Book -Differential` <- cbind(variable, `IPMT - New Book -Differential`)


variable[,3]= "IPMT - Doc Charges"
`Final IPMT - Doc Charges` <- cbind(variable, `Final IPMT - Doc Charges`)

variable[,3]= "IPMT - NPDC Charges"
`Final IPMT - NPDC Charges` <- cbind(variable, `Final IPMT - NPDC Charges`)

variable[,3]= "IPMT - Fee Income Service Charges - Net of tax"
`Final IPMT - Fee Income Service Charges - Net of tax` <- cbind(variable, `Final IPMT - Fee Income Service Charges - Net of tax`)

variable[,3]= "IPMT - Roll over PDC Charges"
`Final IPMT - Roll over PDC Charges` <- cbind(variable, `Final IPMT - Roll over PDC Charges`)

variable[,3]= "IPMT - Outstation Clearing Charges"
`Final IPMT - Outstation Clearing Charges` <- cbind(variable, `Final IPMT - Outstation Clearing Charges`)

variable[,3]= "IPMT - Manufacture Incentives"
`Final IPMT - Manufacture Incentives` <- cbind(variable, `Final IPMT - Manufacture Incentives`)

variable[,3]= "IPMT - Salaries Cost - Sales - Off Roll - Incentives"
`Final IPMT - Salaries Cost - Sales - Off Roll - Incentives` <- cbind(variable, `Final IPMT - Salaries Cost - Sales - Off Roll - Incentives`)

variable[,3]= "IPMT - Sourcing Cost - External Payout"
`Final IPMT - Sourcing Cost - External Payout` <- cbind(variable, `Final IPMT - Sourcing Cost - External Payout`)



final_output=rbindlist(list(`Disbursements file count`,`PDC Peneteration %`,
                            `Fee Income Service Charges - Net of tax`,`Doc Charges`,
                            `NPDC Charges`,
                            `Roll over PDC Charges`,
                            `Outstation Clearing Charges`,`Manufacture Incentives`,
                            `Sourcing Cost - External Payout`,
                            `Salaries Cost - Sales - Off Roll - Incentives`,
                            `Unamort Upfron Income (Net)`,
                            `Disbursements Amt (Net)`,`PMT_RATE`,`Interest Rate (EIR)`,
                            
                            `Final IPMT - Doc Charges`,
                            `Final IPMT - Fee Income Service Charges - Net of tax`,
                            `Final IPMT - Manufacture Incentives`,
                            `Final IPMT - NPDC Charges`,
                            `Final IPMT - Outstation Clearing Charges`,
                            `Final IPMT - Roll over PDC Charges`,
                            `Final IPMT - Sourcing Cost - External Payout`,
                            `Final IPMT - Salaries Cost - Sales - Off Roll - Incentives`
                            
                            
))
final_result <- rbindlist(list(Header,final_output))

colnames(final_result) <- as.character(unlist(final_result[1,]))
final_result=final_result[-1,]
write.csv(final_result,"file:///E:/Ipmt_ppmt/breakup_result.csv")
