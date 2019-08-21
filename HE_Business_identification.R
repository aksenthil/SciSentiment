library(qdap)
library(tidyverse)
library(wordnet)
library(stringr)
library(koRpus)
library(textstem)
library(tm)
library(textclean)
library(hunspell)
library(data.table)
ep= read.csv("D:/HE_Bounce_prediction/HE_approval_note/customerdetails.csv", sep = ',')
# ep=data.table(ep)
# Business_Details=ep[V2=="Business Details"]
# Business_Profile=ep[V2=="Business Profile"]
# Personal_Details=ep[V2=="Personal Details"]
write.csv(ep,"D:/HE_Bounce_prediction/HE_approval_note/HE_customerdetails.csv")
# 
# a1=cbind(Business_Details,Business_Profile,Personal_Details)

colnames(ep)

ep$customerdetails <- do.call(paste, c(ep[c("Business.Details", "Business.Profile","Personal.Details")], sep = " ")) 

ep = subset(ep, select = -c(Business.Details,Business.Profile,Personal.Details) )


# ep$app_buis=rm_stopwords(ep$business)
# ep$app_buis=rm_stopwords(ep$business, tm::stopwords("english"))




# unique(ep$V2)
# ep=data.table(ep)
ep$customerdetails <- tolower(ep$customerdetails)
ep$customerdetails=lemmatize_words(ep$customerdetails)


ep$customerdetails=gsub('[\t]', ' ', ep$customerdetails)
ep$customerdetails=gsub(c('rs.'), 'rs', ep$customerdetails)
ep$customerdetails=gsub(c('mr.'), 'MR:', ep$customerdetails)
ep$customerdetails=gsub(c('ms.'), 'MS:', ep$customerdetails)
ep$customerdetails=gsub(c('mrs.'), 'MRS:', ep$customerdetails)
ep$customerdetails=str_replace(gsub("\\s+", " ", str_trim(ep$customerdetails)), "B", "b")
# ep$customerdetails=strsplit(ep$customerdetails, "\\.([^0-9])")
# ep$customerdetails=gsub('[\t]', ' ', ep$customerdetails)
# ep$customerdetails=gsub('[\]', ' ', ep$customerdetails)
str(ep$customerdetails)

# ep$customerdetails=as.factor(ep$customerdetails)

# a='income'
# for (i in 1:nrow(ep)){
#   print(i)
#   # if(a[1]=='income' | a[2]=='salary')
#   #   
#   # {
#   dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
#   ep$income[i]=Search(dat, a)
#   # }
#   # else
#   # {
#   #   print(a[2]=='salary')
#   #   ep$income[i]="NA"
#   # } 
# 
# }

a=c("income","salary","monthly","revenue","wage","earning","weekly","savings")

for (i in 1:nrow(ep)){
  tryCatch({
    
    if(grepl(a[1],ep$customerdetails[i]))
    {
      
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[1])
    }
    else if(grepl(a[2],ep$customerdetails[i]))
    {
      print(a[2])
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[2])
    }
    else if(grepl(a[3],ep$customerdetails[i]))
    {
      print(a[3])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[3])
    }
    else if(grepl(a[4],ep$customerdetails[i]))
    {
      print(a[4])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[4])
    }
    else if(grepl(a[5],ep$customerdetails[i]))
    {
      print(a[5])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[5])
    }
    else if(grepl(a[6],ep$customerdetails[i]))
    {
      print(a[6])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[6])
    }
    else if(grepl(a[7],ep$customerdetails[i]))
    {
      print(a[7])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[7])
    }
    else if(grepl(a[8],ep$customerdetails[i]))
    {
      print(a[8])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$income[i]=Search(dat, a[8])
    }
    else
    {
      print("NA")
      ep$income[i]="NA"
    }
    
  }, error=function(e){})
}

b=c("manufacturing","clinic","proprietor","business","running","employed", "entrepreneur","contractor","promoter","founder","working","owner")

for (i in 1:nrow(ep)){
  tryCatch({
    
    if(grepl(b[1],ep$customerdetails[i]))
    {
      
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[1])
    }
    else if(grepl(b[2],ep$customerdetails[i]))
    {
      
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[2])
    }
    else if(grepl(b[3],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[3])
    }
    else if(grepl(b[4],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[4])
    }
    else if(grepl(b[5],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[5])
    }
    else if(grepl(b[6],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[6])
    }
    else if(grepl(b[7],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[7])
    }
    else if(grepl(b[8],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[8])
    }
    else if(grepl(b[9],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[9])
    }
    else if(grepl(b[10],ep$customerdetails[i]))
    {
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$business[i]=Search(dat, b[10])
    }
    else
    {
      
      ep$business[i]="NA"
    }
    
  }, error=function(e){})
}

c=c('experience',"since","last","years","past","professional")

for (i in 1:nrow(ep)){
  tryCatch({
    
    if(grepl(c[1],ep$customerdetails[i]))
    {
      
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$experience[i]=Search(dat, c[1])
    }
    else if(grepl(c[2],ep$customerdetails[i]))
    {
      print(c[2])
      
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$experience[i]=Search(dat, c[2])
    }
    else if(grepl(c[3],ep$customerdetails[i]))
    {
      print(c[3])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$experience[i]=Search(dat, c[3])
    }
    else if(grepl(c[4],ep$customerdetails[i]))
    {
      print(c[4])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$experience[i]=Search(dat, c[4])
    }
    else if(grepl(c[5],ep$customerdetails[i]))
    {
      print(c[5])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$experience[i]=Search(dat, c[5])
    }
    else if(grepl(c[6],ep$customerdetails[i]))
    {
      print(c[6])
      dat <- data.frame(text=sent_detect(ep$customerdetails[i]), stringsAsFactors = FALSE)
      ep$experience[i]=Search(dat, c[6])
    }
    else
    {
      print("NA")
      ep$experience[i]="NA"
    }
    
  }, error=function(e){})
}



# Removing punctuation
ep$business=gsub('[[:punct:] ]+',' ',ep$business)
# Removing stopwords
# Adding new word in stop words
new_stops <- c("applicant", "isinto", stopwords("en"))


ep$business2=removeWords(ep$business, new_stops)
# Remove non english words
# ep$income <- ep[which(!grepl("[^\x01-\x7F]+", ep$income)),]
# Remove digits
ep$business=gsub('[[:digit:]]+', '', ep$business)
# Select only numbers
# ep$income=gsub("[^0-9.-]", "", ep$income)
# words<-c('applicant',"is","into","the")
# ep$business <- as.data.frame(sapply(ep$business, function(x) gsub(paste(words, collapse = '|'), '', x)))

str_extract( ep$income,'(rs.[0-9]+k)|(rs.[0-9]+lacs)|([0-9]+ rs)|([0-9]+ k)|(rs.[0-9]+)|([0-9]+ lac) |([0-9]+ lakhs) |([0-9]+ lakh)')
ep$years_of_exp=str_extract(ep$experience, '([0-9]+ years)|([0-9]+ yrs)')
ep$income_of_applicant=str_extract( ep$income,'(rs.[0-9]+k)|([0-9]+ rs)|([0-9]+ k)|(rs.[0-9]+)|([0-9]+ lac) |([0-9]+ lakhs) |([0-9]+ lakh)')
str_extract( ep$business,"([A-za-z]+.business.[A-za-z]+)|")



#"manufacturing","clinic","business","running","proprietor","employed", "entrepreneur","contractor","promoter","founder","working","owner"

write.csv(ep,"D:/HL/input_text_extraction.csv")
