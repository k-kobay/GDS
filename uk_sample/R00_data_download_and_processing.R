# download file name and object name
dfn <- c("SEEP", "SWEP", "CEP", "NWEP", "NEEP", "SSP", "NSP", "ESP", "NIP")

# set working directory
cd_path <- "please set absolute path for this working here"

# create foloder for download data named "dataset"
if(!"dataset" %in% list.files(cd_path)){
  dir.create(file.path(cd_path, "dataset"))
}

# a directory to save download files 
dfolder_path <- file.path(cd_path, "dataset")

# list class object for data
Data_list <- list()

# downloading and importing data
for(i in 1:length(dfn)){
  # file link
  txt_url <- paste("https://www.metoffice.gov.uk/hadobs/hadukp/data/daily/Had", dfn[i], "_daily_qc.txt", sep="")
  
  # download path
  txt_DLname <- file.path(dfolder_path, paste(dfn[i], "txt", sep="."))
  
  # downloading
  download.file(txt_url, txt_DLname)
  
  # importing
  tmp <- read.table(file=txt_DLname, skip=3)
  Data_list[[i]] <- tmp[{tmp$V1>=1931}&{tmp$V1<=2015}, ]
  
  # adding a name
  names(Data_list)[i] <- dfn[i]
}

# a day doesn't exist in leap year
omit_leap <- c(61,62,124,186,279,341)

# a day doesn't exist in non leap year
omit_nonleap  <- c(60,61,62,124,186,279,341)

# leap year list
leap_year <- 1932 + seq(0, 4*21, 4)

# creating datetime matrix "ymd"
day   <- rep(1:31, 12)
month <- rep(1:12, each=31)

for(yy in 1931:2015){ ## START FOR yy
  if(yy==1931){ ## START IF 1
      md  <- data.frame(month=month, day=day)
      ymd <- data.frame(year=yy, md)
  }else{ ## ELSE IF 1
      md  <- data.frame(month=month, day=day)
      ymd <- rbind(ymd, data.frame(year=yy, md))
  } ## END IF 1
} ## END FOR yy

# creating rainfall data matrix "rain_data"
rain_data <- sapply(Data_list, FUN=function(l){
  as.vector(t(l[,-c(1,2)]))
})

# merging datetime and rainfall data "rain_uk"
rain_uk <- data.frame(ymd, rain_data)

# exporting dataset as csv file "rain_uk.csv"
write.csv(file=file.path(dfolder_path, "rain_uk.csv"), x=rain_uk, row.names = FALSE)
