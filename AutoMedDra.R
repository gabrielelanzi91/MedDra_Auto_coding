#' #################################
#' [File name] AutoMedDra.R        #
#' [Author] Gabriele Lanzi         #
#' [Title] R - MedDra Auto coding  #
#' [Release date] 21-06-2017       #
#' #################################
#' For more informations about MedDra: 
#' https://www.meddra.org/


# import MedDra PT dataset 
PT <- read.table("G:/pt.asc", header=FALSE, sep="$", stringsAsFactors=FALSE)
colnames(PT) <- c("1","pt_name","2","pt_soc_code")

# import MedDra SOC dataset 
SOC <- read.table("G:/soc.asc", header=FALSE, sep="$", stringsAsFactors=FALSE)
colnames(SOC) <- c("soc_code","soc_name")

# import Medical Events dataset
data <- read.table("G:/mydata.txt", header=FALSE, col.names ='ae' , sep="$", stringsAsFactors=FALSE)


# Install the RecordLinkage package for strings comparison
#install.packages("RecordLinkage")
library(RecordLinkage)


for( a in 1:nrow(data) ){

  ma <- NULL
  
  for( b in 1:nrow(PT) ){
    
    # use the levenshteinSim index for compare PT and events strings
    ma[b] <- levenshteinSim( data$ae[a] , PT$pt_name[b]  )

  }
  
  # set a low limit for the index
  if( max(ma) > 0.5 ){ 
    
          # select the position of the highest levenshteinSim index calculated
          maxi <- which.max(ma)
	  data$PT[a] <- PT$pt_name[ maxi ]
	  # use the pt_soc_code for select the soc_name
	  soc_code_t = PT$pt_soc_code[ maxi ]
	  data$SOC[a] <- SOC$soc_name[ SOC$soc_code == soc_code_t ]
    
  } 
  else {
    
    data$PT[a] <- ""
    data$SOC[a] <- ""
    
  }
  
}

