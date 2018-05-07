
######################################################################
## Script containing minor useful functions - 
##  cat place in loop at set interval 
##  return sizes of all objects in environment
##  remove commas, dollar signs, percentage signs 
##  return vector of all observations with duplicated values
##  save workspace with timestamped name
######################################################################


#============================================================
## function to cat place in loop

itertell.fun <- function(i,denom=1){
  if(i%%denom==0){
    cat(i,"\n")
  }
}

#============================================================
#============================================================



#============================================================
## function to return sizes of all objects in workspace

# object_sizes.fun <- function(object.sizes=ls()){
#   options(scipen=999)
#   
#   object.sizes <- lapply(object.list, function(x){
#     out <- c(object.size(eval(parse(text=x)))/(1000^2),
#              object.size(eval(parse(text=x)))/(1000))
#     return(out)
#   }
#   )
#   
#   do.call("rbind",object.sizes)
#   
#   out.sizes <- as.data.frame(cbind(object.list,
#                                    do.call("rbind",object.sizes)
#   ),stringsAsFactors=F)
#   
#   out.sizes[,2] <- round(as.numeric(gsub("[[:alpha:]]","",out.sizes[,2])),digits=3)
#   out.sizes[,3] <- round(as.numeric(gsub("[[:alpha:]]","",out.sizes[,3])),digits=3)
#   out.sizes <- out.sizes[order(out.sizes[,3],decreasing=T),]
#   colnames(out.sizes) <- c("object","Mb","Kb")
#   View(out.sizes)
#   return(out.sizes)
# }

object_sizes.fun <- function(){
  options(scipen=999)
  object.list <- ls(.GlobalEnv)
  
  object.sizes <- lapply(object.list, function(x){
    out <- c(object.size(eval(parse(text=x)))/(1000^2),
             object.size(eval(parse(text=x)))/(1000))
    return(out)
  }
  )
  
  do.call("rbind",object.sizes)
  
  out.sizes <- as.data.frame(cbind(object.list,
                                   do.call("rbind",object.sizes)
  ),stringsAsFactors=F)
  
  out.sizes[,2] <- round(as.numeric(gsub("[[:alpha:]]","",out.sizes[,2])),digits=3)
  out.sizes[,3] <- round(as.numeric(gsub("[[:alpha:]]","",out.sizes[,3])),digits=3)
  out.sizes <- out.sizes[order(out.sizes[,3],decreasing=T),]
  colnames(out.sizes) <- c("object","Mb","Kb")
  View(out.sizes)
  return(out.sizes)
}


#============================================================
#============================================================


#===================================================================
## functions to remove commas, dollar signs and percentage signs

comma.rem <- function(x){
  out <- gsub(",","",x)
  return(out)
}

perc.rem <- function(x){
  x <- gsub("%","",x)
  return(x)
}

dollar.fix <- function(var){
  var <- gsub("[^[:^punct:].]","",var,perl=T)
  var <- as.numeric(as.character(var))
  return(var)
}

#============================================================
#============================================================



#==========================================================================
## returns vector of all observations which are the duplicated

alldupes.fun <- function(df,col,type="logical"){
  if(grepl("log",type,ignore.case=T) | 
     grepl("bool",type,ignore.case=T)
  ){
    cat("returning logical T/F vector of which observations belong to duplicated set\n")
    out <- df[,col] %in% 
      df[duplicated(df[,col]),col]
    return(out)
  } else if(grepl("posit",type,ignore.case=T)){
    cat("returning an integer vector of positions of observations belonging to duplicated set\n")
    out <- which(df[,col] %in% 
                   df[duplicated(df[,col]),col])
    return(out)
  } else if(grepl("val",type,ignore.case=T)){
    cat("returning values of observations belonging to duplicated set\n")
    out <- df[
      which(df[,col] %in% 
              df[duplicated(df[,col]),col])
      ,col]
    return(out)
  } else{
    cat("please choose output type of 'logical','position' or 'value'\n")
  }
}

#==========================================================================
#==========================================================================



#======================================================================================================
## saves workspace to working directory with timestamped filename using preset workspace name

workspace_save.fun <- function(){
  if(exists("workspacename")){
    workspacename.full <- paste(workspacename, 
                                format(Sys.time(), " %Y%m%d_%H%M")
                                ,".RData"
                                ,sep="")
    ## clear warnings for 
    assign("last.warning",NULL,envir=baseenv())
    
    save.image(workspacename.full)
    
    if(length(warnings())>0){
      cat("Workspace Image Save raised warnings\ncheck 'warnings()' to see detail")
    } else{
      cat("Workspace Image Successfully Saved at:\n",
          paste(getwd(),"/",workspacename,".RData",sep=""),
          "\n")
    }
  } else {
    cat("Please assign workspace name to variable 'workspacename'")
  }
}

#======================================================================================================
#======================================================================================================


## decode motherfucking VINs
vin_decode.fun <- function(VIN){
  require(RJSONIO)
  require(RCurl)
  
  target_url <- paste(
    "https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinExtended/"
    ,VIN
    ,"?format=json"
    ,sep=""
  )
  
  raw_data <- getURL(target_url)
  data.tmp <- fromJSON(raw_data)
  result <- data.tmp[[2]]
  vin.tmp <- data.tmp[[3]]
  data.list <- data.tmp[[4]]
  
  varnames <- unlist(lapply(data.list, function(x) 
    x$Variable
  )
  )
  
  varnames <- tolower(gsub(" ","_"
                           ,gsub("[[:punct:]]","_",varnames)
  )
  )
  
  values <- unlist(lapply(data.list, function(x){ 
    val <- ifelse(is.null(x$Value)
                  ,NA
                  ,x$Value
    )
    
    return(val)
  }
  )
  )
  
  out <- as.data.frame(t(values)
                       ,stringsAsFactors=F)
  colnames(out) <- varnames
  out[,"VIN"] <- vin.tmp
  return(out)
}



# Timestamp function ------------------------------------------------------
## creates timestamp in the format I use for versioned files

timestamp.fun <- function(){
  out <- format(
    Sys.time()
    ,"%Y%m%d_%H%M"
  )
  return(out)
}



# filename function -------------------------------------------------------
## creates filename in format I use for 

filename.fun <- function(path = getwd()
                         ,name = "workspace"
                         ,extension = ".RData"
){
  require(stringr)
  
  if(str_sub(path,start=-1,end=-1)!="/"){
    path = paste(path
                 ,"/"
                 ,sep="")
  }
  
  filename <- paste(
    path
    ,name
    ," "
    ,timestamp.fun()
    ,extension
    ,sep=""
  )
  return(filename)
}


# Calculate modal value ---------------------------------------------------

Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}



# Put ceiling and floor on scalar vector ----------------------------------

floor_ceiling.fun <- function(x,floor= -Inf,ceiling = Inf){
  x = ifelse(x<floor
             ,floor
             ,x
  )
  x = ifelse(x>ceiling
             ,ceiling
             ,x
  )
  return(x)
}

