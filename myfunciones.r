#functions for running averages.Rmd 

###################
#Function to list files and then open them
csvFun = function(f){read.csv(f, header=FALSE, sep = ",")}

###################
#Function for preparing the data sets
prepare= function(l,b) {#(,verbose = TRUE){
  
  longTemp = list.files(l, full.names = TRUE)
  ln <<- list.files(l, full.names = FALSE)
  abrirLong = sapply(longTemp,csvFun)
  longClean <<- abrirLong %>%
    .[2:4,] %>%
    list2DF() %>%
    .[-(1:3),] 
  brevTemp = list.files(b, full.names = TRUE)
  bn <<- list.files(b, full.names = FALSE)
  abrirBrev = sapply(brevTemp,csvFun) 
  brevClean <<- abrirBrev %>%
    .[2:4,] %>%
    list2DF() %>%
    .[-(1:3),]
  longClean <<- as.data.frame(sapply(longClean, as.numeric))
  brevClean <<- as.data.frame(sapply(brevClean, as.numeric))
}

############################
# Function for naming columns of the two new files with the "specimen file name"

nombreCol = function(an){
  nombres = list()
  semiName = list()
  for (i in 1:length(an)){
    nombre = an[i]
    newNombre = nombre %>%
      stringr::str_remove(., '.fcsv') %>%
      stringr::str_remove(., '_upper_jaw_1') %>%
      stringr::str_remove(., 'jaw') %>%
      stringr::str_remove(., '_skull_') %>%
      stringr::str_remove(., 'upper') %>%
      stringr::str_remove(., '-H') %>%
      stringr::str_remove(., 'Cranium') %>%
      stringr::str_remove(., ' ' )%>%
      stringr::str_remove(., '__' )%>%
      stringr::str_remove(., '_herps')
    semiName <- append(semiName,newNombre)
    semiNames <<- semiName
    nombreX = paste(newNombre,"x",sep="-")
    nombreY = paste(newNombre,"y",sep="-")
    nombreZ = paste(newNombre,"z",sep="-")
    #nombreCoord <- list(nombreX,nombreY,nombreZ)
    nombres <- nombres %>%
      append(.,nombreX) %>%
      append(.,nombreY,) %>%
      append(.,nombreZ,)
    paraCol <<- nombres
  }
}


###########################
#Function to check if lists of files are equally long

equalLong = function(ln, bn){
  filasBn = length(bn)
  filasLn = length(ln)
  if (filasBn == filasLn){
    filas <- as.numeric(filasLn)
    print("Folders contain the same amount of files. Keep on working!!")
  }else{
    menor<<-min(filasBn,filasLn)
    mayor<<-max(filasBn,filasLn)
    print(paste(" The amount of files differ between both folders:", menor,"vs", mayor, ". Check if there are specimens missing and make folders equally long.", sep=" "))}
}


###############################
# Function to check if files have the same names so we can calculate averages 
# between the same specimens

equalName = function(xb, xl){
  nombreCompBad = vector("character", 0L)
  #xyzLn = as.vecto(xyzLn, mode = "any")
  #xyzBn = as.vector(xyzBn, mode = "any")
  filas = length(xb)
  sort(unlist(xb), decreasing = F)
  sort(unlist(xl), decreasing = F)
  for (i in seq_along(filas)){
    bnName = xb[i]
    lnName = xl[i]
    igual = (tolower(bnName) == tolower(lnName))
    if (isTRUE (igual)){
      next 
    }else{ 
      nombreCompBad[[i]] = paste("CHECK your filenames.", bnName, "(brevirostrines) and", lnName, "(longirostrines) are different",sep=" ")
      print(nombreCompBad)
    }
  }
  print(paste("Check if there are different names on the printed list. Is there not a list?? Then all names are the same in both folders!"))
}
  

#####################
#Function for calculating the averages for each x,y,z coordinates, 
#using brevClean and longClean dataframes.

#bc = brevClean
#lc = longClean

averAges = function(bc,lc){
  colLn = ncol(longClean)
  promedios = data.frame()
  for (i in 1:colLn){
    newDF = data.frame()
    newDF<- cbind(bc[,i],lc[,i])
    promedio<- data.frame(rowMeans(newDF, na.rm=TRUE))
    colnames(promedio) = colnames(bc[i])
    promedios<- (append(promedios,promedio))
    misPromed <<- as.data.frame(promedios)
    }
}



###############
###############


#############################
#############################
#############################
#############################


#something I wrote for naming columns inside "prepare()"function
#{{
##colnames(longClean)[seq(1,ncol(longClean),1)] <- nombres

#colnames(brevClean)[seq(1,ncol(brevClean),1)] <- nombres
#vbles = c(longClean,longNames,brevClean,brevNames)
#longClean <<- longClean
#longNames <<- longNames
#brevClean <<- brevClean
#brevNames <<- brevNames
#
##}}
##
###Function for splitting the Dfs (brev/longClean)into several DFs, these are the amount of files
#that I have so I can compare the same file name with two different templates **
