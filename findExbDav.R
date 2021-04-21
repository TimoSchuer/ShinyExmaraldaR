
###based on code https://gist.github.com/viking/6910f152ad0a7d3c439e2870fbcea623
library(curl)
library(XML)##"cfndt.pbox@uni-muenster.de","Centrum2019;!@",dav="https://uni-muenster.sciebo.de/remote.php/dav/files/cfndt.pbox%40uni-muenster.de/Emsland-Projekt/Börger/Bör-07"
library(stringr)
listDirsDav("cfndt.pbox@uni-muenster.de","Centrum2019;!@",dav="https://uni-muenster.sciebo.de/remote.php/dav/files/cfndt.pbox%40uni-muenster.de/Emsland-Projekt/Börger/Bör-07")
listDirsDav <- function(username, password, relPath = "/", dav = "https://dav.box.com/dav") {
  uri <- URLencode(paste(dav, relPath, sep=""))

  # fetch directory listing via curl and parse XML response
  h <- new_handle()
  handle_setopt(h, customrequest = "PROPFIND")
  handle_setopt(h, username = username)
  handle_setopt(h, password = password)
  response <- curl_fetch_memory(uri, h)
  text <- rawToChar(response$content)
  doc <- xmlParse(text, asText=TRUE)

  # calculate relative paths
  base <- paste(stringr::str_glue(str_split(uri,"/")[[1]][1],"//",str_split(uri,"/")[[1]][3]))
  result <- unlist(
    xpathApply(doc, "//d:response/d:href", function(node) {
     stringr::str_glue(base, URLdecode(xmlValue(node)))
    })
  )
  result <- result[result != ""]
  return(result[-1])
}



browseSubDirsDav <- function(username, password, dav_list,file_list= c(),relPath= "/", type =".exb"){
  if(length(dav_list)==0){
    return(file_list)
  }else{
    result <- c()
    for (i in dav_list) {
     r <- listDirsDav(username = username, password =  password, dav= i, relPath = relPath)
     result <- append(result, r)
    }
    dav_list <- result[stringr::str_ends(result,"/")]
    k <- result[stringr::str_ends(result,str_glue("\\", type))]
    file_list <- append(file_list, k)
    if(length(dav_list)==0){
      return(file_list)
    }
    return(append(file_list,browseSubDirsDav(username=username, password = password, dav_list=dav_list,file_list =  file_list, relPath = relPath, type=type )))
  }
}
