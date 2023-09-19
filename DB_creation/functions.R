
change_date <- function(x){
  
  if(is.na(x)) return(NA_Date_)
  
  if(str_count(x, ".") == 6){
    return(as.Date(paste0(x,"01"), "%Y%m%d"))
  }else{
    if(str_count(x, ".") == 8){
      return(as.Date(x, "%Y%m%d"))
    }
  }
}


read_any <- function(db, table){
  
  
  tab <- odbc::dbGetQuery(
    db, paste0("SELECT * FROM public.", table,";")
  )
  
  return(tab)
}


drop_table <- function(db, table){
  odbc::dbSendQuery(db, paste("drop table", table))
}
