
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

conecta_base <- function(){
  
  senha <- read.table("senha.txt")
  
  db <- DBI::dbConnect(odbc::odbc(),
                       Driver = "{PostgreSQL ODBC Driver(ANSI)}",
                       Database = "DATASUS_PA",
                       UserName = senha$V1[1],
                       Password = senha$V1[2],
                       Servername = "localhost",
                       Port = 5433,
                       encoding = "CP1252", 
                       # Encoding of R sessions, Windows R default is "CP1252" (Windows-1252)
                       clientcharset = "UTF-8")
  return(db)
  
}


get_types_sql <- function(){
  
}
