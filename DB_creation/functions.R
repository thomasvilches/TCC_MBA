
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
  
  senha <-read.table("senha.txt")
  
  db <- DBI::dbConnect(RPostgres::Postgres(),
                       #Driver = "{PostgreSQL ODBC Driver(ANSI)}",
                       #Database = "DATASUS_PA",
                       dbname = "DATASUS_PA",
                       #UserName = senha$V1[1],
                       user = senha$V1[1],
                       #Password = senha$V1[2],
                       password = senha$V1[2],
                       #Servername = "localhost",
                       host = "localhost",
                       port = 5433#,
                       #encoding = "CP1252", 
                       # Encoding of R sessions, Windows R default is "CP1252" (Windows-1252)
                       #clientcharset = "UTF-8"
                       )
  return(db)
  
}


get_types_sql <- function(){
  
}



trocar_id <- function(con, df, X, Z, tabY){
  
  tab <- odbc::dbGetQuery(
    con, paste0("SELECT ", X,",",Z,  " FROM ", tabY,";")
  )
  
  df_novo <- df %>% 
    left_join(tab, by = X) %>% 
    select(-paste0(X))
  
  
  return(df_novo)
}


append_sia <- function(x){
  
  print(x)
  dados_raw <- read.dbc(x)
  
  municipios <- read_any(con, "mun_interesse")
  
  municipios$codibge_mod <- strtrim(municipios$codibge, 6)
  
  # Colunas de interesse
  
  ##
  # 1 - cod estabelecimento
  # 14 - Data do processamento
  # 15 - data da realização
  # 16 - Código do procedimento
  # 30 - CID Principal
  # 40 - Municipio de residencia (ou estabelecimento caso BPA)
  # 41 - Quantidade Produzida
  # 42 - QUantidade aprovada
  
  col_int <- c(1, 14, 15, 16, 30, 40, 41, 42)
  
  # names(dados_raw)[4]
  
  IBGE_cidades <- read_any(con, "ibge_cidades")
  
  IBGE_cidades$codibge_mod <- strtrim(IBGE_cidades$codibge, 6)
  
  IBGE_cidades <- IBGE_cidades %>% 
    select(idmun, codibge_mod)
  
  dados <- dados_raw %>% 
    select(all_of(col_int)) %>% 
    filter(PA_MUNPCN %in% municipios$codibge_mod) %>%
    rename(codcid = PA_CIDPRI) %>% 
    trocar_id(con, ., "codcid", "idcid", "cid") %>% # okay, tudo lá
    rename(id_cid_prim = idcid) %>% 
    # rename(codcid = PA_CIDSEC) %>% 
    # trocar_id(con, ., "codcid", "idcid", "cid") %>% # okay, tudo lá
    # rename(id_cid_sec = idcid, codcid = PA_CIDCAS) %>%
    # trocar_id(con, ., "codcid", "idcid", "cid") %>%# okay, alguns tem na, mas é okay
    # rename(id_cid_cas = idcid) %>% 
    left_join(IBGE_cidades, by = c("PA_MUNPCN" = "codibge_mod")) %>% 
    rename(id_mun_pct = idmun) %>% 
    # left_join(IBGE_cidades, by = c("PA_UFMUN" = "codibge_mod")) %>% 
    # rename(id_mun_est = idmun) %>% 
    rename(codproc = PA_PROC_ID) %>%
    trocar_id(con, ., "codproc", "idproc", "procedimentos") %>% 
    rename(id_proc = idproc) %>%
    # rename(codmotsai = PA_MOTSAI) %>%
    # trocar_id(con, ., "codmotsai", "idmotsai", "motsai") %>% 
    # rename(id_motsai = idmotsai) %>% 
    # rename(codcompl = PA_NIVCPL) %>%
    # mutate(codcompl = as.integer(codcompl)) %>% 
    # trocar_id(con, ., "codcompl", "idcompl", "complex") %>% 
    # rename(id_compl = idcompl) %>% 
    # rename(codraca = PA_RACACOR) %>%
    # trocar_id(con, ., "codraca", "idraca", "raca") %>% 
    # rename(id_raca = idraca) %>% 
    mutate(
      dt_proces = ymd(paste0(PA_MVM,"15")),
      dt_realiz = ymd(paste0(PA_CMP,"15"))
    ) %>% 
    # select(-PA_MVM, -PA_CMP, -PA_MUNPCN, -PA_UFMUN, -PA_TPUPS,
    #        -PA_CNPJCPF) %>% 
    select(-PA_MVM, -PA_CMP, -PA_MUNPCN) %>% 
    rename(
      cnes = PA_CODUNI,
      # tpfin = PA_TPFIN,
      # categoria = PA_CATEND,
      # sexo = PA_SEXO,
      # idade = PA_IDADE,
      # obito = PA_OBITO,
      # encer = PA_ENCERR,
      # perman = PA_PERMAN,
      # transf = PA_TRANSF,
      # alta = PA_ALTA
    ) %>% 
    mutate(
      cnes = as.character(cnes),
      # tpfin = as.character(tpfin),
      # sexo = as.character(sexo),
      # categoria = as.character(categoria),
      # obito = as.character(obito),
      # encer = as.character(encer),
      # perman = as.character(perman),
      # transf = as.character(transf),
      # alta = as.character(alta),
      # idade = as.integer(idade)
    )
  
  dados <- dados %>% 
    trocar_id(con, ., "cnes", "idcnes", "estabelecimento")
  
  
  dados <- dados %>% 
    group_by(idcnes, id_mun_pct, dt_realiz, dt_proces, id_cid_prim) %>% 
    summarise(
      pa_qtdpro = sum(PA_QTDPRO, na.rm = TRUE),
      pa_qtdaprov = sum(PA_QTDAPR, na.rm = TRUE)
    )
  
  tipos <- dbDataType(db, dados)
  
  
  msg <- paste("CREATE TABLE sia_pa (
  identrada SERIAL PRIMARY KEY,",
               paste(names(dados)," ", tipos[names(dados)], collapse = ",\n"),
               ");")
  
  
  if(!"sia_pa" %in% dbListTables(con, schema = "public")) odbc::dbSendQuery(con, msg)
  
  # read_any(con, "sia_pa")
  
  names(dados) <- tolower(names(dados))
  odbc::dbWriteTable(
    con, name = "sia_pa", value = dados,
    row.names = FALSE, append = TRUE
  )
}
