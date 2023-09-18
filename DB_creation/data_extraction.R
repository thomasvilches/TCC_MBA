
# Pacotes -----------------------------------------------------------------

#remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(tidyverse)



# Explorando microdatasus -------------------------------------------------

# Dicionario em https://datasus.saude.gov.br/transferencia-de-arquivos/#
dados <- fetch_datasus(year_start = 2015, year_end = 2015, month_start = 1, month_end = 1, uf = "PA", information_system = "SIA-PA")
dados <- process_sim(dados)

glimpse(dados)


id <- "0205020127"

dados %>% 
  filter(PA_PROC_ID == id, PA_UFMUN == "150130")

dados %>% 
  filter(PA_MUNPCN == "150130")

dados %>% 
  pull(PA_MVM) %>% unique

dados %>% 
  pull(PA_MVM) %>% unique

dados2 <- fetch_datasus(
  year_start = 2015, year_end = 2015,
  month_start = 11, month_end = 12, uf = "PA",
  information_system = "SIA-PA", vars = c("PA_QTDAPR", "PA_PROC_ID")
)
glimpse(dados2)




# Dados baixados ----------------------------------------------------------


dados_raw <- read.dbc("../Dados/SIA_PA/PASP1501a.dbc")

glimpse(dados_raw)

dados_raw[,8]

dados_raw %>% 
  group_by(PA_PROC_ID) %>% 
  summarise(
    n = length(unique(PA_NIVCPL))
  ) %>% filter(n>1)
unique(dados_raw$PA_NIVCPL)
unique(dados_raw$PA_MOTSAI)
unique(dados_raw$PA_OBITO)
unique(dados_raw$PA_RACACOR)

# Colunas de interesse

##
# 1 - cod estabelecimento
# 4 - cod munic estabelecimento
# 8 - tipo de estabelecimento
# 11 - CNPJ estabelecimento
# 14 - Data do processamento
# 15 - data da realização
# 16 - Código do procedimento
# 17 - Tipo de financiamento
# 19 - Complexidade do procedimento
# 24 - Motivo de saida
# 25 - Indicador de obito (APAC)
# 26 - Indicador de encerramento
# 27 - Indicador de permanencia
# 28 - Indicador de Alta
# 29 - Indicador de transferencia
# 30 - CID Principal
# 31 - CID Secundario
# 32 - CID causas associadas
# 33 - Carater atendimento
# 34 - Idade
# 38 - Sexo
# 39 - Raca/Cor
# 40 - Municipio de residencia (ou estabelecimento caso BPA)
# 41 - Quantidade Produzida
# 42 - QUantidade aprovada




# Checando CNES -----------------------------------------------------------

cnes <- fetch_datasus(year_start = 2015, year_end = 2015, month_start = 1, month_end = 4, uf = "SP", information_system = "CNES-ST")
cnes <- fetch_datasus(year_start = 2015, year_end = 2020, month_start = 1, month_end = 12, uf = "SP", information_system = "CNES-ST")

glimpse(cnes)  
  

# A FIOCRUZ possui um dicionário de variaveis melhor
# https://pcdas.icict.fiocruz.br/conjunto-de-dados/cadastro-nacional-de-estabelecimentos-de-saude/dicionario-de-variaveis/
  
# Vamos juntar leitos e salas por urgencia e emergencia, ambulatoriais, obstétricos, centro cirurgico e neonatal

colunas <- c(
  "CNES",
  "CODUFMUN",
  "COD_CEP",
  "CPF_CNPJ",
  "PF_PJ",
  "COD_IR",
  "REGSAUDE",
  "MICR_REG",
  "DISTRSAN",
  "VINC_SUS",
  "ATIVIDAD",
  "TP_UNID",
  "DT_PUBLM",
  "DT_PUBLE",
  "DT_EXPED",
  "ALVARA",
  "AV_ACRED",
  "DT_ACRED",
  "AV_PNASS",
  "DT_PNASS",
  "NIVATE_A",
  "NIVATE_H",
  "LEITHOSP",
  "URGEMERG",
  "ATENDAMB",
  "CENTRCIR",
  "CENTROBS",
  "CENTRNEO",
  "ATENDHOS",
  "ATEND_PR",
  "DT_ATUAL",
  "COMPETEN"
)

# QTLEIT
# QTINST

df <- cnes %>% 
  select(all_of(colunas), starts_with("QTLEIT"), starts_with("QTINST"))

glimpse(df)

### PF_PJ
pfpj <- data.frame(codpjpf= c("1", "3"), descpjpf = c("PF", "PJ"))


odbc::dbSendQuery(db, "CREATE TABLE cnes_pfpj (
  idPJPF SERIAL PRIMARY KEY,
  codpjpf CHAR(1),
  descpjpf  VARCHAR(30)
);")


odbc::dbWriteTable(
  db, name = "cnes_pfpj", value = pfpj,
  row.names = FALSE, append = TRUE
)


## COD_IR

unique(df$COD_IR)

ir <- data.frame(
  codir = c("10", "11", "12", "13", "14", "15", "16"),
  descir = c("Público", "Filantrópico", "Sem fins lucrativos", "Privado lucrativa simples", "Privado lucrativa", "sindical", "Pessoa física")
)


odbc::dbSendQuery(db, "CREATE TABLE cnes_ir (
  idir SERIAL PRIMARY KEY,
  codir CHAR(2),
  descir  VARCHAR(30)
);")


odbc::dbWriteTable(
  db, name = "cnes_ir", value = ir,
  row.names = FALSE, append = TRUE
)


## ATIVIDAD

atv <- data.frame(
  codatv = c("01", "02", "03", "04", "05", "99"),
  descatv = c("Unidade universitária", "Unidade escola superior isolada", "Unidade auxiliar de ensino", "Unidade sem atividade de ensino", "Hospital de ensino", "Atividade de ensino não informada")
)


odbc::dbSendQuery(db, "CREATE TABLE cnes_ativid (
  idatv SERIAL PRIMARY KEY,
  codatv CHAR(2),
  descatv  VARCHAR(50)
);")


odbc::dbWriteTable(
  db, name = "cnes_ativid", value = atv,
  row.names = FALSE, append = TRUE
)


### TP_UNID

unique(df$TP_UNID)

tp_est <- read.table("../Dados/CNES_aux/CNV/TP_ESTAB.CNV", skip = 1, sep = ";")

tp <- str_replace_all(tp_est$V1, "\\d", "") %>% trimws("both")
cod <- str_extract(tp_est$V1, "\\d+ *$") %>% trimws("both")

tp_est_f <- data.frame(desctpest = tp, codtpest = cod) %>% filter(!is.na(codtpest))


odbc::dbSendQuery(db, "CREATE TABLE cnes_tpest (
  idtpest SERIAL PRIMARY KEY,
  codtpest CHAR(2),
  desctpest  VARCHAR(100)
);")


odbc::dbWriteTable(
  db, name = "cnes_tpest", value = tp_est_f,
  row.names = FALSE, append = TRUE
)


# Ajeitando Datas ---------------------------------------------------------

df$DT_ACRED %>% unique
df$DT_PUBLE %>% unique
df$DT_ATUAL %>% unique
df$DT_EXPED %>% unique
df$COMPETEN %>% unique

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

df <- df %>% 
  rowwise() %>% 
  mutate_at(vars(contains('DT_')),
    change_date
  ) %>% ungroup()

df <- df %>% 
  rowwise() %>% 
  mutate(
    COMPETEN = change_date(COMPETEN)
  ) %>% ungroup()

View(df)



