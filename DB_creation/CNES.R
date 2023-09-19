

source("DB_creation/functions.R", encoding = "UTF-8")
source("DB_creation/packages.R", encoding = "UTF-8")



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



# Substituindo os indices -------------------------------------------------

dbListTables(db, schema = "public")

ir <- read_any(db, "cnes_ir")

names(df)
df <- df %>%
  left_join(ir, by = c("COD_IR" = "codir")) %>% 
  select(-COD_IR, -descir) %>% 
  rename(id_ir = idir)

pfpj <- read_any(db, "cnes_pfpj")

df <- df %>%
  left_join(pfpj, by = c("PF_PJ" = "codpjpf")) %>% 
  select(-PF_PJ, -descpjpf) %>% 
  rename(id_pjpf = idpjpf)

atv <- read_any(db, "cnes_ativid")

df <- df %>%
  left_join(atv, by = c("ATIVIDAD" = "codatv")) %>% 
  select(-ATIVIDAD, -descatv) %>% 
  rename(id_atv = idatv)

est <- read_any(db, "cnes_tpest")

df <- df %>%
  left_join(est, by = c("TP_UNID" = "codtpest")) %>% 
  select(-TP_UNID, -desctpest) %>% 
  rename(id_tpest = idtpest)


