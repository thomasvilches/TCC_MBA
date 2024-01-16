

source("packages.R", encoding = "UTF-8")
source("functions.R", encoding = "UTF-8")


db <- conecta_base()


# Criando tabelas auxiliares ----------------------------------------------

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


# Checando CNES -----------------------------------------------------------

cnes <- fetch_datasus(year_start = 2015, year_end = 2015, month_start = 1, month_end = 4, uf = "SP", information_system = "CNES-ST")
# cnes <- fetch_datasus(year_start = 2015, year_end = 2020, month_start = 1, month_end = 12, uf = "SP", information_system = "CNES-ST")

glimpse(cnes)  


# A FIOCRUZ possui um dicionário de variaveis melhor
# https://pcdas.icict.fiocruz.br/conjunto-de-dados/cadastro-nacional-de-estabelecimentos-de-saude/dicionario-de-variaveis/

# Vamos juntar leitos e salas por urgencia e emergencia, ambulatoriais, obstétricos, centro cirurgico e neonatal



# -------------------------------------------------------------------------


# Baixei os dados e vou fazer a extração de todos

files <- list.files("../Dados/CNES_2015_2020/", full.names = TRUE, pattern = ".\\.dbc")


entra_cnes(1, files)
entra_cnes(2, files)
entra_cnes(3, files)
entra_cnes(4, files)
entra_cnes(5, files)
entra_cnes(6, files)

Ano = 6
files[(12*(Ano-1)+1):(12*Ano)]


# read_any(db, "cnes_data")

# drop_table(db, "cnes_data")


# ajeitando ---------------------------------------------------------------


odbc::dbSendQuery(db, "CREATE TABLE estabelecimento (
  idcnes SERIAL PRIMARY KEY,
  cnes CHAR(7),
  id_mun  INTEGER
);")

read_any(db, "cnes_data") %>%
  group_by(cnes) %>% 
  summarise(
    id_mun = unique(id_mun)
  ) %>% 
odbc::dbWriteTable(
  db, name = "estabelecimento", value = .,
  row.names = FALSE, append = TRUE
)

# drop_table(db, "estabelecimento")
# 
# ALTER TABLE cnes_data
# DROP COLUMN id_mun;
# 
#  ALTER TABLE cnes_data
#  ADD COLUMN id_cnes INTEGER;
# 
#  -- Atualiza a nova coluna com os dados da tabela de origem usando um JOIN
#  UPDATE cnes_data
#  SET id_cnes = estabelecimento.idcnes
#  FROM estabelecimento
#  WHERE cnes_data.cnes = estabelecimento.cnes;
# 
#  ALTER TABLE cnes_data
#  DROP COLUMN id_mun;
# 
#  ALTER TABLE cnes_data
#  DROP COLUMN cnes;


# Tabela por ano ----------------------------------------------------------

cnes <- read_any(db, "cnes_data")

nrow(cnes)
names(cnes)

cnes %>% 
  mutate(
    Ano = year(competen)
  ) %>% 
  group_by(id_cnes, Ano) %>% 
  mutate(
    n = length(unique(id_atv))
  ) %>% filter(n > 1) %>% arrange(id_cnes, competen)

df1 <- cnes %>% 
  mutate(
    Ano = year(competen)
  ) %>% 
  group_by(id_cnes, Ano) %>% 
  summarise_at(vars(starts_with("qt")), list(~ mean(., na.rm = TRUE)))


df2 <- cnes %>% 
  mutate(
    Ano = year(competen)
  ) %>% 
  group_by(id_cnes, Ano) %>%
  summarise_at(vars(c(vinc_sus, nivate_a, nivate_h,
                   leithosp, urgemerg, atendamb,
                   centrcir, centrobs, centrneo,
                   atendhos)),
            list(~ max(as.integer(.), na.rm = TRUE))
  )

df <- full_join(df1, df2)

nrow(df1)
nrow(df2)
nrow(df)


names(df) <- tolower(names(df))
tipos <- dbDataType(db, df)

df <- df %>% mutate(ano = as.integer(ano))
glimpse(df)

msg <- paste("CREATE TABLE cnes_data_year (
  idcnesdatayear SERIAL PRIMARY KEY,",
             paste(names(df)," ", tipos[names(df)], collapse = ",\n"),
             ");")


# cat(msg)
odbc::dbSendQuery(db, msg)

odbc::dbWriteTable(
  db, name = "cnes_data_year", value = df,
  row.names = FALSE, append = TRUE
)


df %>% select(id_cnes, ano) %>% unique %>% nrow

# Disconecta --------------------------------------------------------------

DBI::dbDisconnect(db)
