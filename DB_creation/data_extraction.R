
source("DB_creation/packages.R", encoding = "UTF-8")
source("DB_creation/functions.R", encoding = "UTF-8")

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


# dados arquivo -----------------------------------------------------------


sp2015 <- read.dbc("../Dados/SIA_PA/PASP1501a.dbc")




# Municipios interesse ----------------------------------------------------------

con <- conecta_base()

# vamos separar os municipios da Grande sao paulo

municipios <- read.table("../Dados/GSPA.txt", header = TRUE, sep = ";")
IBGE_cidades <- read_any(con, "ibge_cidades")

#sum(municipios$municipios %in% IBGE_cidades$cidade)

municipios$municipios[!municipios$municipios %in% IBGE_cidades$cidade]
# tem 4 muinicipios que não estão com o nome certo

# vamos tentar rastrear eles
IBGE_cidades %>% 
  filter(grepl("Biritiba|Embu|Santana|Lourenço", cidade))

municipios <- municipios %>% 
  mutate(
    municipios = case_when(
      grepl("Biritiba", municipios) ~ "Biritiba-Mirim",
      grepl("Embu$", municipios) ~ "Embu das Artes",
      grepl("Santana", municipios) ~ "Santana de Parnaíba",
      grepl("Lourenço", municipios) ~ "São Lourenço da Serra",
      TRUE ~ municipios
    )
  ) %>%
  left_join(IBGE_cidades, by = c("municipios" = "cidade")) %>% 
  rename(
    id_mun = idmun,
    cidade = municipios
  )

names(municipios)

msg <- paste("CREATE TABLE mun_interesse (
  idmunint SERIAL PRIMARY KEY,
  id_mun INTEGER,
  codibge CHAR(7),
  cidade  VARCHAR(100));")

odbc::dbSendQuery(con, msg)


odbc::dbWriteTable(
  con, name = "mun_interesse", value = municipios,
  row.names = FALSE, append = TRUE
)


# Manipulando dados -------------------------------------------------------

con <- conecta_base()
arquivos <- list.files("../Dados/SIA_PA/", full.names = TRUE)

lapply(arquivos, append_sia)

# drop_table(con, "sia_pa")



# Disconecta --------------------------------------------------------------

DBI::dbDisconnect(con)
# DBI::dbDisconnect(db)
