
source("DB_creation/packages.R", encoding = "UTF-8")

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



