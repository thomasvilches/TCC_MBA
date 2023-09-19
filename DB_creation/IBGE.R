

source("DB_creation/functions.R", encoding = "UTF-8")
source("DB_creation/packages.R", encoding = "UTF-8")


# data --------------------------------------------------------------------

# This file uses the data taken from census 2010
# the data can be found at https://www.ibge.gov.br/estatisticas/sociais/populacao/9662-censo-demografico-2010.html?edicao=10411&t=downloads
# following the path 
# Censo_Demografico_2010 > Resultados_do_Universo > ods > Municipios > saopaulo_**.zip


# Dados -------------------------------------------------------------------

# Vamos criar uma tabela de referência para as cidades e seus nomes

# vou extrair da própria tabela de sexo


dados <- readxl::read_xls("../Dados/sao_paulo_20190207/Tabela 4.20.1.1.xls", sheet = 1, range = "A90:K3748", col_names = FALSE)

names(dados) <- c("cidade", "total", "homens", "mulheres", "urb_total", "urb_homens", "urb_mulheres", "rur_total", "rur_homens", "rur_mulheres", "codibge")

glimpse(dados)

dados <- dados %>% 
  mutate(
    codibge  = as.character(codibge)
  ) %>% 
  filter(str_length(codibge) == 7)


df <- dados %>% 
  select(cidade, codibge) %>% 
  unique


msg <- paste("CREATE TABLE ibge_cidades (
  idmun SERIAL PRIMARY KEY,
  codibge CHAR(7),
  cidade  VARCHAR(100));")

odbc::dbSendQuery(db, msg)


odbc::dbWriteTable(
  db, name = "ibge_cidades", value = df,
  row.names = FALSE, append = TRUE
)

read_any(db, "ibge_cidades")

# Homens/Mulheres

dados <- readxl::read_xls("../Dados/sao_paulo_20190207/Tabela 4.20.1.1.xls", sheet = 1, range = "A90:K1769", col_names = FALSE)

names(dados) <- c("cidade", "total", "homens", "mulheres", "urb_total", "urb_homens", "urb_mulheres", "rur_total", "rur_homens", "rur_mulheres", "codibge")

glimpse(dados)

dados <- dados %>% 
  mutate(
    codibge  = as.character(codibge)
  ) %>% 
  filter(str_length(codibge) == 7)


dados <- dados %>% 
  mutate_at(vars(contains('urb_')),
            as.integer
  )

dados <- dados %>% 
  mutate_at(vars(contains('rur_')),
            as.integer
  )

dados <- dados %>% 
  mutate_at(vars(c(2, 3, 4)),
            as.integer
  )

nrow(dados)


cities <- read_any(db, "ibge_cidades")

names(cities)

dados <- dados %>% 
  left_join(cities, by = "codibge") %>% 
  select(-codibge, -cidade.x, -cidade.y) %>% 
  rename(id_mun = idmun)

msg <- paste("CREATE TABLE ibge_sex (
  idmunsex SERIAL PRIMARY KEY,
  id_mun INTEGER,",
  paste(names(dados)[seq(1, 9)], " INTEGER", collapse = ","),
");")

odbc::dbSendQuery(db, msg)

read_any(db, "ibge_sex")

odbc::dbWriteTable(
  db, name = "ibge_sex", value = dados,
  row.names = FALSE, append = TRUE
)

# drop_table(db, "ibge_sex")

# Grupos de idade


dados <- readxl::read_xls("../Dados/sao_paulo_20190207/Tabela 4.20.1.2.xls", sheet = 1, range = "R89C1:R1769C17", col_names = FALSE)

ncol(dados)

names(dados) <- c("cidade", "total", "ag0to4", "ag5to9", "ag10to14",
                  "ag15to17", "ag18to19", "ag20to24", "cidade2",
                  "ag25to29", "ag30to34", "ag35to39", "ag40to49",
                  "ag50to59", "ag60to69", "ag70plus", "codibge")

dados <- dados %>% 
  select(-cidade2, -cidade) %>% 
  mutate(
    codibge = as.character(codibge)
  ) %>% 
  filter(str_length(codibge) == 7) %>% 
  left_join(cities, by = "codibge") %>% 
  select(-codibge, -cidade) %>% 
  rename(id_mun = idmun)

dados <- dados %>% 
  relocate(id_mun)



msg <- paste("CREATE TABLE ibge_idade (
  idmunidade SERIAL PRIMARY KEY,
  id_mun INTEGER,",
             paste(names(dados)[seq(2, 15)], " INTEGER", collapse = ","),
             ");")

odbc::dbSendQuery(db, msg)

read_any(db, "ibge_idade")

odbc::dbWriteTable(
  db, name = "ibge_idade", value = dados,
  row.names = FALSE, append = TRUE
)
