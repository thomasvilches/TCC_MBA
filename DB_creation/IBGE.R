

source("functions.R", encoding = "UTF-8")
source("packages.R", encoding = "UTF-8")


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


# Raça


dados <- readxl::read_xls("../Dados/sao_paulo_20190207/Tabela 4.20.2.1.xls", sheet = 1, range = "R89C1:R1769C9", col_names = FALSE)

ncol(dados)

names(dados) <- c("cidade", "total", "branca", "preta", "amarela",
                  "parda", "indigena", "sem_declaracao", "codibge")

dados <- dados %>% 
  select(-cidade) %>% 
  mutate(
    codibge = as.character(codibge)
  ) %>% 
  filter(str_length(codibge) == 7) %>% 
  left_join(cities, by = "codibge") %>% 
  select(-codibge, -cidade) %>% 
  rename(id_mun = idmun)


dados <- dados %>% 
  relocate(id_mun)

dados <- dados %>% 
  mutate_at(
    vars(seq(2, 8)),
    as.integer
  )



msg <- paste("CREATE TABLE ibge_raca (
  idmunraca SERIAL PRIMARY KEY,
  id_mun INTEGER,",
             paste(names(dados)[seq(2, 8)], " INTEGER", collapse = ","),
             ");")

odbc::dbSendQuery(db, msg)

read_any(db, "ibge_raca")

odbc::dbWriteTable(
  db, name = "ibge_raca", value = dados,
  row.names = FALSE, append = TRUE
)



# alfabetização


dados <- readxl::read_xls("../Dados/sao_paulo_20190207/Tabela 4.20.4.1.xls", sheet = 1, range = "R89C1:R1769C11", col_names = FALSE)

ncol(dados)

names(dados) <- c("cidade", "total", "total_homens", "total_mulheres",
                  "alf_total", "alf_homens", "alf_mulheres",
                  "taxa_total", "taxa_homens", "taxa_mulheres", "codibge")

dados <- dados %>% 
  select(-cidade) %>% 
  mutate(
    codibge = as.character(codibge)
  ) %>% 
  filter(str_length(codibge) == 7) %>% 
  left_join(cities, by = "codibge") %>% 
  select(-codibge, -cidade) %>% 
  rename(id_mun = idmun)


dados <- dados %>% 
  relocate(id_mun)

ncol(dados)
glimpse(dados)

dados <- dados %>% 
  mutate_at(
    vars(seq(2, 7)),
    as.integer
  )



msg <- paste("CREATE TABLE ibge_alfabetizacao (
  idmunalf SERIAL PRIMARY KEY,
  id_mun INTEGER,",
             paste(names(dados)[seq(2, 7)], " INTEGER", collapse = ","),
             ",",
             paste(names(dados)[seq(8, 10)], " NUMERIC", collapse = ","),
             ");")

odbc::dbSendQuery(db, msg)

read_any(db, "ibge_alfabetizacao")

odbc::dbWriteTable(
  db, name = "ibge_alfabetizacao", value = dados,
  row.names = FALSE, append = TRUE
)

# drop_table(db, "ibge_alfabetizacao")

# renda


dados <- readxl::read_xls("../Dados/sao_paulo_20190207/Tabela 4.20.7.1.xls", sheet = 1, range = "R89C1:R1769C11", col_names = FALSE)

ncol(dados)
glimpse(dados)

names(dados) <- c("cidade", "total", "ate_meio", "de_meioa1", "de_1a2",
                  "de_2a5", "de_5a10", "de_10a20", "s_20plus", "sem_rendimento", "codibge")


dados <- dados %>% 
  select(-cidade) %>% 
  mutate(
    codibge = as.character(codibge)
  ) %>% 
  filter(str_length(codibge) == 7) %>% 
  left_join(cities, by = "codibge") %>% 
  select(-codibge, -cidade) %>% 
  rename(id_mun = idmun)

glimpse(dados)

dados <- dados %>% 
  relocate(id_mun)

dados <- dados %>% 
  mutate_at(
    vars(seq(2, 10)),
    as.integer
  )



msg <- paste("CREATE TABLE ibge_renda (
  idmunrenda SERIAL PRIMARY KEY,
  id_mun INTEGER,",
             paste(names(dados)[seq(2, 10)], " INTEGER", collapse = ","),
             ");")

odbc::dbSendQuery(db, msg)

read_any(db, "ibge_renda")

odbc::dbWriteTable(
  db, name = "ibge_renda", value = dados,
  row.names = FALSE, append = TRUE
)


# Arrumando ref idade ---------------------------------------------------------


db <- conecta_base()


idade <- read_any(db, "ibge_idade") %>% head


nomes <- idade %>% select(starts_with("ag")) %>% names

idades <- nomes %>% gsub("ag", "", .) %>%
  gsub("to", " a ", .) %>% gsub("(\\d+)$", "\\1 anos", .) %>% gsub("plus", " anos ou mais", .)
ages <- nomes %>% gsub("ag", "", .) %>%
  gsub("to", " to ", .) %>% gsub("(\\d+)$", "\\1 years", .) %>% gsub("plus", "+ years", .)

df <- data.frame(groups = nomes, idade_pt = idades, idade_en = idades)


msg <- paste("CREATE TABLE ref_idades (
  ididades SERIAL PRIMARY KEY,",
             paste(names(df), " VARCHAR", collapse = ","),
             ");")


odbc::dbSendQuery(db, msg)

odbc::dbWriteTable(
  db, name = "ref_idades", value = df,
  row.names = FALSE, append = TRUE
)

# Arrumando ref raca ---------------------------------------------------------


db <- conecta_base()


raca <- read_any(db, "ibge_raca") %>% head


nomes <- raca %>% select(!starts_with("id")) %>% names

raca <- c("Total", "Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem declaração")
race <- c("Total", "White", "Black", "East Asian", "Brown", "Indigenous", "No declaration")
  
df <- data.frame(groups = nomes, raca_pt = raca, raca_en = race)


msg <- paste("CREATE TABLE ref_raca (
  idraca SERIAL PRIMARY KEY,",
             paste(names(df), " VARCHAR", collapse = ","),
             ");")


odbc::dbSendQuery(db, msg)

odbc::dbWriteTable(
  db, name = "ref_raca", value = df,
  row.names = FALSE, append = TRUE
)




# Arrumando ref renda -----------------------------------------------------



renda <- read_any(db, "ibge_renda") %>% head



nomes <- renda %>% select(!starts_with("id")) %>% names

df <- data.frame(groups = nomes) %>% 
  mutate(
    renda_pt = case_when(
      groups == "total" ~ "Total",
      groups == "ate_meio" ~ "Até meio salário mínimo",
      groups == "de_meioa1" ~ "De meio a um salário mínimo",
      groups == "s_20plus" ~ "Mais de 20 salários mínimos",
      groups == "sem_rendimento" ~ "Sem rendimento",
      TRUE ~ gsub("de_(\\d+)a(\\d+)", "De \\1 a \\2 salários mínimos", groups)
      
    ),
    renda_en = case_when(
      groups == "total" ~ "Total",
      groups == "ate_meio" ~ "Less than half minimum wage",
      groups == "de_meioa1" ~ "Half to one minimum wage",
      groups == "s_20plus" ~ "More than 20 minimum wages",
      groups == "sem_rendimento" ~ "No income",
      TRUE ~ gsub("de_(\\d+)a(\\d+)", "From \\1 to \\2 minimum wages", groups)
      
    )
  )


msg <- paste("CREATE TABLE ref_renda (
  idrendaref SERIAL PRIMARY KEY,",
             paste(names(df), " VARCHAR", collapse = ","),
             ");")


odbc::dbSendQuery(db, msg)

odbc::dbWriteTable(
  db, name = "ref_renda", value = df,
  row.names = FALSE, append = TRUE
)




