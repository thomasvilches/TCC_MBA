
source("DB_creation/packages.R", encoding = "UTF-8")

# ?dbWriteTable

senha <- read.table("senha.txt")

db <- DBI::dbConnect(odbc::odbc(),
                     Driver = "{PostgreSQL ODBC Driver(ANSI)}",
                     Database = "DATASUS_PA",
                     UserName = senha$V1[1],
                     Password = senha$V1[2],
                     Servername = "localhost",
                     Port = 5433)


# baixado de arquivos para tabulaçao
# https://datasus.saude.gov.br/transferencia-de-arquivos/#

# Cadastro geral de estabelecimentos
dados <- read.dbf("../Dados/SIA_AUX/DBF/CADGERSP.dbf")
head(dados)


# Tabela Procedimentos ----------------------------------------------------
# esse arquivo tem os códigos dos procedimentos
dados <- read.dbf("../Dados/SIA_AUX/DBF/TB_SIGTAW.dbf")
head(dados)
nrow(dados)

dados$IP_DSCR <- iconv(dados$IP_DSCR, from = 'UTF-8', to = 'ASCII//TRANSLIT')

names(dados) <- c("codproc", "descproc")

# create_tables.sql
odbc::dbWriteTable(
  db, name = "procedimentos", value = dados,
  row.names = FALSE, append = TRUE
)


# rodando tabela


# Tabela CID ----------------------------------------------------
# esse arquivo tem os códigos dos CIDs
# Necessario achar o dicionario desta tabela
dados <- read.dbf("../Dados/SIA_AUX/DBF/S_CID.DBF")
head(dados,20)
nrow(dados)

names(dados) <- c("codcid", "opc", "cat", "subcat", "desccid", "restrsexo", "camposrad", "estadio", "repeterad")
unique(dados$cat)
unique(dados$subcat)
unique(dados$restrsexo)
unique(dados$camposrad)
unique(dados$estadio)
unique(dados$repeterad)

dados$desccid <- iconv(dados$desccid, from = 'latin1', to = 'ASCII//TRANSLIT')

odbc::dbWriteTable(
  db, name = "cid", value = dados,
  row.names = FALSE, append = TRUE
)

# Tabela cidades ----------------------------------------------------------
# contem os codigos das cidades
dados <- read.table("../Dados/SIA_AUX/CNV/sp_municip.cnv", sep = ";")
names(dados)

nrow(dados)

df <- str_split(dados$V1, pattern = "\\n") %>% 
  Reduce(c, .)
df

cidades <- str_extract_all(df, "[A-Z\\s]+")
cidades <- lapply(cidades, trimws,  which="both")

cidades <-lapply(cidades, function(x) x[3])

cidades <- Reduce(c, cidades)


codigos <- str_extract_all(df, "[\\d]+$")
codigos <- Reduce(c, codigos)
tab_cidades <- data.frame(cidade = cidades, codigo = codigos)



# tipo estab --------------------------------------------------------------


dados <- read.csv("../Dados/SIA_AUX/CNV/TP_ESTAB.CNV", header = FALSE, skip = 1, sep = ";", encoding = "latin1")
names(dados)
dados

dados <- dados$V1
dados <- iconv(dados, from = 'latin1', to = 'ASCII//TRANSLIT')

Tipo <- str_extract_all(dados, "[A-Z\\s]+")
Tipo <- lapply(Tipo, trimws,  which="both")

Tipo <-lapply(Tipo, function(x) str_flatten(x[x != ""]))

Tipo <- Reduce(c, Tipo)

codigos <- str_extract_all(dados, "-?[\\d]+$")
codigos <- Reduce(c, codigos)
tab_tipos_est <- data.frame(desctpestab = Tipo, codtpestab = codigos)


odbc::dbWriteTable(
  db, name = "tpestab", value = tab_tipos_est,
  row.names = FALSE, append = TRUE
)

# tipo financiamento --------------------------------------------------------------


dados <- read.csv("../Dados/SIA_AUX/CNV/TP_FINAN.CNV", header = FALSE, skip = 1, sep = ";", encoding = "latin1")
names(dados)
dados

dados <- dados$V1
dados <- iconv(dados, from = 'latin1', to = 'ASCII//TRANSLIT')

Tipo <- str_extract_all(dados, "\\D+")
Tipo <- lapply(Tipo, trimws,  which="both")

Tipo <-lapply(Tipo, function(x) str_flatten(x[x != ""]))

Tipo <- Reduce(c, Tipo)
Tipo[1] <- "Nao se aplica/Nao discriminado"
Tipo[90] <- "Nao se aplica/Nao discriminado"

codigos <- str_extract_all(dados, "-?[\\d]+$")
codigos <- Reduce(c, codigos)
codigos <- c("000000", codigos)
codigos[90] <- "999999"
tab_tipos_fin <- data.frame(desctpfin = Tipo, codtpfin = codigos)

odbc::dbWriteTable(
  db, name = "tpfinan", value = tab_tipos_fin,
  row.names = FALSE, append = TRUE
)


# tab complex -------------------------------------------------------------

tab_comp <- data.frame(codcompl = c(0, 1, 2, 3), desccompl = c("Nao se aplica", "Atencao Basica", "Media", "Alta"))


odbc::dbSendQuery(db, "CREATE TABLE complex (
  idcompl SERIAL PRIMARY KEY,
  codcompl INT NOT NULL,
  desccompl  VARCHAR(30)
);")


odbc::dbWriteTable(
  db, name = "complex", value = tab_comp,
  row.names = FALSE, append = TRUE
)

# tab Carater -------------------------------------------------------------

tab_catend <- data.frame(
  codcatend = c("99", "00", "01", "02", "03", "04", "05", "06"),
  desccatend = c("CARATER DE ATENDIMENTO NÃO INFORMADO", "CARATER DE ATENDIMENTO NÃO INFORMADO",
                "ELETIVO", "URGENCIA",
                "ACIDENTE NO LOCAL DE TRABALHO OU A SERV. DA EMPRESA",
                "ACIDENTE NO TRAJETO PARA O TRABALHO",
                "OUTROS TIPOS DE ACIDENTE DE TRÂNSITO",
                "OUTROS TIPOS LESÕES/ENVENENAMENTOS(AGENT.FIS./QUIM.)"
              )
)

odbc::dbWriteTable(
  db, name = "carater", value = tab_catend,
  row.names = FALSE, append = TRUE
)

# tab Raça -------------------------------------------------------------

tab_raca <- data.frame(
  codraca = c("99", "00", "01", "02", "03", "04", "05", "06", "09", "1M","1G","1C","DE", "D","87"),
  descraca = c(
                 "SEM INFORMACAO",
    "RACA/COR NAO EXIGIDO",
                 "BRANCA", "PRETA",
                 "PARDA",
                 "AMARELA",
                 "INDIGENA",
                 "RACA/COR INDEVIDOS",
                 rep("RACA/COR INDEVIDOS", 7)
  )
)


odbc::dbWriteTable(
  db, name = "raca", value = tab_raca,
  row.names = FALSE, append = TRUE
)

# Motivo saida --------------------------------------------------------------


dados <- read.csv("../Dados/SIA_AUX/CNV/MOTSAIPE.CNV", header = FALSE, skip = 1, sep = ";", encoding = "latin1")
names(dados)
dados

dados <- dados$V1
dados <- iconv(dados, from = 'latin1', to = 'ASCII//TRANSLIT')

Tipo <- str_extract_all(dados, "\\D+")
Tipo <- lapply(Tipo, trimws,  which="both")

Tipo <-lapply(Tipo, function(x) str_flatten(x[x != ""]))

Tipo <- Reduce(c, Tipo)

codigos <- str_extract_all(dados, "-?[\\d]+$")
codigos <- Reduce(c, codigos)

motsai <- data.frame(descmotsai = Tipo, codmotsai = codigos)


odbc::dbWriteTable(
  db, name = "motsai", value = motsai,
  row.names = FALSE, append = TRUE
)




# Cheking tables and desconnecting ----------------------------------------

odbc::dbListTables(db)

DBI::dbDisconnect(db)



