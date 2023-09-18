CREATE TABLE procedimentos (
  idproc SERIAL PRIMARY KEY,
  CODPROC VARCHAR(30) NOT NULL,
  DESCPROC  VARCHAR(100)
);

-- c("codcid", "opc", "cat", "subcat", "desccid",
--"restrsexo", "camposrad", "estadio", "repeterad")
CREATE TABLE cid (
  idcid SERIAL PRIMARY KEY,
  codcid VARCHAR(10) NOT NULL,
  opc  CHAR(1),
  cat CHAR(1),
  subcat CHAR(1),
  desccid VARCHAR(100) NOT NULL,
  restrsexo INT,
  camposrad INT,
  estadio CHAR(1),
  repeterad CHAR(1)
);


CREATE TABLE tpestab (
  idtpest SERIAL PRIMARY KEY,
  codtpestab VARCHAR(10) NOT NULL,
  desctpestab VARCHAR(100) NOT NULL
);


CREATE TABLE tpfinan (
  idtpfin SERIAL PRIMARY KEY,
  codtpfin VARCHAR(10) NOT NULL,
  desctpfin VARCHAR(100) NOT NULL
);

CREATE TABLE carater (
  idcarat SERIAL PRIMARY KEY,
  codcatend CHAR(2) NOT NULL,
  desccatend  VARCHAR(100)
);

CREATE TABLE raca (
  idraca SERIAL PRIMARY KEY,
  codraca VARCHAR(2) NOT NULL,
  descraca  VARCHAR(30)
);

-- motivo saida
CREATE TABLE motsai (
  idmotsai SERIAL PRIMARY KEY,
  codmotsai CHAR(2) NOT NULL,
  descmotsai  VARCHAR(100)
);


-- complex
CREATE TABLE complex (
  idcompl SERIAL PRIMARY KEY,
  codcompl VARCHAR(2) NOT NULL,
  desccompl  VARCHAR(30)
);