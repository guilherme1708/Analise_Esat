# Lista 2 - Análise Estatística - MAE0314

library(readxl)

peru <- read_excel("/home/gui/peru.xlsx")
attach(peru)

Matriz <- matrix(c(Age,Weight,Height,Pulse),39,4)
colnames(Matriz) <- c("Age","Weight","Height","Pulse")
Medias <- apply(Matriz,2,mean)
Covariancia <- cov(Matriz)
rownames(Covariancia) <- c("Age","Weight","Height","Pulse")
colnames(Covariancia) <- c("Age","Weight","Height","Pulse")
Medias
Covariancia

Correlacao <- cor(Matriz)
rownames(Correlacao) <- c("Age","Weight","Height","Pulse")
colnames(Correlacao) <- c("Age","Weight","Height","Pulse")
Correlacao

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "blue", ...)
}

#função retirada do help(pairs)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * 1)
}
pairs(Matriz, diag.panel = panel.hist, upper.panel = panel.cor)


Mac <- read_excel("/home/gui/BigMac.xlsx")
attach(Mac)
Matriz <- matrix(c(BigMac,Bread,as.numeric(EngSal),as.numeric(TeachSal),Service),45,5)
colnames(Matriz) <- c("BigMac","Bread","EngSal","TeachSal","Service")
Media <- apply(Matriz,2,mean)
Covariancia <- cov(Matriz)
rownames(Covariancia) <- c("BigMac","Bread","EngSal","TeachSal","Service")
colnames(Covariancia) <- c("BigMac","Bread","EngSal","TeachSal","Service")
Media
Covariancia

Correlacao <- cor(Matriz)
rownames(Correlacao) <- c("BigMac","Bread","EngSal","TeachSal","Service")
colnames(Correlacao) <- c("BigMac","Bread","EngSal","TeachSal","Service")
Correlacao

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "blue", ...)
}

#função retirada do help(pairs)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * 1)
}
pairs(Matriz, diag.panel = panel.hist, upper.panel = panel.cor)
