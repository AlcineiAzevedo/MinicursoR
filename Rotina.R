#Analise de experimentos no software R
#Nermy Ribeiro Valadares
#---------------------------------------------------------------------
remove(list=ls())#limpa a memoria 
#Ctrl+L limpa o console 
#Importar o conjunto de dados 
setwd("C:/Users/User/Desktop/Curso")
D=read.table("Dados.txt",h=T)
D1=read.csv("DadosCSV.csv",h=T, sep = ";") #CSV
#--------------------------------------------------------------------
#Analises descritivas 
#Metodos numericos 
#Media
DmedGeral=mean(D$resp) #obtendo a media da variavel resposta
DmedGeral
Dmedia=aggregate(D$resp,list(D$trat),mean)
Dmedia

#Mediana
median(D$resp) # obtendo a mediana 

#Variacia 
var(D$resp)#obtendo a variancia

#Desvio padrao
sd(D$resp) #obtendo Desvio padrao

#Erro padrao da media 
ep1=sd(D[1:3,3])/sqrt(3)#obtendo o erro padrao da media 
ep1
ep2=sd(D[4:6,3])/sqrt(3)#obtendo o erro padrao da media 
ep2
Dsd=aggregate(D$resp,list(D$trat),sd)
Dep=Dsd$x/sqrt(3)
Dep

Cop=cbind(Dmedia,Dep)
colnames(Cop)=c("Trat","Média","Ep")
Cop

#Metodos graficos 
hist(D$resp)
#-------------------------------------------------------------------
remove(list=ls())
#Analise de experimentos (tabela)
#Ex: Experimento em DIC
#Tratamentos qualitatovos e variáveis respostas quantitativas
dados=read.table("Dados.txt",h=T)
trat=as.factor(dados$trat)#Transforma os tratamentos em fatores 
Anova=aov(dados$resp~trat)
summary(Anova)

#Teste de normalidade 
Res=residuals(Anova)
shapiro.test(Res)
#h0=Os residuos seguem distribuicao normal 
#h1=Os residuos nao seguem distribuicao normal
#Se p<=0,05 rejeita h0

# Homogeneidade de variancias 
modelo=dados$resp~dados$trat
bartlett.test(modelo, dados)
#hipotese do teste bartlett
#h0= As variancias dos residuos sao homoganeas 
#h1= As variancias dos residuos nao sao homogeneas
#Se p<=0,05 rejeita-se h0
#-------------------------------------------------------------------
#Testes de comparacao de medias
#Tukey
install.packages("agricolae")
library(agricolae)
result=HSD.test(Anova,"trat", group=TRUE,console=TRUE)
bar.group(result$groups,ylim=c(0,(max(dados)*1.25)), density=4,border="blue")
text((nrow(result$groups[2]))/2,max(dados)*1.20,"Teste de Médias")
#-------------------------------------------------------------------
#Pacotes
#ExpDes.pt
install.packages("ExpDes.pt")
library(ExpDes.pt)
dic(
  trat=dados$trat,
  resp=dados$resp,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

#DBC
remove(list=ls())
dados=read.table("DadosDBC.txt",h=T)
trat=as.factor(dados$trat)
bloc=as.factor(dados$rep)
Anova=aov(dados$resp~trat+bloc)
summary(Anova)

#ExpDes.pt
dbc(
  trat=dados$trat,
  bloco=dados$rep,
  resp=dados$resp,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "oneillmathews",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)


#DQL
remove(list=ls())
dados=read.table("dadosDQL.txt",h=T)
col=as.factor(dados$colunas)
lin=as.factor(dados$linhas)
Anova=aov(dados$var~dados$trat+col+lin)
summary(Anova)

#ExpDes.pt
dql(
  trat=dados$trat,
  linha=dados$linhas,
  coluna=dados$colunas,
  resp=dados$var,
  quali = TRUE,
  mcomp = "tukey",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
#-------------------------------------------------------------------
#Fatorial 
#DIC
dados=read.table("dadosFAT.txt",h=T)
FA=as.factor(dados$FA)
FB=as.factor(dados$FB)
Anova=aov(dados$var~FA+FB+FA:FB)
summary(Anova)

#ExpDes.pt
fat2.dic(
  fator1=dados$FA,
  fator2=dados$FB,
  resp=dados$var,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("variedade", "Irrigação"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)


#######DBC
dados=read.table("dadosFAT.txt",h=T)
FA=as.factor(dados$FA)
FB=as.factor(dados$FB)
Bloco=as.factor(dados$rep)
Anova=aov(dados$var~Bloco+FA+FB+FA:FB)
summary(Anova)

#ExpDes.pt
fat2.dbc(
  fator1=dados$FA,
  fator2=dados$FB,
  bloco=dados$rep,
  resp=dados$var,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("F1", "F2"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
#-------------------------------------------------------------------
#Parcela subdividida (DIC)
dados=read.table("DadosPSUB1.txt",h=T)
psub2.dic(
  fator1=dados$Irrigacao,
  fator2=dados$Variedade,
  repet=dados$rep,
  resp=dados$resp,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("F1", "F2"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
#Interação siginificativa
dados=read.table("DadosPSUBI.txt",h=T)
psub2.dic(
  fator1=dados$FA,
  fator2=dados$FB,
  repet=dados$bloco,
  resp=dados$resp,
  quali = c(TRUE, TRUE),
  mcomp = "tukey",
  fac.names = c("F1", "F2"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
#-------------------------------------------------------------------
#Analisar varias variaveis ao mesmo tempo e salvar o resultado
D=read.table("DadosVariasVariaveis.txt",h=T)
sink("Resultado.txt")#Salva o resultado no diretorio
for(i in 3:13){
  print(colnames(D)[i])
  dbc(D$trat, D$rep,D[,i],mcomp = "sk")
}
sink()

#-------------------------------------------------------------------
#Tratamentos quantitativos e variáveis respostas quantitativas
#Regressao
remove(list=ls())
temp=c(18,16,25,22,20,21,23,19,17)
dilat=c(5,3,10,8,6,7,9,6,5)
#Regressao linear de primeiro grau
reglin=lm(dilat~temp) # ~utilizado para criar modelos 
anova(reglin)
summary(reglin)

#y=0,73**x-8,17**   R2=0,96

D=cbind(temp,dilat)
D

#Regressao quadratica 
reglin2=lm(dilat~temp+I(temp^2))
anova(reglin2)
summary(reglin2)

#Regressao cubica 
reglin3=lm(dilat~temp+I(temp^2)+ I(temp^3))
anova(reglin3)
summary(reglin3)

#ExpDes.pt
dados=read.table("REG.txt", h=T)
dic(
  trat=dados$Trat,
  resp=dados$Resp,
  quali = FALSE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)
#-------------------------------------------------------------------
#Testemunha adicional 
install.packages("Tratamentos.ad")
library(Tratamentos.ad)
D=read.table("DadosTest.txt",h=T)
fatorial2.ad.dbc(D)
