library(tidyverse)
library(lrgs)
library(fastDummies)
library(ggplot2)
library(GGally)
library(HDInterval)

#--- Lendo e preparando os dados ---#

db <- read_csv('dados.zip')

db <- db %>% mutate(Species = as.factor(Species))

db <- db %>% filter(Weight != 0) #retirando observacao 0



#--- Análise descritiva ---#

aux_db <- pivot_longer(db,3:7,names_to = "medida")

ggplot(aux_db,aes(y = value, color = medida))+ # Figura 1
  geom_boxplot()+
  theme_minimal()

ggplot(db, aes(y = Weight))+ # Figura 2
  geom_boxplot(color = 4)+
  coord_cartesian(xlim = c(-1.5,1.5))+
  theme_minimal()

ggpairs(db[,1:6]) #verificando correlação entre preditoras

# summary(db$Weight)


#--- Preparando os dados e aplicando Gibbs regression ---#
db <- db %>% select(-Length2,-Length3) #decidimos manter apenas um length

db <- dummy_cols(db, remove_first_dummy = TRUE,remove_selected_columns = TRUE) #Criando Dummies

db <- db %>% mutate("Length1:Height" = Length1*Height, "Length1:Width" = Length1*Width, "Height:Width" = Height*Width, "Length1:Height:Width" = Length1*Height*Width) # dada as claras correlações entre entre as preditoras, adicionamos interação para controlar.

db <- db %>% mutate(Weight = log(Weight)) #tranformando Y para ln(Y)


n <- 10100 # número de iterações das cadeias
p <- 13 # número de covariaveis

start_1 <- list(B = matrix(rep(0,p+1), nrow = p+1), Sigma = matrix(1)) #Amostrando dois modelos com valores iniciais diferentes
start_2 <- list(B = matrix(rep(100,p+1), nrow = p+1), Sigma = matrix(10))

bayesian_model_a <- Gibbs.regression(as.matrix(db[,2:(p+1)]), db$Weight, NULL, n, trace='bs', fix='xy', start = start_1) ##GIBBS
bayesian_model_b <- Gibbs.regression(as.matrix(db[,2:(p+1)]), db$Weight, NULL, n, trace='bs', fix='xy', start = start_2)

post_chains_1 <- Gibbs.post2dataframe(bayesian_model_a)
post_chains_2 <- Gibbs.post2dataframe(bayesian_model_b)

colnames(post_chains_1) <- c(paste("B",0:(p), sep = ""),"Sigma")


#--- Verificação do resultado da Gibbs regression ---#

# burn-in

post_chains_1 <- post_chains_1[101:10100,]
post_chains_2 <- post_chains_2[101:10100,]

n<- 10000

#Intervalos de credibilidade de 95%

cred_intervals <- data.frame(colnames(post_chains_1), rep(0,p+2), rep(0,p+2))
colnames(cred_intervals) <- c("Parâmetro","2.5%","97.5%")

for (i in 1:(p+2)){
  HPD_region <- HDInterval::hdi(post_chains_1[,i])
  cred_intervals[i,2] <- HPD_region[1]
  cred_intervals[i,3] <- HPD_region[2]
}

#plot dos intervalos

ggplot(cred_intervals, aes(x = "Parâmetro"))+
  geom_segment(aes(x=`Parâmetro`,y = `2.5%`, yend = `97.5%`), color = ifelse(cred_intervals$`2.5%`*cred_intervals$`97.5%`<0,2,4))+
  geom_hline(yintercept = 0, color = 2, linetype = "longdash")+
  coord_flip()

