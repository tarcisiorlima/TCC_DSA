###############################################
# Separate the database between test and train #
set.seed(123)
bool_treino <- stats::runif(dim(arvored_decisao_tmp)[1])>.20 # 80% train - 20% test
table (bool_treino)
treino <- arvored_decisao_tmp[bool_treino,]
teste  <- arvored_decisao_tmp[!bool_treino,]

arvored_decisao_tmp %>% str
# Deixar a árvore ser feliz

############################################
# Vamos construir a árvore de classificação #
set.seed(123)
arvore_machine_failure <- rpart::rpart(machine_failure_factor ~  air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                       data=treino,
                                       method='class',
                                       xval=5,
                                       control = rpart.control(cp = 0, 
                                                               minsplit = 1, 
                                                               maxdepth = 30)
)


arvore_TWF <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=5,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)



arvore_HDF <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=5,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)


arvore_PWF <- rpart::rpart(PWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=5,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)



arvore_OSF <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=5,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)


arvore_RNF <- rpart::rpart(RNF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=5,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)

# Plotando a árvore
rpart.plot::rpart.plot(arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores
#Arvore falha TWF
rpart.plot::rpart.plot(arvore_TWF,
                       box.palette = paleta) # Paleta de cores
#Arvore falha HDF
rpart.plot::rpart.plot(arvore_HDF,
                       box.palette = paleta) # Paleta de cores
#Arvore falha PWF
rpart.plot::rpart.plot(arvore_PWF,
                       box.palette = paleta) # Paleta de cores
#Arvore falha OSF
rpart.plot::rpart.plot(arvore_OSF,
                       box.palette = paleta) # Paleta de cores
#Arvore falha RNF
rpart.plot::rpart.plot(arvore_RNF,
                       box.palette = paleta) # Paleta de cores


################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure
p_teste = stats::predict(arvore_machine_failure, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_teste, teste$machine_failure_factor)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de treino: %s ', percent(acc))


###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$machine_failure_factor, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

rpart.plot::rpart.plot(arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores

##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_machine_failure)
tab_cp

plotcp(arvore_machine_failure)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_machine_failure <- rpart::rpart(machine_failure_factor ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_machine_failure, teste)
c_teste = base::factor(ifelse(p_teste[,2]>0.03, "Y", "N"))
################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure

tab <- table(c_teste, teste$machine_failure_factor)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))
###############################

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$machine_failure_factor, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))


# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC


############################### PLOT
rpart.plot::rpart.plot(arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_machine_failure,
                       box.palette = paleta) # Paleta de cores

################################################################################################################
#filtrando a base de dados de teste e treino para apresentar apenas os casos de falha.
all_errors_teste <- filter(teste, machine_failure == 1)
summary(all_errors_teste)

all_errors_treino <- filter(treino, machine_failure == 1)
summary(all_errors_treino)
#############################################

##############################################################################################################  

#PODA HDF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_HDF)
tab_cp

plotcp(arvore_HDF)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_HDF <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                                 data=treino,
                                                 method='class',
                                                 xval=0,
                                                 control = rpart.control(cp = cp_min, 
                                                                         minsplit = 1, 
                                                                         maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_HDF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))
################################################################################
# Avaliar a árvore na base de teste: arvore_poda_HDF

tab <- table(c_teste, teste$HDF)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))
###############################

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$HDF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_HDF,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_HDF,
                       box.palette = paleta) # Paleta de cores

##############################################################################################################  

#PODA HDF Filtro por falha
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_HDF_error)
tab_cp

plotcp(arvore_HDF_error)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_HDF_error <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=all_errors_teste,
                                method='class',
                                xval=0,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_HDF_error, all_errors_teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))
################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure

tab <- table(c_teste, all_errors_teste$HDF)
acc <- (tab[1,1]+tab[2,2])/nrow(all_errors_teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))
###############################
############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=all_errors_teste$HDF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_poda_HDF_error,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_HDF,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_HDF,
                       box.palette = paleta) # Paleta de cores
##############################################################################################################  
#PODA PWF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_PWF)
tab_cp

plotcp(arvore_PWF)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_PWF <- rpart::rpart(PWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=0,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_PWF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure

tab <- table(c_teste, teste$PWF)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))
###############################
#####
############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$PWF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_PWF,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_PWF,
                       box.palette = paleta) # Paleta de cores

##############################################################################################################  

#PODA OSF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_OSF)
tab_cp

plotcp(arvore_OSF)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_OSF <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=0,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_OSF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))
################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure

tab <- table(c_teste, teste$OSF)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))
###############################
#####
############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$OSF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_OSF,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_OSF,
                       box.palette = paleta) # Paleta de cores
###########################################################################################
#PODA OSF ERROR
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_OSF_error)
tab_cp

plotcp(arvore_OSF_error)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_OSF_error <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=all_errors_treino,
                                method='class',
                                xval=0,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_OSF_error, all_errors_teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))
################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure

tab <- table(c_teste, teste$OSF)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))
###############################
#####
############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=all_errors_teste$OSF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_OSF_error,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_OSF_error,
                       box.palette = paleta) # Paleta de cores
###########################################################################################
#PODA TWF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_TWF)
tab_cp

plotcp(arvore_TWF)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_TWF <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=0,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_TWF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$TWF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_TWF,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_TWF,
                       box.palette = paleta) # Paleta de cores

####################################################################################
###########################################################################################
#PODA RNF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_RNF)
tab_cp

plotcp(arvore_RNF)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_RNF <- rpart::rpart(RNF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=0,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_RNF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$RNF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
rpart.plot::rpart.plot(arvore_RNF,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_RNF,
                       box.palette = paleta) # Paleta de cores

################################################################################################

