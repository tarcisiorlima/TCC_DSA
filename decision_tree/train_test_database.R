###############################################
# Separate the database between test and train #
set.seed(123)
bool_treino <- stats::runif(dim(ai4i2020_arvored_decisao_tmp)[1])>.25 # 75% test - 25% train
table (bool_treino)
#7525 observations in train data base and 2475 observations in test database
treino <- ai4i2020_arvored_decisao_tmp[bool_treino,]
teste  <- ai4i2020_arvored_decisao_tmp[!bool_treino,]

ai4i2020_arvored_decisao_tmp %>% str
# Deixar a árvore ser feliz
# ATENÇÂO! NÃO PLOTAR ESTA ÁRVORE!

set.seed(123)
arvore <- rpart::rpart(machine_failure_fact ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                       data=treino,
                       method='class',
                       xval=5,
                       control = rpart.control(cp = 0, 
                                               minsplit = 1, 
                                               maxdepth = 30)
)

############################################
# Vamos construir a árvore de classificação #
treino_arvore_machine_failure <- rpart::rpart(as.factor(machine_failure_fact) ~  air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                       data=ai4i2020_arvored_decisao_tmp,
                                       parms = list(split = 'gini'), # podemos trocar para  'information'
                                      method='class' # Essa opção indica que a resposta é qualitativa
)


treino_arvore_TWF <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

treino_arvore_HDF <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

treino_arvore_PWF <- rpart::rpart(PWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

treino_arvore_OSF <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

treino_arvore_RNF <- rpart::rpart(RNF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(treino_arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores

#FAIL
rpart.plot::rpart.plot(treino_arvore_TWF,
                       box.palette = paleta) # Paleta de cores

rpart.plot::rpart.plot(treino_arvore_HDF,
                       box.palette = paleta) # Paleta de cores

rpart.plot::rpart.plot(treino_arvore_PWF,
                       box.palette = paleta) # Paleta de cores

rpart.plot::rpart.plot(treino_arvore_OSF,
                       box.palette = paleta) # Paleta de cores

#FAIL
rpart.plot::rpart.plot(treino_arvore_RNF,
                       box.palette = paleta) # Paleta de cores


# Verificando a complexidade da árvore
arvore$frame
############################################
# Avaliar a árvore na base de treino
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))

p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_treino, treino$machine_failure)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
acc
sprintf('Acurácia na base de treino: %s ', percent(acc))

tab <- table(c_teste, teste$machine_failure)
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
aval_treino <- data.frame(obs= treino$machine_failure_fact, #observed event
                          pred=c_treino,              #predicted event
                          Y = p_treino[,2],           #Y probability
                          N = 1-p_treino[,2]          #N probability
)

aval_treino %>% head
str (aval_treino)
caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs)) #calculo da curva rock

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$machine_failure_fact, 
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

rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore)
tab_cp

plotcp(arvore)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda <- rpart::rpart(machine_failure_fact ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

p_treino = stats::predict(arvore_poda, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore_poda, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
aval_treino <- data.frame(obs=treino$machine_failure_fact, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$machine_failure_fact, 
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
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda,
                       box.palette = paleta) # Paleta de cores
        