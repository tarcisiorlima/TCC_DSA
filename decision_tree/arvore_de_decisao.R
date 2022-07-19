########################
# Instalação de pacotes
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret'       # Funções úteis para machine learning
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Importing the csv dataset
library(readr)
library(tidyverse)
library(reshape)
ai4i2020_arvored_decisao <- read_csv("ai4i2020_arvored_decisao.csv")
View(ai4i2020_arvored_decisao)

# Creating temperorary data set to preserve original data
ai4i2020_arvored_decisao_tmp <- ai4i2020_arvored_decisao

#############################################
#changing the variables name for ease of use 

str(ai4i2020_arvored_decisao_tmp) # Mostra a estrutura da base de dados
names(ai4i2020_arvored_decisao_tmp) # Para ver os nomes das variáveis

# rename function : argument is - new name = older name

ai4i2020_arvored_decisao_tmp <- dplyr::rename(ai4i2020_arvored_decisao_tmp, 
                    process_temperature = "Process temperature [K]",
                    product_id  = "Product ID",
                    air_temperature = "Air temperature [K]",
                    rotational_speed = "Rotational speed [rpm]",
                    tool_wear = "Tool wear [min]",
                    type = "Type",
                    torque = "Torque [Nm]",
                    machine_failure = "Machine failure")

names(ai4i2020_arvored_decisao_tmp)
##########################################

descritiva <- function(var,var1){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(ai4i2020_arvored_decisao_tmp, measurevar= "machine_failure", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/(5*var1), fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=machine_failure, ymin=machine_failure-se, ymax=machine_failure+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=machine_failure, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=machine_failure, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de falha") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*(5*var1), name = "Frequencia"), labels = scales::percent)
}

#Initial analysis to check the general relationship between the maquine failure and the process variables
#Green: Frequency
#Blue: Average fail error

descritiva("type",max(table((ai4i2020_arvored_decisao_tmp$type))))

#Categorizing continuous variables to further analysis
ai4i2020_arvored_decisao_tmp$cat_air_temperature <- quantcut (ai4i2020_arvored_decisao_tmp$air_temperature, 20, dig.lab=6)
descritiva("cat_air_temperature", max(table(ai4i2020_arvored_decisao_tmp$cat_air_temperature)))
#descritiva("air_temperature", length((ai4i2020_arvored_decisao_tmp$air_temperature)))

ai4i2020_arvored_decisao_tmp$cat_process_temperature <- quantcut (ai4i2020_arvored_decisao_tmp$process_temperature, 20, dig.lab=6)
descritiva("cat_process_temperature", max(table(ai4i2020_arvored_decisao_tmp$cat_process_temperature)))
#descritiva("process_temperature", max(table(ai4i2020_arvored_decisao_tmp$process_temperature)))

ai4i2020_arvored_decisao_tmp$cat_rotational_speed <- quantcut (ai4i2020_arvored_decisao_tmp$rotational_speed, 20, dig.lab=6)
descritiva("cat_rotational_speed",max(table(ai4i2020_arvored_decisao_tmp$cat_rotational_speed)))
#descritiva("rotational_speed", max(table(ai4i2020_arvored_decisao_tmp$rotational_speed)))

ai4i2020_arvored_decisao_tmp$cat_torque <- quantcut (ai4i2020_arvored_decisao_tmp$torque, 20, dig.lab=6)
descritiva("cat_torque", max(table(ai4i2020_arvored_decisao_tmp$cat_torque)))
descritiva("torque",max(table(ai4i2020_arvored_decisao_tmp$torque)))

ai4i2020_arvored_decisao_tmp$cat_torque <- quantcut (ai4i2020_arvored_decisao_tmp$torque, 20, dig.lab=6)
descritiva("cat_torque", max(table(ai4i2020_arvored_decisao_tmp$cat_torque)))
descritiva("tool_wear", max(table(ai4i2020_arvored_decisao_tmp$tool_wear)))
     
ai4i2020_arvored_decisao_tmp %>% str                         

#as.factor(ai4i2020_arvored_decisao_tmp$product_id) + as.factor(ai4i2020_arvored_decisao_tmp$type) +

#############################################
# Vamos construir a árvore de classificação #
arvore_machine_failure <- rpart::rpart(machine_failure ~  air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                data=ai4i2020_arvored_decisao_tmp,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

arvore_TWF <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=ai4i2020_arvored_decisao_tmp,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

arvore_HDF <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                       data=ai4i2020_arvored_decisao_tmp,
                                       parms = list(split = 'gini'), # podemos trocar para  'information'
                                       method='class' # Essa opção indica que a resposta é qualitativa
)

arvore_PWF <- rpart::rpart(PWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=ai4i2020_arvored_decisao_tmp,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

arvore_OSF <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=ai4i2020_arvored_decisao_tmp,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

arvore_RNF <- rpart::rpart(RNF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=ai4i2020_arvored_decisao_tmp,
                           parms = list(split = 'gini'), # podemos trocar para  'information'
                           method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores

#FAIL
rpart.plot::rpart.plot(arvore_TWF,
                       box.palette = paleta) # Paleta de cores

rpart.plot::rpart.plot(arvore_HDF,
                      box.palette = paleta) # Paleta de cores

rpart.plot::rpart.plot(arvore_PWF,
                       box.palette = paleta) # Paleta de cores

rpart.plot::rpart.plot(arvore_OSF,
                       box.palette = paleta) # Paleta de cores

#FAIL
rpart.plot::rpart.plot(arvore_RNF,
                       box.palette = paleta) # Paleta de cores
##############################


# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de sobreviver
prob = predict(arvore, ai4i2020_arvored_decisao_tmp)

# Classificação dos sobreviventes
class = prob[,2]>.5

head (class)
# Matriz de confusão
tab <- table(class, ai4i2020_arvored_decisao_tmp$machine_failure)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc
