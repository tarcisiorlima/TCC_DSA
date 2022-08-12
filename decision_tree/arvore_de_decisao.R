########################
# Instalação de pacotes
pacotes <- c('tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',       # Funções úteis para machine learning
             'readr',
             'reshape',
             'plotROC',
             'readr',
             'tidyverse',
             'reshape'
             
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

#Importando o dataset csv 
arvored_decisao <- read_csv("ai4i2020_arvored_decisao.csv")
#View(ai4i2020_arvored_decisao)

# Creating temporary data set to preserve original data
arvored_decisao_tmp <- arvored_decisao

#############################################
#changing the variables name for ease of use 

str(arvored_decisao_tmp) # Displays the data base structure
names(arvored_decisao_tmp) # Displays the variables names

# rename function : argument is - new name = older name

arvored_decisao_tmp <- dplyr::rename(arvored_decisao_tmp, 
                    process_temperature = "Process temperature [K]",
                    product_id  = "Product ID",
                    air_temperature = "Air temperature [K]",
                    rotational_speed = "Rotational speed [rpm]",
                    tool_wear = "Tool wear [min]",
                    type = "Type",
                    torque = "Torque [Nm]",
                    machine_failure = "Machine failure")


arvored_decisao_tmp$machine_failure_factor <- as.factor(ifelse(arvored_decisao_tmp$machine_failure == 0, "N","Y"))
arvored_decisao_tmp$TWF <- as.factor(ifelse(arvored_decisao_tmp$TWF == 0, "N","Y"))
arvored_decisao_tmp$HDF <- as.factor(ifelse(arvored_decisao_tmp$HDF == 0, "N","Y"))
arvored_decisao_tmp$PWF <- as.factor(ifelse(arvored_decisao_tmp$PWF == 0, "N","Y"))
arvored_decisao_tmp$OSF <- as.factor(ifelse(arvored_decisao_tmp$OSF == 0, "N","Y"))
arvored_decisao_tmp$RNF <- as.factor(ifelse(arvored_decisao_tmp$RNF == 0, "N","Y"))

names(arvored_decisao_tmp)
str(arvored_decisao_tmp)
##########################################

#Quantity of machine failure events: 339 failures out of 10000 events
table(arvored_decisao_tmp$machine_failure)

descritiva <- function(var,var1){
  # Sumarize the machine failure tax related to the variable in analysis
  tgc <- Rmisc::summarySE(arvored_decisao_tmp, measurevar= "machine_failure", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plots the bar graph with the frequencies
    geom_bar(aes(x=tgc[,var], weight=N/(5*var1), fill=as.factor(tgc[,var]))) + 
    # Plots the error bars
    geom_errorbar(aes(x=tgc[,var], y=machine_failure, ymin=machine_failure-se, ymax=machine_failure+se, colour='1'), width=.1) +
    # Plots the averages for each group
    geom_point(aes(x=tgc[,var], y=machine_failure, colour='1', group='1')) +
    # Plots the lines connecting the averages
    geom_line(aes(x=tgc[,var], y=machine_failure, colour='1', group='1')) +
    # Color Scale of the Averages Chart
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Bar chart color scale
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # 'Lighter' graphic aesthetics
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Removes the subtitle
    theme(legend.position = "none") +
    # Axis label
    xlab(var) + ylab("MACHINE  FAILURE  RATE  (%)") + 
    # Secondary axis marks
    scale_y_continuous(sec.axis = sec_axis(~.*(5*var1), name = "FREQUENCY"), labels = scales::percent)
}

#Initial analysis to check the general relationship between the maquine failure and the process variables
#Green: Frequency
#Blue: Average fail error

descritiva("type",max(table((arvored_decisao_tmp$type))))

max(table((arvored_decisao_tmp$type)))

#Categorizing continuous variables to further analysis
arvored_decisao_tmp$cat_air_temperature <- quantcut (arvored_decisao_tmp$air_temperature, 20, dig.lab=6)
descritiva("cat_air_temperature", max(table(arvored_decisao_tmp$cat_air_temperature)))


arvored_decisao_tmp$cat_process_temperature <- quantcut (arvored_decisao_tmp$process_temperature, 8, dig.lab=6)
descritiva("cat_process_temperature", max(table(arvored_decisao_tmp$cat_process_temperature)))


arvored_decisao_tmp$cat_rotational_speed <- quantcut (arvored_decisao_tmp$rotational_speed, 9, dig.lab=6)
descritiva("cat_rotational_speed",max(table(arvored_decisao_tmp$cat_rotational_speed)))


arvored_decisao_tmp$cat_torque <- quantcut (arvored_decisao_tmp$torque, 10, dig.lab=6)
descritiva("cat_torque", max(table(arvored_decisao_tmp$cat_torque)))


arvored_decisao_tmp$cat_tool_wear <- quantcut (arvored_decisao_tmp$tool_wear, 10, dig.lab=6)
descritiva("cat_tool_wear", max(table(arvored_decisao_tmp$cat_tool_wear)))

     
arvored_decisao_tmp %>% str                         

