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
count(arvored_decisao_tmp$type)
##########################################

#Quantity of machine failure events: 339 failures out of 10000 events
table(arvored_decisao_tmp$machine_failure)

#criação de uma base de dados composta apenas das observações com falha
all_errors <- data.frame(filter(arvored_decisao_tmp, arvored_decisao_tmp$machine_failure_factor == 'Y'))
all_errors
filter(all_errors, all_errors$machine_failure == 1)
names(all_errors)
count(all_errors$TWF)
count(arvored_decisao_tmp$TWF)

count(all_errors$OSF)
count(arvored_decisao_tmp$OSF)

count(all_errors$HDF)
count(arvored_decisao_tmp$HDF)

count(all_errors$PWF)
count(arvored_decisao_tmp$PWF)

count(all_errors$RNF)
count(arvored_decisao$RNF)

filter(arvored_decisao_tmp, arvored_decisao_tmp$RNF == 'Y')
descritiva <- function(var,var1){
  # Sumarize the machine failure tax related to the variable in analysis
  tgc <- Rmisc::summarySE(arvored_decisao_tmp, measurevar= "machine_failure", groupvars=c(var))
  
  (ggplot(tgc) + 
    # Plots the bar graph with the frequencies
  
    #geom_bar(aes(x=tgc[,var], weight=N, fill=as.factor(tgc[,var]))) + 
    # Plots the error bars
    geom_errorbar(aes(x=tgc[,var], y=machine_failure, ymin=machine_failure-se, ymax=machine_failure+se), width=.1, color = "black") +
    # Plots the averages for each group
    geom_point(aes(x=tgc[,var], y=machine_failure, group = '1'), color = "black") +
    # Plots the lines connecting the averages
    geom_line( aes(x=tgc[,var], y=machine_failure, group = '1'), color = "black") +
    # Color Scale of the Averages Chart
    #scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Bar chart color scale
    #scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # 'Lighter' graphic aesthetics
    #theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
        #  panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    theme_classic(base_line_size = 1.5) +

    # Removes the subtitle
    theme(legend.position = "none") +
    theme(axis.text.x = element_text (size = 11, color = "black"),  
          axis.title.x = element_text(vjust = -1.5),
          plot.margin = margin(20, 20, 20, 20, "pt"),
    element_blank(), axis.line = element_line("black", size = 1.5)) +
    theme(axis.text.y = element_text (size = 11, color = "black"), 
          axis.title.y = element_text(vjust = +6),
          plot.margin = margin(20, 20, 20, 20, "pt"),
    element_blank(), axis.line = element_line("black", size = 1.5)) +
    # Axis label
    xlab(var1) + ylab("TAXA DE FALHA DE EQUIPAMENTO (%)")  +
    # Secondary axis marks
    scale_y_continuous(labels = scales::percent) )
  

}

#Initial analysis to check the general relationship between the maquine failure and the process variables
#Green: Frequency
#Blue: Average fail error

descritiva("type","TIPO")

max(table((arvored_decisao_tmp$type)))

#Categorizing continuous variables to further analysis
arvored_decisao_tmp$cat_air_temperature <- quantcut (arvored_decisao_tmp$air_temperature, 8, dig.lab=6)
descritiva("cat_air_temperature", "FAIXAS DE TEMPERATURA DO AR (K)")
#descritiva("air_temperature", "TEMPERATURA DO AR (K)")

arvored_decisao_tmp$cat_process_temperature <- quantcut (arvored_decisao_tmp$process_temperature, 8, dig.lab=6)
descritiva("cat_process_temperature", "FAIXAS DE TEMPERATURA DO PROCESSO (K)")
#descritiva("process_temperature", "FAIXAS DE TEMPERATURA DO PROCESSO (K)")

arvored_decisao_tmp$cat_rotational_speed <- quantcut (arvored_decisao_tmp$rotational_speed, 8, dig.lab=6)
descritiva("cat_rotational_speed","FAIXAS DE VELOCIDADE DE ROTAÇÃO (RPM)")
#descritiva("rotational_speed","VELOCIDADE DE ROTAÇÃO (RPM)")

arvored_decisao_tmp$cat_torque <- quantcut (arvored_decisao_tmp$torque, 8, dig.lab=6)
descritiva("cat_torque", "FAIXAS DE TORQUE (N)")
#descritiva("torque", "TORQUE (N)")

arvored_decisao_tmp$cat_tool_wear <- quantcut (arvored_decisao_tmp$tool_wear, 8, dig.lab=6)
descritiva("cat_tool_wear", "FAIXAS DE TEMPO DE USO DA FERRAMENTA (MINUTOS)")
#descritiva("tool_wear", "TEMPO DE USO DA FERRAMENTA (MINUTOS)")

     
arvored_decisao_tmp %>% str                         

