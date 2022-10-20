###############################################
# Separate the database between test and train #
set.seed(123)
bool_treino <- stats::runif(dim(arvored_decisao_tmp)[1])>.20 # 80% train - 20% test
table (bool_treino)
treino <- arvored_decisao_tmp[bool_treino,]
teste  <- arvored_decisao_tmp[!bool_treino,]

#filtrando bases de teste e treino por machine failure = 1 "Y"

#all_errors_treino <- data.frame(filter(treino, treino$machine_failure_factor == 'Y'))
#all_errors_teste <-data.frame(filter(teste, teste$machine_failure_factor == 'Y'))

arvored_decisao_tmp %>% str
# Deixar a árvore ser feliz

############################################
# Vamos construir a árvore de classificação #
set.seed(123)
arvore_machine_failure <- rpart::rpart(machine_failure_factor ~  air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                       data=treino,
                                       method='class',
                                       xval=10,
                                       control = rpart.control(cp = 0, 
                                                               minsplit = 1, 
                                                               maxdepth = 30)
)


arvore_TWF <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)

#arvore_TWF_error <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
       #                    data=all_errors,
            #               method='class',
             #              xval=10,
                  #         control = rpart.control(cp = 0, 
            #                                       minsplit = 1, 
                    #                               maxdepth = 30)
#)



arvore_HDF <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)


arvore_PWF <- rpart::rpart(PWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)



arvore_OSF <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)


arvore_RNF <- rpart::rpart(RNF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                           data=treino,
                           method='class',
                           xval=10,
                           control = rpart.control(cp = 0, 
                                                   minsplit = 1, 
                                                   maxdepth = 30)
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)

# Plotando a árvore
#rpart.plot::rpart.plot(arvore_machine_failure,
                     #  box.palette = paleta) # Paleta de cores
#Arvore falha TWF
#rpart.plot::rpart.plot(arvore_TWF,
    #                   box.palette = paleta) # Paleta de cores
#Arvore falha HDF
#rpart.plot::rpart.plot(arvore_HDF,
   #                    box.palette = paleta) # Paleta de cores
#Arvore falha PWF
#rpart.plot::rpart.plot(arvore_PWF,
   #                    box.palette = paleta) # Paleta de cores
#Arvore falha OSF
#rpart.plot::rpart.plot(arvore_OSF,
                     #  box.palette = paleta) # Paleta de cores
#Arvore falha RNF
#rpart.plot::rpart.plot(arvore_RNF,
                    #   box.palette = paleta) # Paleta de cores


################################################################################
# Avaliar a árvore na base de teste: arvore_machine_failure
p_teste = stats::predict(arvore_machine_failure, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_teste, teste$machine_failure_factor)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
acc
sprintf('Acurácia na base de teste: %s ', percent(acc))


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

caret::twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

rpart.plot::rpart.plot(arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores


#my function 

myplot_cp <-function (x, minline = TRUE, lty = 3, col = 1, upper = c("size", 
                                                           "splits", "none"), ...) 
  {
    dots <- list(...)
    if (!inherits(x, "rpart")) 
      stop("Not a legitimate \"rpart\" object")
    upper <- match.arg(upper)
    p.rpart <- x$cptable
    if (ncol(p.rpart) < 5L) 
      stop("'cptable' does not contain cross-validation results")
    xstd <- p.rpart[, 5L]
    xerror <- p.rpart[, 4L]
    nsplit <- p.rpart[, 2L]
    ns <- seq_along(nsplit)
    cp0 <- p.rpart[, 1L]
    cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
    if (!"ylim" %in% names(dots)) 
      dots$ylim <- c(min(xerror - xstd) - 0.1, max(xerror + 
                                                     xstd) + 0.1)
    do.call(plot, c(list(ns, xerror, axes = FALSE, xlab = "cp", 
                         ylab = "X-val Relative Error", type = "o"), dots))
    box()
    axis(2, ...)
    segments(ns, xerror - xstd, ns, xerror + xstd)
    axis(1L, at = ns, labels = as.character(signif(cp, 2L)), tick = FALSE, 
         ...)
    switch(upper, size = {
      axis(3L, at = ns, labels = as.character(nsplit + 1), tick = FALSE, 
           ...)
      mtext("size of tree", side = 3, line = 3)
    }, splits = {
      axis(3L, at = ns, labels = as.character(nsplit), tick = FALSE, ...)
      mtext("number of splits", side = 3, line = 3)
    })
    minpos <- min(seq_along(xerror)[xerror == min(xerror)])
    if (minline) 
      abline(h = (xerror + xstd)[minpos], lty = lty, col = col)
    my_cpplot <- data.frame (var_xstd = xstd, var_xerror = xerror, var_nsplit = nsplit, var_ns = ns, var_cp = cp)
    

    #linhas adicionadas
    (ggplot(my_cpplot)+
        geom_errorbar(aes(x= var_cp, y=var_xerror, ymin=var_xerror - var_xstd, ymax=var_xerror + var_xstd), color = "black")+
    geom_point(aes(x=var_cp, y=var_xerror), color = "black") +

      # Plots the lines connecting the averages
      geom_line( aes(x=var_cp, y=var_xerror), color = "black") +
        #scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "Size of tree"))+

       # geom_point(aes(x=var_cp, y=var_ns/100), color = "red") +
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
           plot.margin = margin(25, 25, 25, 25, "pt"),
            element_blank(), axis.line = element_line("black", size = 1.5)) +
      theme(axis.text.y = element_text (size = 11, color = "black"), 
            axis.title.y = element_text(vjust = +6),
            plot.margin = margin(25, 25, 25, 25, "pt"),
            element_blank(), axis.line = element_line("black", size = 1.5)) +
      # Axis label
      xlab("CP") + ylab("ERRO RELATIVO (%)"))
     
  }
############################################### OUTRA FUNC
 
myplot_cp2<-function (x, minline = TRUE, lty = 3, col = 1, upper = c("size", 
                                                          "splits", "none"), ...) 
 {
   dots <- list(...)
   if (!inherits(x, "rpart")) 
     stop("Not a legitimate \"rpart\" object")
   upper <- match.arg(upper)
   p.rpart <- x$cptable
   if (ncol(p.rpart) < 5L) 
     stop("'cptable' does not contain cross-validation results")
   xstd <- p.rpart[, 5L]
   xerror <- p.rpart[, 4L]
   nsplit <- p.rpart[, 2L]
   ns <- seq_along(nsplit)
   cp0 <- p.rpart[, 1L]
   cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
   if (!"ylim" %in% names(dots)) 
     dots$ylim <- c(min(xerror - xstd) - 0.1, max(xerror + 
                                                    xstd) + 0.1)
   do.call(plot, c(list(ns, xerror, axes = FALSE, xlab = "cp", 
                        ylab = "ERRO RELATIVO", type = "o"), dots))
  box()
   axis(2, col.ticks = "white", tick = FALSE,  ...)
   segments(ns, xerror - xstd, ns, xerror + xstd)
   axis(1L, at = ns, labels = as.character(signif(cp, 2L)),tick = FALSE, 
        ...)
   switch(upper, size = {
     axis(3L, at = ns, labels = as.character(nsplit + 1), tick = FALSE,  
          ...)
     mtext("TAMANHO DA ÁRVORE", side = 3, line = 3)
   }, splits = {
     axis(3L, at = ns, labels = as.character(nsplit),tick = FALSE,  ...)
     mtext("number of splits", side = 3, line = 3)
   })
   minpos <- min(seq_along(xerror)[xerror == min(xerror)])
   if (minline) 
     abline(h = (xerror + xstd)[minpos], lty = lty, col = col)
   invisible()
 }                                                         


##########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_machine_failure)
tab_cp

plotcp(arvore_machine_failure)
myplot_cp(arvore_machine_failure)
myplot_cp2(arvore_machine_failure)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_machine_failure <- rpart::rpart(machine_failure_factor ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                            data=treino,
                            method='class',
                            xval=10,
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
aval_teste_machine_failure_factor <- aval_teste #adicionado
aval_teste_machine_failure_factor$VARIAVEL <- as.factor("FALHA") #adicionado

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

#alterando umas coisasdlasdsadsadsa
# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='black')) + 
  plotROC::geom_roc(n.cuts = 0) +
  #scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme_classic(base_line_size = 1.5) +
  theme(legend.position = "none") +
 # ggtitle("Curva ROC - base de teste")
  theme(axis.text.x = element_text (size = 11, color = "black"),  
        axis.title.x = element_text(vjust = -1.5),
        plot.margin = margin(20, 20, 20, 20, "pt"),
        element_blank(), axis.line = element_line("black", size = 1.5)) +
  theme(axis.text.y = element_text (size = 11, color = "black"), 
        axis.title.y = element_text(vjust = +6),
        plot.margin = margin(20, 20, 20, 20, "pt"),
        element_blank(), axis.line = element_line("black", size = 1.5)) 

CurvaROC


############################### PLOT

rpart.plot::rpart.plot(arvore_machine_failure,
                       box.palette = paleta) # Paleta de cores
rpart.plot::rpart.plot(arvore_poda_machine_failure,
                       box.palette = paleta) # Paleta de cores

##############################################################################################################  

#PODA HDF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_HDF)
tab_cp

plotcp(arvore_HDF)
myplot_cp2(arvore_HDF)
tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_HDF <- rpart::rpart(HDF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                                 data=treino,
                                                 method='class',
                                                 xval=10,
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
aval_teste_HDF <- aval_teste #adicionado
aval_teste_HDF$VARIAVEL <- as.factor("HDF") #adicionado

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

##############################################################################################################  
#PODA PWF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_PWF)
tab_cp

plotcp(arvore_PWF)
myplot_cp2(arvore_PWF)
tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_PWF <- rpart::rpart(PWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=10,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_PWF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.009, "Y", "N"))

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
aval_teste_PWF <- aval_teste #adicionado
aval_teste_PWF$VARIAVEL <- as.factor("PWF") #adicionado

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
myplot_cp2(arvore_OSF)
tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_OSF <- rpart::rpart(OSF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=10,
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
aval_teste_OSF <- aval_teste #adicionado
aval_teste_OSF$VARIAVEL <- as.factor("OSF") #adicionado

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

###########################################################################################
#PODA TWF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_TWF)
tab_cp

plotcp(arvore_TWF)
myplot_cp2(arvore_TWF)

tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_TWF <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=10,
                                control = rpart.control(cp = cp_min, 
                                                        minsplit = 1, 
                                                        maxdepth = 30)
)

p_teste = stats::predict(arvore_poda_TWF, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

################################################################################
# Avaliar a árvore na base de teste:

tab <- table(c_teste, teste$TWF)

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
aval_teste <- data.frame(obs=teste$TWF, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)
aval_teste_TWF <- aval_teste #adicionado
aval_teste_TWF$VARIAVEL <- as.factor ("TWF") #adicionado

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
#PODA TWF error filter
#########################
# pós-poda (Grid Search) #
##########################
#tab_cp <- rpart::printcp(arvore_TWF_error)
#tab_cp

#plotcp(arvore_TWF_error)

#tab_cp[which.min(tab_cp[,'xerror']),]
#cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

#set.seed(1)
#arvore_poda_TWF_error <- rpart::rpart(TWF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
#                                data=all_errors_treino,
#                                method='class',
#                                xval=10,
#                                control = rpart.control(cp = cp_min, 
#                                                        minsplit = 1, 
#                                                        maxdepth = 30)
#)

#p_teste = stats::predict(arvore_poda_TWF_error, all_errors_teste)
#c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))
################################################################################
# Avaliar a árvore na base de teste:

#tab <- table(c_teste, all_errors_teste$TWF)
#acc <- (tab[1,1]+tab[2,2])/nrow(all_errors_teste)
#acc
#sprintf('Acurácia na base de teste: %s ', percent(acc))
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
#aval_teste <- data.frame(obs=all_errors_teste$TWF, 
#                         pred=c_teste,
#                         Y = p_teste[,2],
#                         N = 1-p_teste[,2]
#)
#aval_teste_TWF_all_errors <- aval_teste #adicionado
#aval_teste_TWF_all_errors$VARIAVEL <- as.factor("TWF - APENAS ERROS") #adicionado

#twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
#CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
#  plotROC::geom_roc(n.cuts = 0) +
#  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
#  theme(legend.position = "none") +
#  ggtitle("Curva ROC - base de teste")

#CurvaROC
#rpart.plot::rpart.plot(arvore_TWF,
#                       box.palette = paleta) # Paleta de cores
#rpart.plot::rpart.plot(arvore_poda_TWF_error,
#                       box.palette = paleta) # Paleta de cores

####################################################################################
###########################################################################################
#PODA RNF
#########################
# pós-poda (Grid Search) #
##########################
tab_cp <- rpart::printcp(arvore_RNF)
tab_cp

plotcp(arvore_RNF)
myplot_cp2(arvore_RNF)
tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']

set.seed(1)
arvore_poda_RNF <- rpart::rpart(RNF ~ air_temperature + process_temperature + rotational_speed + torque + tool_wear,
                                data=treino,
                                method='class',
                                xval=10,
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
aval_teste_RNF <- aval_teste #adicionado
aval_teste_RNF$VARIAVEL <- as.factor("RNF")
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


#Grafico ROC completo
complete_roc <- rbind(aval_teste_HDF, aval_teste_machine_failure_factor, aval_teste_OSF, aval_teste_PWF, aval_teste_RNF, aval_teste_TWF)
aval_teste_RNF$VARIAVEL <- as.factor("RNF/TWF*")
complete_roc <- rbind(aval_teste_HDF, aval_teste_machine_failure_factor, aval_teste_OSF, aval_teste_PWF, aval_teste_RNF)

#complete_roc_graph <- melt_roc(complete_roc,"obs", Y )
CurvaROC <- ggplot(complete_roc, aes(d = obs, m = Y, colour=VARIAVEL)) +

  plotROC::geom_roc(n.cuts = 0) +
 #style_roc()+
  scale_color_viridis_d() +
  labs(color = "VARIÁVEIS") +
  theme_classic(base_line_size = 1.5) +
 # theme(legend.position = "none") +
  # ggtitle("Curva ROC - base de teste")
  theme(axis.text.x = element_text (size = 11, color = "black"),  
        axis.title.x = element_text(size = 11, vjust = -1.5),
        plot.margin = margin(20, 20, 20, 20, "pt"),
        legend.text = element_text (size = 11, color = "black"),
        element_blank(), axis.line = element_line("black", size = 1.5)) +
  theme(axis.text.y = element_text (size = 11, color = "black"), 
        axis.title.y = element_text(size = 11, vjust = +6),
        plot.margin = margin(20, 20, 20, 20, "pt"),
        legend.text = element_text (size = 11, color = "black"),
        element_blank(), axis.line = element_line("black", size = 1.5))+
 
  xlab("FRAÇÃO VERDADEIRO POSITIVO") + ylab("FRAÇÃO FALSO POSITIVO")  

CurvaROC

