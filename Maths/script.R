library(ggplot2)
library(DataExplorer)
library(dplyr)
library(ggthemes)
library(psych)
library(funModeling)
library(nortest)
library(knitr)

library(car)
library(nortest)
library(corrplot)
library(fitur)
library(ggplot2)
library(directlabels)
library(ggthemes)
library(scales)
library(gridExtra)
library(stats)
library(readxl)
library(reshape2)
library(dplyr)
library(BSDA)
library(e1071)

load("performance.RData")

data <- base_f
head(data)

str(data)
summary(data)

# Casting to factor
data <- data %>% mutate(
  Medu = factor(ifelse(
    Medu == 1, "None or primary education (4th grade)",
    ifelse(
      Medu == 2, "5th to 9th grade",
      ifelse(
        Medu == 3, "secondary education",
        ifelse(
          Medu == 4, "higher education",
          NA
        )
      )
    )
  )),
  Fedu = factor(ifelse(
    Fedu == 1, "None or primary education (4th grade)",
    ifelse(
      Fedu == 2, "5th to 9th grade",
      ifelse(
        Fedu == 3, "secondary education",
        ifelse(
          Fedu == 4, "higher education",
          NA
        )
      )
    )
  )),
  traveltime = factor(ifelse(
    traveltime == 1, "<15 min.",
    ifelse(
      traveltime == 2, "15 to 30 min.",
      ifelse(
        traveltime == 3, ">30 min.",
        NA
      )
    )
  )),
  absences = factor(ifelse(
    absences == 1, "0 to 5 absences",
    ifelse(
      absences == 2, "6 to 10 absences",
      ifelse(
        absences == 3, "11 to 20 absences",
        ifelse(
          absences == 4, ">20 absences",
          NA
        )
      )
    )
  ))
)
str(data)
summary(data)


# AN√ÅLISIS UNIVARIADO


# G3

## Tabla de informaci√≥n G3
data %>% select(age, G3) %>% psych::describe(IQR = TRUE, quant = c(.25,.75))

## Diagrama de caja
data %>% ggplot(aes(y=G3)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
  theme_base() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 

## Histograma
ggplot(data, aes(x=G3)) + 
  geom_histogram(bins=1+3.322*log(nrow(data)), aes(y=..density..), colour="black", fill="white")+
  geom_vline(aes(xintercept=mean(G3)), color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.2, fill="#FF6666")

## Prueba de normalidad:
qqPlot(data$G3)
shapiro.test(data$G3)

# Internet

## Tabla de frecuencia
internet_table <- freq(data$internet, plot = FALSE)
internet_table 

## Diagrama de barras de frecuencia
internet_table %>% ggplot(x=2, aes(x=var, y= frequency)) +
  geom_bar(stat = "identity", fill="steelblue", color="black") +
  labs(x="Internet", y="Frecuencia") 
  

## Diagrama de torta
internet_table %>% ggplot(aes(x=2, y=percentage, fill=var)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  labs(fill="Acceso a Internet") + 
  theme_void() +
  geom_text(aes(label = percent(percentage/100)),position = position_stack(vjust = 0.5),color = "white", size=5) +
  xlim(0.5, 2.5) 

# Reason

## Tabla de frecuencia
internet_table <- freq(data$reason, plot = FALSE)
internet_table 

## Diagrama de barras de frecuencia
internet_table %>% ggplot(x=2, aes(x=var, y= frequency)) +
  geom_bar(stat = "identity", fill="steelblue", color="black") +
  labs(x="Reason", y="Frecuencia") 

## Diagrama de torta
internet_table %>% ggplot(aes(x=2, y=percentage, fill=var)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  labs(fill="Reason") + 
  theme_void() +
  geom_text(aes(label = percent(percentage/100)),position = position_stack(vjust = 0.5),color = "white", size=5) +
  xlim(0.5, 2.5) 


# Inasistencias

## Tabla de frecuencia
internet_table <- freq(data$absences, plot = FALSE)
internet_table 

## Diagrama de barras de frecuencia
internet_table %>% ggplot(x=2, aes(x=var, y= frequency)) +
  geom_bar(stat = "identity", fill="steelblue", color="black") +
  labs(x="Internet", y="Inasistencias") 


## Diagrama de torta
internet_table %>% ggplot(aes(x=2, y=percentage, fill=var)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  labs(fill="Inasistencias") + 
  theme_void() +
  geom_text(aes(label = percent(percentage/100)),position = position_stack(vjust = 0.5),color = "white", size=5) +
  xlim(0.5, 2.5) 

# AN√ÅLISIS BIVARIADO

# Internet vs G3
internet_G3 <- data %>% select(G3, internet)

## Tabla descriptiva
psych::describeBy(internet_G3, internet_G3$internet, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Prueba Kruskal-Wallis
kruskal.test(G3~internet,data=internet_G3)  # Medias iguales

## Diagrama de barras
internet_G3 %>% ggplot(aes(x=internet, y=G3))+
  geom_boxplot(fill="orange")+
  labs(y="Nota final matem√°ticas")+
  theme_base()

# Reason vs G3
reason_G3 <- data %>% select(G3, reason)

## Tabla descriptiva
psych::describeBy(reason_G3, reason_G3$reason, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Prueba Kruskal-Wallis
kruskal.test(G3~reason, data=reason_G3)  # Medias iguales

## Diagrama de barras
reason_G3 %>% ggplot(aes(x=reason, y=G3))+
  geom_boxplot(fill="orange")+
  labs(y="Nota final matem√°ticas")+
  theme_base()

# Abscences vs G3
absences_G3 <- data %>% select(G3, absences)

## Tabla descriptiva
psych::describeBy(absences_G3, absences_G3$absences, mat=TRUE, IQR = TRUE, quant = c(.25,.75), digits=4 ) %>% 
  na.omit() %>%
  select(-item, -vars) %>%
  rename(category = group1)

## Prueba Kruskal-Wallis
kruskal.test(G3~absences, data=absences_G3)  # Medias diferentes

## Prueba Wilcoxon
pairwise.wilcox.test(x=absences_G3$G3,g=absences_G3$absences) # INTERPRETAR RESULTADOS!

## Diagrama de barras
absences_G3 %>% ggplot(aes(x=absences, y=G3))+
  geom_boxplot(fill="orange")+
  labs(y="Nota final matem√°ticas")+
  theme_base()








# MARIO
library(hrbrthemes)
library(viridis)

#funciones

descriptivas <- function(data){
  #C·lculo simple de estadÌticas descriptivos
  min <- min(data, na.rm = TRUE)
  q1 <- quantile(data, probs = 0.25, na.rm = TRUE)
  media <- mean.default(data, na.rm = TRUE)
  media_rec <- mean.default(data, trim = 0.025, na.rm = TRUE)
  mediana <- median.default(data, na.rm = TRUE)
  #moda <- mfv(data)
  var <- var(data, na.rm = TRUE)
  desvest <- sd(data, na.rm = TRUE)
  q3 <- quantile(data, probs = 0.75, na.rm = TRUE)
  max <- max(data, na.rm = TRUE)
  s <- skew(data)
  #skewness(data)
  c <- kurtosi(data)
  coefVariacion <- sd(data)/mean(data)
  
  #Valores de estadÌsticos como vector
  values <- as.numeric(c(min, q1, media, media_rec, mediana, var, desvest, q3, max, s, c, coefVariacion))
  #Encabezados de cada estadÌstico como un vector
  labels <- c("MÌnimo", "Q1", "Media", "Media recortada", "Mediana",
              "Varianza", "DesviaciÛn Est·ndar", "Q3", "M·ximo", "SimetrÌa", "Curtosis", "Coeficiente de variaciÛn")
  tbl <- as_tibble(rbind(values))
  names(tbl) <- labels
  tbl
}


plotBar <- function(data){
  #se requiere funModeling and tidyverse
  plt <- funModeling::freq(data, plot = FALSE) %>%
    ggplot(aes(x = var, y = percentage, fill = var)) + 
    geom_bar(stat='identity', color = 'black') +
    labs(fill = 'Tipos', y = 'Porcentaje', title = 'Posibles valores de la variable') +
    scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0,100)) +
    geom_text(aes(label = paste(percentage,'%')), nudge_y = 10, color = 'black', fontface = "bold") +
    scale_fill_manual(values = c("#0D1F2D", "#546A7B", "#9EA3B0", "#91999F", "#33658A", "#2F4858", "#4F5D75", "#BFC0C0", "#212D40", "#364156")) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  plt
}


plotBoxplot <- function(data, x, y, title = '', subtitle = '', ylabel = '', caption = ''){
  #se requiere funModeling and tidyverse
  plt <- ggplot(data, aes(x = x, y = y, fill = x)) +
    geom_boxplot(color = 'gray') +
    scale_fill_manual(values = c("#0D1F2D", "#546A7B", "#9EA3B0", "#91999F", "#33658A", "#2F4858", "#4F5D75", "#BFC0C0", "#212D40", "#364156")) +
    labs(fill = 'Tipos', y = ylabel, title = title, subtitle = subtitle, caption = caption) +
    #scale_fill_brewer(palette = 'Blues') +
    #geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_minimal() +
    theme(
      plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0.5)
      
    ) +
    xlab("")
  plt
}


tablaFrecuencias <- function(data){
  tbl <- funModeling::freq(data, plot = FALSE);
  names(tbl) <- c('CategorÌa/Indicador', 'Frecuencia absoluta', 'Porcentaje', 'Porcentaje acumulado')
  tbl
}

descriptivasNivel <- function(data, groupVar, y){
  #Se puede adaptar para que muestre las estadisticas m·s comunes
  y <- enquo(y)
  groupVar <- enquo(groupVar)
  tbl <- data %>%
    group_by(!!groupVar) %>%
    summarise(
      `MÌnimo` = min(!!y, na.rm = TRUE),
      Q1 = quantile(!!y, probs = 0.25, na.rm = TRUE),
      Q3 = quantile(!!y, probs = 0.75, na.rm = TRUE),
      Media = mean.default(!!y, na.rm = TRUE),
      `Media recortada` = mean.default(!!y, trim = 0.025, na.rm = TRUE),
      Mediana = median.default(!!y, na.rm = TRUE),
      #moda <- mfv(data)
      Varianza = var(!!y, na.rm = TRUE),
      `DesviaciÛn Est·ndar` = sd(!!y, na.rm = TRUE),
      M·ximo = max(!!y, na.rm = TRUE),
      `SimetrÌa` = skew(!!y),
      #skewness(data)
      Curtosis = kurtosi(!!y),
      `Coeficiente de variaciÛn` = sd(!!y)/mean(!!y)
    )
  tbl
}


# VARIABLES 
#sex
#age
#address
#Pstatus


data$sex <- factor(data$sex, levels = c('F', 'M'), labels = c('Femenino', 'Masculino'))
data$address <- factor(data$address, levels = c('R', 'U'), labels = c('Rural', 'Urbano'))
data$Pstatus <- factor(data$Pstatus, levels = c('A', 'T'), labels = c('Separados', 'Viviendo juntos'))


###################################################### AN¡LISIS UNIVARIADO


#SEX - CUALITATIVA #########################
#Tabla frecuencia
#No se observa nada extraÒo ni curioso
tablaFrecuencias(data$sex)
plotBar(data$sex)


#AGE - CUANTITATIVA DISCRETA ######################

#Resumen descriptivas
summary(data$age)

#QuÈ tan cerca est· la media y la mediana?
descriptivas(data$age)$Media
descriptivas(data$age)$Mediana

#Coeficiente de variaciÛn
descriptivas(data$age)$`Coeficiente de variaciÛn`

#Datos extremos???


#Se procede a realizar la prueba de normalidad de Shapiro-Wilk.

#prueba normalidad
shapiro.test(data$age)
ad.test(data$age)
lillie.test(data$age)


#Histogram
data %>% select(age) %>% arrange(age) %>%
  ggplot(aes(x = age)) +
  geom_histogram(aes(y=..density..), breaks = seq(14, 25, 1), bins = 1+3.322*log(nrow(data)), color = 'black', fill = '#546A7B') +
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed", size=1) + 
  geom_density(alpha=.1, fill="#FF6666") +
  labs(x = 'Edades', y = 'Frecuencia %', title = 'DistribuciÛn de edades', subtitle = 'DistribuciÛn de las edades de los estudiantes...', caption = 'Prueba de Shapiro-Wilk. \n P-value: 0 \n - \n Prueba de Anderson-Darling \n P-value 0 \n - \n Prueba de Kolmogorov-Smirnov \n P-value 0') + 
  scale_x_continuous(breaks = seq(10, 25, 1)) + 
  scale_y_continuous(limits=c(0,0.4), breaks = seq(0,0.4,0.1), labels = percent) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  ) 



#En la gr·fica anterior, se observa que la variable tiene un sesgo positivo que se asemeja al de una distribuciÛn Poisson.
#Adem·s, los resultados obtenidos con las pruebas Shapiro-Wilk, Anderson-Darling y Kolmogorov-Smirnov, nos confirma que la variable no sigue una distribuciÛn normal.


#Boxplot
data %>% select(age) %>% arrange(age) %>%
  ggplot(aes(x = age)) +
  geom_boxplot(color = 'grey30', fill = '#4F5D75') +
  labs(x = 'Edades', y = 'Frecuencia', title = 'Edades', caption = '') + 
  scale_x_continuous(breaks = seq(0, 50, 2)) + 
  scale_y_continuous(breaks = seq(0, 120, 20)) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  )

#En la gr·fica anterior, se puede observar que la distribuciÛn de la variable tiene un sesgo positivo.


#Existen algunas pruebas tales como Shapiro-Wilks, Kolmogorov-Smirnov y
#Anderson-Darling (las veremos en R).


#qqPlot
qqPlot(data$age)
#En el gr·fico Cuantil-Cuantil se puede observar que la variable no sigue una distribuciÛn normal.


# ADDRESS - CUALITATIVA


#Tabla frecuencia
tablaFrecuencias(data$address)
plotBar(data$address)


# Pstatus - Cualitativa


#Tabla frecuencia
tablaFrecuencias(data$Pstatus)
plotBar(data$Pstatus)


######################################################### AN¡LISIS BIVARIADO 


#G3 VS SEX
#Cuantitativa discreta vs Cualitativa nominal

descriptivasNivel(data, sex, G3)
#Seg˙n las estadÌsticas descriptivas, existe una diferencia entre la media de la calificaciÛn final entre hombres y mujeres.

plotBoxplot(data, data$sex, data$G3, title = 'CalificaciÛn final seg˙n el sexo del estudiante', subtitle = 'DistribuciÛn de las calificaciones teniendo en cuenta el sexo del estudiante', ylabel = 'CalificaciÛn final', caption = 'Prueba de anova. \n P-value: 0.0531 \n - \n Prueba t conjunta. \n P-value: 0.0531')
#En el gr·fico anterior, se observa que en promedio los estudiantes hombres obtienen mayor calificaciÛn final que las mujeres

#No hay normalidad en G3. No puedo usar F ni Levene de forma cl·sica

#Se utiliza levene con mediana Brown Forsythe
leveneTest(G3~sex, data = data, center = "median") #Brown-Forsythe - No rechazo Ho
#Varianzas iguales u homogenea

#prueba t conjunta
t.test(G3~sex, data=data, var.equal=T) #prueba t conjunta. #Acepto Ho
#Medias iguales. Aunque fue un valor muy ajustdo.

#aplica tlc, varianzas iguales. Dudas en este punto. Aplica anova a pesar de no tener normalidad y varianzas iguales? Aplica tlc?
#prueba Anova
aovG3Sex <- aov(G3~sex, data = data)
summary(aovG3Sex) #Se rechaza Ho
#anova sobra dado q solo hay 2 pob

#ConclusiÛn:
#Las medias de las calificaciones obtenidas por los hombres y las mujeres son iguales.


#G3 VS AGE
#Cuantitativa discreta vs cuantitativa discreta

#Scatterplot
data %>%
  ggplot(aes(x = age, y = G3)) +
  geom_point() +
  labs(x = 'Edades expresadas en aÒos', y = 'Calificacion final', title = 'CalificaciÛn final seg˙n la edad del estudiante', subtitle = '', caption = 'Coeficiente de correlaciÛn Pearson. \n P-value: 0.007905 \n - \n Prueba de Anderson-Darling \n P-value 0 \n - \n Prueba de Kolmogorov-Smirnov \n P-value 0') + 
  scale_x_continuous(breaks = seq(10, 25, 1)) + 
  scale_y_continuous(limits=c(0,25), breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
    #panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5)
  ) +
  geom_jitter()


#Correlacion
cor(data$age, data$G3)
cor.test(data$age, data$G3)

#Se rechaza que haya correlaciÛn. No esta correlacionado


#G3 VS address

#CUantitativa discreta vs cualitativa nominal
descriptivasNivel(data, address, G3)
#Seg˙n las estadÌsticas descriptivas, existe una diferencia entre la media de la calificaciÛn final entre estudiantes de la zona urbana y la zona rural.

plotBoxplot(data, data$address, data$G3, title = 'CalificaciÛn final seg˙n el tipo de domicilio del estuadiante', subtitle = 'DistribuciÛn de las calificaciones teniendo en cuenta el tipo de domicilio del estudiante', ylabel = 'CalificaciÛn final', caption = 'Prueba de anova. \n P-value: 0.0139 \n - \n Prueba t conjunta. \n P-value: 0.0139')
#En el gr·fico anterior, se observa que en promedio los estudiantes de la zona urbana obtuvieron mayor calificaciÛn final que los de la zona rural

#Normalidad en G3?
qqPlot(data$G3)
shapiro.test(data$G3)
ad.test(data$G3)
lillie.test(data$G3)
#No hay normalidad. No puedo usar F ni Levene de forma cl·sica

#Se utiliza levene con mediana Brown Forsythe
leveneTest(G3~address, data = data, center = "median") #Brown-Forsythe - No rechazo Ho
#Varianzas iguales u homogenea

#prueba t conjunta
t.test(G3~address, data=data, var.equal=T) #prueba t conjunta. #Rechazo Ho
#Medias desiguales

#aplica tlc, varianzas iguales. Dudas en este punto. Aplica anova a pesar de no tener normalidad y varianzas iguales? Aplica tlc?
#prueba Anova
aovG3Address <- aov(G3~address, data = data)
summary(aovG3Address) #Se rechaza Ho
#anova sobra dado q solo hay 2 pob

#ConclusiÛn:
#Hay diferencias entre las calificaciones obtenidas por los estudiantes de la zona rural y la zona urbana.



#G3 VS Pstatus
#Cuantitativa discreta vs cualitativa nominal

descriptivasNivel(data, Pstatus, G3)
#Seg˙n las estadÌsticas descriptivas, existe una diferencia entre la media y varianza de la calificaciÛn final entre estudiantes que sus padre viven juntos y separados.

plotBoxplot(data, data$Pstatus, data$G3, title = 'CalificaciÛn final seg˙n el estado de convivencia de los padres de los estudiantes', subtitle = 'DistribuciÛn de las calificaciones teniendo en cuenta estado de convivencia de los padres de los estudiantes', ylabel = 'CalificaciÛn final', caption = 'Prueba de anova. \n P-value: 0.616 \n - \n Prueba t conjunta. \n P-value: 0.6156')
#AYUDA CON INTERPRETACI”N

#No hay normalidad en G3. No puedo usar F ni Levene de forma cl·sica

#Se utiliza levene con mediana Brown Forsythe
leveneTest(G3~Pstatus, data = data, center = "median") #Brown-Forsythe - No rechazo Ho
#Varianzas iguales u homogenea

#prueba t conjunta
t.test(G3~Pstatus, data=data, var.equal=T) #prueba t conjunta. #Acepto Ho
#Medias iguales.

#aplica tlc, varianzas iguales. Dudas en este punto. Aplica anova a pesar de no tener normalidad y varianzas iguales? Aplica tlc?
#prueba Anova
aovG3Pstatus <- aov(G3~Pstatus, data = data)
summary(aovG3Pstatus) #Se rechaza Ho

#ConclusiÛn:
#Las medias de las calificaciones obtenidas por los estudiantes que sus padres viven juntos o est·n separados son iguales.


# MARIO
