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


# ANÁLISIS UNIVARIADO


# G3

## Tabla de información G3
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
internet_table %>% ggplot(aes(x=var, y= percentage)) +
  geom_bar(stat = "identity", fill="steelblue", color="black") +
  labs(x="Internet", y="Frecuencia") +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,20))
  

## Diagrama de torta
internet_table %>% ggplot(aes(x=2, y=percentage, fill=var)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  labs(fill="Acceso a Internet") + 
  theme_void() +
  geom_text(aes(label = percent(percentage)),position = position_stack(vjust = 0.5),color = "white", size=5) +
  xlim(0.5, 2.5) 

#create_report(data)

