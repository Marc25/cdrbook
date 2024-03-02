# Capítulo 16 Modelos lineales generalizados

# 16.6.2 Ejemplo de regresión de Poisson
# A partir del mismo conjunto de datos, cleveland, ahora se considera como variable a explicar, 
# variable respuesta, dhosp, el número de días de hospitalización de un paciente. 
# Como variables explicativas se seleccionan las siguientes: diag, edad, sexo y tdolor, 
# que son de distinto tipo, lo que permitirá ilustrar sus distintas interpretaciones.
# 
dtClev<-data.table(CDR::cleveland)
# diag: ha suf enf coronoraia o no 
# dhosp numero de dias de hosp
dtClev %>% 
ggplot(aes(dhosp,fill=diag))+
  geom_bar(position="dodge")

# tiene mas dias de hosp cuando tiene diag =1:  pocos pacientes tienen hosp mas de 3-4 dias 


dtClev %>% 
  ggplot(aes(x=as.factor(dhosp), edad))+
  geom_boxplot()

str(dtClev)
dtClev[, edad:=as.integer(edad)]
dtClev[, dep:=as.numeric(dep)]
dhos_rp <- glm(dhosp ~., family="poisson", data=dtClev) 

summary(dhos_rp)
exp(coef(dhos_rp))
plot(dhos_rp)


# Viendo el coef de diag1 el numero medio de dias en el hosp es 3 veces mayor para cl
# con diag1 que resp al diag0
# sexo1 : el coef 1.23 indica que el sexo1 hombre incrementa el numeero de dias mas que una mujer

# Ajuste02 ----
dhos_rp2 <- glm(dhosp ~ diag + sexo+edad, family = "poisson", data=dtClev)
summary(dhos_rp2)

# Adecuación ----
# En los ajustes reg_pois y reg_pois2 hay información sobre la null y la residual deviance, con las que se puede realizar el contraste de comparación de modelos (el simple frente al elaborado) mencionado en la Sec. 16.4.2.
# Contratamos entoncs el modelo frente al modelo nulo la media de la var
pchisq(dhos_rp$deviance, dhos_rp$df.residual, lower.tail = F)
pchisq(dhos_rp2$deviance, dhos_rp$df.residual, lower.tail = F)
# Al ser los p–valores superiores a 0,05, 
# se puede considerar que ambos modelos explican “mejor” que el modelo nulo, 
# el que únicamente contiene la constante y el término aleatorio.
# 
# Adecuaci'on del Modelo 
La diferencia entre la deviance de un modelo más elaborado y el simple se distribuye como una 
χ
2
con tantos grados de libertad como restricciones impuestas a los parámetros, lo que permite contrastar cuál de los dos modelos ajusta mejor los datos. P–valores bajos indican que el modelo ajustado es inadecuado, debiendo investigarse otras posibles variables predictoras, si se incumple la hipótesis de linealidad o existe sobredispersión (por ejemplo, por exceso de ceros).



