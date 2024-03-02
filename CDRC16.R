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

