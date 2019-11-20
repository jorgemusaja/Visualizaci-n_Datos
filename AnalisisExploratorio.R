
rm(list = ls())
setwd("/home/jorge/Dropbox/Semestre_03/Visualización_Datos/A5 _ Proyecto de visualización de datos. Familiarízate con los datos (PEC2)")

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(openxlsx)

################################################################################

datos <- read_csv("pax_data_195_agreements_16-11-19.csv")

table(datos$ImSrc, useNA = "alw")
datos <- datos[,1:((dim(datos)[2])-1)]

################################################################################

#glimpse(datos)

glimpse(datos[,1:25])

#------------------------------------------------------------------------------#

tabla1 <- table(datos$Con, datos$Contp)
write.xlsx(tabla1, "tabla1.xlsx")

#tabla2 <- table(datos$Con, datos$Status, datos$Agtp)
tabla2 <- datos %>%
  group_by(Con, Status, Agtp) %>%
  summarise(Frecuencia = n())
write.xlsx(tabla2, "tabla2.xlsx")

#------------------------------------------------------------------------------#

boxplot(data=datos, Lgt ~ Stage, horizontal = T)

tabla3 <- datos %>%
  group_by(Stage) %>%
  summarise(Mediana = median(Lgt),
            Media = mean(Lgt))
write.xlsx(tabla3, "tabla3.xlsx")

#------------------------------------------------------------------------------#

datos2 <- datos[,26:dim(datos)[2]]

a = vector()

for (i in 1:dim(datos2)[2]) {
  a[i] = dim(datos2)[1] - sum(datos2[,i]==0) 
  # El minuendo es 195
  # El sustraendo en el número veces que aparecen los ceros
  # El resultado es el número de veces que aparecen valores distintos de cero
}

a = data.frame(a)
a$name = colnames(datos2)

39/195
0.20*195

a = filter(a, a>=39)
a$name

datos_cut <- select(datos, a$name)

"
for (i in 1:dim(datos_cut)[2]) {
  print(table(datos_cut[,i]))
}
"

#------------------------------------------------------------------------------#

#length(unique(datos$AgtId))
datos_ex <- data.frame(datos$AgtId)

datos_ex$Indigenous_people <- ifelse(datos_cut$GInd==0, "Not mentioned",
                                     ifelse(datos_cut$GInd==1, "indigenous peoples are merely mentioned",
                                            ifelse(datos_cut$GInd==2, "the agreement contains provision(s) on indigenous people",
                                                   ifelse(datos_cut$GInd==3, "the agreement deals with issues related to indigenous people in a substantive and substantial way",
                                                          NA))))

datos_ex$Women_girls_gender <- ifelse(datos_cut$GeWom==0, "Not mentioned",
                                      ifelse(datos_cut$GeWom==1, "if any of the peace agreement provisions are specifically addressing women, their inclusion, and their rights",
                                             NA))

datos_ex$Civil_Society <- ifelse(datos_cut$Civso==0, "Not mentioned",
                                 ifelse(datos_cut$Civso==1, "if the peace agreement includes any provisions specifically addressed at the inclusion of civil society",
                                        NA))

datos_ex$Human_Rights_Rule_of_Law <- ifelse(datos_cut$HrGen==0, "Not mentioned",
                                            ifelse(datos_cut$HrGen==1, "if the peace agreement includes any general references and rhetorical commitment to human rights",
                                                   NA))

datos_ex$Equality <- ifelse(datos_cut$EqGen==0, "Not mentioned",
                            ifelse(datos_cut$EqGen==1, "rhetorical provision or mention of equality in the agreement",
                                   ifelse(datos_cut$EqGen==2, "substantive provisions on equality",
                                          ifelse(datos_cut$EqGen==3, "detailed substantive provisions on equality, suggesting commitment",
                                                 NA))))

datos_ex$Democracy <- ifelse(datos_cut$HrDem==0, "Not mentioned",
                             ifelse(datos_cut$HrDem==1, "rhetorical provisions/ mention of democracy",
                                    ifelse(datos_cut$HrDem==2, "substantive provisions on democracy",
                                           ifelse(datos_cut$HrDem==3, "detailed substantive provisions, indicating commitment",
                                                  NA))))

datos_ex$Protection_measures <- ifelse(datos_cut$Prot==0, "Not mentioned",
                                       ifelse(datos_cut$Prot==1, "rhetorical provisions or mention of protection measures",
                                              ifelse(datos_cut$Prot==2, "substantive provisions concerning the manner in which protection measures are implemented",
                                                     ifelse(datos_cut$Prot==3, "detailed provisions for the implementation of protection measures",
                                                            NA))))

datos_ex$Media_and_communication <- ifelse(datos_cut$Med==0, "Not mentioned",
                                           ifelse(datos_cut$Med==1, "rhetorical provisions, mention of media and communication, but no details or substantive content",
                                                  ifelse(datos_cut$Med==2, "substantive provisions on media and communication",
                                                         ifelse(datos_cut$Med==3, "detailed substantive provisions, indicating commitment",
                                                                NA))))

datos_ex$Development_socio_economic_reconstruction <- ifelse(datos_cut$Dev==0, "Not mentioned",
                                                             ifelse(datos_cut$Dev==1, "only have a cursory, rhetorical mention",
                                                                    ifelse(datos_cut$Dev==2, "provide some detail on arrangements to support development or reconstruction",
                                                                           ifelse(datos_cut$Dev==3, "agreements which provide details on economic development and reconstruction plans",
                                                                                  NA))))

datos_ex$Socio_Economic_Development <- ifelse(datos_cut$DevSoc==0, "Not mentioned",
                                              ifelse(datos_cut$DevSoc==1, "the agreement contains any provisions for development in general",
                                                                    NA))

datos_ex$Security_Sector <- ifelse(datos_cut$SsrGua==0, "Not mentioned",
                                   ifelse(datos_cut$SsrGua==1, "if the peace agreement contains provisions dealing with the security guarantees",
                                          NA))

datos_ex$Ceasefire <- ifelse(datos_cut$Ce==0, "Not mentioned",
                             ifelse(datos_cut$Ce==1, "general reference to ceasefires, but no mention of a concrete mechanism or process",
                                    ifelse(datos_cut$Ce==2, "reference to a concrete mechanism or process, but in more general, less enforceable terms",
                                           ifelse(datos_cut$Ce==3, "reference to a concrete ceasefire mechanism or process, detailed and enforceable",
                                                  NA))))

datos_ex$Disarmament_Demobilisation_Reintegration <- ifelse(datos_cut$SsrDdr==0, "Not mentioned",
                                                            ifelse(datos_cut$SsrDdr==1, "general references to DDR, no mention of a concrete mechanism/process",
                                                                   ifelse(datos_cut$SsrDdr==2, "reference to a concrete mechanism or process, but in a more general manner and with less enforceable terms",
                                                                          ifelse(datos_cut$SsrDdr==3, "reference to DDR, with a concrete mechanism or process that is enforceable and specified in detailed",
                                                                                 NA))))

datos_ex$DDR_Programme <- ifelse(datos_cut$DdrProg==0, "Not mentioned",
                                 ifelse(datos_cut$DdrProg==1, "if the peace agreement provides for an actual DDR programme",
                                        NA))

datos_ex$Rebel_opposition_Para_statal_forces <- ifelse(datos_cut$SsrPsf==0, "Not mentioned",
                                                       ifelse(datos_cut$SsrPsf==1, "if the peace agreement includes any mention or references to how rebel/opposition group/forces",
                                                              NA))

datos_ex$Reconciliation <- ifelse(datos_cut$TjNR==0, "Not mentioned",
                                  ifelse(datos_cut$TjNR==1, "includes general references to reconciliation",
                                         ifelse(datos_cut$TjNR==2, "provisions going beyond a mere call for reconciliation to establish some sort of reconciliation measure or activity in its own right",
                                                ifelse(datos_cut$TjNR==3, "detailed reconciliation measure",
                                                       NA))))

datos_ex$International_Mission_Force_Similar <- ifelse(datos_cut$ImPK==0, "Not mentioned",
                                                       ifelse(datos_cut$ImPK==1, "if the peace agreement includes any provision to deploy peacekeepers or other international teams with a similar function",
                                                              NA))

datos_ex$Enforcement_Mechanism <- ifelse(datos_cut$ImE==0, "Not mentioned",
                                         ifelse(datos_cut$ImE==1, "if the peace agreement includes any mechanism by which the agreement specifically provides for its own enforcement",
                                                NA))

#------------------------------------------------------------------------------#

colnames(datos_ex) <- ifelse(colnames(datos_ex)=="datos.AgtId", "key", colnames(datos_ex))

#------------------------------------------------------------------------------#

datos_basic <- datos[,1:25]

colnames(datos_basic) <- ifelse(colnames(datos_basic)=="AgtId", "key", colnames(datos_basic))

sum(datos_basic$key == datos_ex$key)

################################################################################

write.xlsx(datos_basic, "datos_basic.xlsx")
write.xlsx(datos_ex, "datos_ex.xlsx")

################################################################################
