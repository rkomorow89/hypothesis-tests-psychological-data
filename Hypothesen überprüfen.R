setwd("~/Daten-Orchestrierung/Auftrag Lene")
install.packages("readxl")
library("readxl")
df <- read_excel("Daten Variablen .xlsx", na= "NA")
#df <- read.csv2("SUF DEAS 2020.csv", header=TRUE, na= "NA")
View(df)
show(df)
#deskriptive Statistik 
install.packages("Hmisc")
library("Hmisc")
describe(df$lz_20)
describe (df$lone6_20)
describe(df$jp17)
describe(df$jp19)
describe(df$jp21)
describe(df$jp23)
describe(df$jp24)
describe(df$jp26)
describe(df$altervoll_20)
describe(df$jp14)
#table(df$jp14)
describe(df$isced_20) 
table(df$isced_20)  #Kreuztabelle
describe(df$erw_20)
table(df$erw_20)
table(df$geschlecht_20, df$altervoll_20)
hist(df$geschlecht_20,df$altervoll_20)
tab <-
  par(mfrow = c(1, 2))
hist(df$altervoll_20[df$geschlecht_20==1], col="gray", main="Men", xlab="Age")
#hist(df$altervoll_20[df$geschlecht_20==2], col="gray", main = "Women", xlab="Age")
#par(mfrow = c(1, 1))# muss man wieder zurück 
lz1 <- subset(df, lz_20>0)
hist(lz1$lz_20)
hist(lz1$lz_20, main="Distribution Life Satisfaction", xlab = "Life Satisfaction")
ggplot(df, aes(x=lz_20))+ geom_histogram(aes(y=..density..))
+ xlim(1,5)
+ stat_function(fun=dnorm, args=list(mean=mean(df$lz_20), sd=sd(df$lz_20))) 
#Normalverteilungslinie nicht funktioniert
lone <- subset(df, lone6_20>0)
hist(lone$lone6_20, main= "Distribution Loneliness", xlab="Loneliness")
library(psych)
# cronbachs alpha life satisfaction 
alpha(subset(df,select=c(jp1_1,jp1_2,jp1_3,jp1_4,jp1_5)),check.keys=TRUE)
# cronbachs alpha loneliness 
alpha(subset(df,select=c(jp35_1,jp35_2,jp35_3,jp35_4,jp35_5, jp35_6)),check.keys=TRUE)
mean(df$altervoll_20)
sd(df$altervoll_20)
sd(df$altervoll_20, na.rm=FALSE)
table(df$geschlecht_20)
mean(sum(df$geschlecht_20 == 1, na.rm=TRUE))
# einzelene Altersgruppen/ Geschlecht 
#1= Männer 
A1 <- subset(df,altervoll_20<=54 & geschlecht_20 ==1)
show(A1)
mean(A1$altervoll_20,na.rm = TRUE)
sd(A1$altervoll_20,na.rm = TRUE)
A2 <- subset(df, altervoll_20 >=55 & altervoll_20<=69 & geschlecht_20 ==1)
A3 <- subset(df, altervoll_20 >=70 & altervoll_20<=85 & geschlecht_20 ==1)
A4 <- subset(df, altervoll_20 >=86 & altervoll_20<=98 & geschlecht_20 ==1)
A5 <- subset(df, altervoll_20 & geschlecht_20 ==1)
mean(A5$altervoll_20,na.rm = TRUE)
sd(A5$altervoll_20,na.rm = TRUE)
#einzelne Altersgruppen Frauen 
B1 <- subset(df,altervoll_20<=54 & geschlecht_20 ==2)
B2 <- subset(df, altervoll_20 >=55 & altervoll_20<=69 & geschlecht_20 ==2)
B3 <- subset(df, altervoll_20 >=70 & altervoll_20<=85 & geschlecht_20 ==2)
B4 <- subset(df, altervoll_20 >=86 & altervoll_20<=98 & geschlecht_20 ==2)
B5 <- subset(df, altervoll_20 & geschlecht_20 ==2)
mean(B5$altervoll_20,na.rm = TRUE)
sd(B5$altervoll_20,na.rm = TRUE)
#Interkorrelationsmatrix 
subset_cor <-subset(df,select=c() )
korr_tab <- cor(subset_cor)
#Interkorrelationen von Studienvariabeln 
subset_cor <- subset(df, select=c(jp17,jp19,jp21,jp23,jp24,jp26, lz_20, lone6_20))
korr_tab <- cor(subset_cor, method="spearman")
cor.test(subset_cor, method="spearman")# cor.test händisch einzeln, hat nicht funktioniert/ Fehler 
aov(lz_20~jp17*jp19,data=df)
summary(aov(lz_20~jp17*jp23*jp26,data=df))
Partner <-df$jp17
Partner=ifelse(Partner<=0,"NA",Partner)
Partner=ifelse(Partner==3,-1,Partner)
Partner=ifelse(Partner==2,0,Partner)
Partner=as.numeric(Partner)
reg_model<-lm(df$lz_20~df$jp13 +df$jp17+df$jp19)
summary(reg_model)

#Hypothesen überprüfen

#H1. The quality of social relations, during the pandemic, is related to the levels of reported subjective well-being. 

#H1.1 Individuals who report deterioration in at least 2 social domains (e.g., family and partnership) since the onset of the pandemic reveal lower subjective well-being than individuals who report no deterioration or even improvement  in any of the social relations.

#--> (jp 17, jp23, jp 26 als UV, lz_20 als AV) 

#H1.1.1 Individuals who report that their social relations with their partner have become more positive since the onset of the pandemic reveal higher subjective well-being compared to individuals who reported no change or negative change  in their social relations with their partner.

#mit ANOVA und ANCOVA überprüfen

#Individuen ohne Angaben entfernen
C1 <- subset(df, jp17 >= 1)
C2 <- subset(C1, lz_20 >= 1)

install.packages("psych")
library(psych)

#Descriptive Statistik nach Gruppen (Qualität der Partnerbeziehung besser geworden/gleich geblieben/verschlechtert bzw. 1/2/3)
describeBy(C2$lz_20, C2$jp17)
#Erkenntnis: Mittelwerte unterscheiden sich bei Gruppe 3 (3,31) von Gruppe 1 (3,97) und Gruppe 2 (3,95)

#Varianzhomogenität überprüfen mit Levene-Test
install.packages("car")
library (car)

leveneTest(C2$lz_20, C2$jp17)
#Erkenntnis: Varianzhomogenität ist nicht gegeben, da p < 0,05, erkennbar auch daran, das Standardabweichungen der drei Gruppen in nicht gleich sind (0,7/0,64/0,79)
#Voraussetzungen für ANOVA-Test sind eigentlich nicht erfüllt

#ANOVA durchführen
anova_training <- aov(C2$lz_20 ~ C2$jp17)
summary(anova_training)
#Erkenntnis: da p < 0,05, kann die Nullhypothese, dass Gleichheit der Mittelwerte der drei Gruppen besteht, verworfen werden

#Post-hoc-Analyse: paarweise Gruppenvergleiche
pairwise.t.test(C2$lz_20, C2$jp17, p.adjust="bonferroni")
#Erkenntnis: zwischen Gruppe 1 und 2 besteht kein Unterschied (p>0,05), zwischen 1 und 3 sowie 2 und 3 besteht Unterschied (p<0,05)

#Effektstärke berechnen:

#Eta² berechnen
install.packages("DescTools")
library(DescTools)
EtaSq <- EtaSq(anova_training)
eta.sq <- EtaSq[1]

#f-Wert berechnen
sqrt(eta.sq/(1-eta.sq))
#Erkenntnis: da 0,1 < f < 0,25 ist, ist ein schwacher Unterschied der Mittelwerte vorhanden, H1.1.1 kann somit bestätigt werden

#ANCOVA durchführen
ancova_model<- aov(C2$lz_20 ~ C2$jp17 + C2$altervoll_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Alter

ancova_model <- aov(C2$lz_20 ~ C2$jp17 + C2$geschlecht_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Geschlecht

ancova_model <- aov(C2$lz_20 ~ C2$jp17 + C2$isced_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Bildung

######################################

#H1.1.2 Individuals who report that their social relations with their family have become more positive since the onset of the pandemic reveal higher subjective well-being compared to individuals who reported no change or negative change in their social relations with their family.

#mit ANOVA und ANCOVA überprüfen

#Individuen ohne Angaben entfernen
D1 <- subset(df, jp23 >= 1)
D2 <- subset(D1, lz_20 >= 1)

#Descriptive Statistik nach Gruppen (Qualität der Familienbeziehung besser geworden/gleich geblieben/verschlechtert bzw. 1/2/3)
describeBy(D2$lz_20, D2$jp23)
#Erkenntnis: Mittelwerte unterscheiden sich bei Gruppe 3 (3,53) von Gruppe 1 (3,9) und Gruppe 2 (3,89)

#Varianzhomogenität überprüfen mit Levene-Test

leveneTest(D2$lz_20, D2$jp23)
#Erkenntnis: Varianzhomogenität ist nicht gegeben, da p < 0,05, erkennbar auch daran, das Standardabweichungen der drei Gruppen in nicht gleich sind (0,74/0,69/0,82)
#Voraussetzungen für ANOVA-Test sind eigentlich nicht erfüllt

#ANOVA durchführen
anova_training <- aov(D2$lz_20 ~ D2$jp23)
summary(anova_training)
#Erkenntnis: da p < 0,05, kann die Nullhypothese, dass Gleichheit der Mittelwerte der drei Gruppen besteht, verworfen werden

#Post-hoc-Analyse: paarweise Gruppenvergleiche
pairwise.t.test(D2$lz_20, D2$jp23, p.adjust="bonferroni")
#Erkenntnis: zwischen Gruppe 1 und 2 besteht kein Unterschied (p>0,05), zwischen 1 und 3 sowie 2 und 3 besteht Unterschied (p<0,05)

#Effektstärke berechnen:

#Eta² berechnen
EtaSq <- EtaSq(anova_training)
eta.sq <- EtaSq[1]

#f-Wert berechnen
sqrt(eta.sq/(1-eta.sq))
#Erkenntnis: da f < 0,1 ist, ist kein Unterschied der Mittelwerte vorhanden, H1.1.2 kann somit nicht bestätigt werden

#ANCOVA durchführen
ancova_model<- aov(D2$lz_20 ~ D2$jp23 + D2$altervoll_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Alter

ancova_model <- aov(D2$lz_20 ~ D2$jp23 + D2$geschlecht_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Geschlecht

ancova_model <- aov(D2$lz_20 ~ D2$jp23 + D2$isced_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Bildung

#####################################

#H1.1.3 Individuals who report that their social relations with their friends have become more positive since the onset of the pandemic reveal higher subjective well-being compared to individuals who reported no change or negative change in their social relations with their friends.

#mit ANOVA und ANCOVA überprüfen

#Individuen ohne Angaben entfernen
E1 <- subset(df, jp26 >= 1)
E2 <- subset(E1, lz_20 >= 1)

#Descriptive Statistik nach Gruppen (Qualität der Freundesbeziehung besser geworden/gleich geblieben/verschlechtert bzw. 1/2/3)
describeBy(E2$lz_20, E2$jp26)
#Erkenntnis: Mittelwerte unterscheiden sich bei Gruppe 3 (3,59) von Gruppe 1 (3,77) und Gruppe 2 (3,9)

#Varianzhomogenität überprüfen mit Levene-Test

leveneTest(E2$lz_20, E2$jp26)
#Erkenntnis: Varianzhomogenität ist nicht gegeben, da p < 0,05, erkennbar auch daran, das Standardabweichungen der drei Gruppen in nicht gleich sind (0,76/0,68/0,79)
#Voraussetzungen für ANOVA-Test sind eigentlich nicht erfüllt

#ANOVA durchführen
anova_training <- aov(E2$lz_20 ~ E2$jp26)
summary(anova_training)
#Erkenntnis: da p < 0,05, kann die Nullhypothese, dass Gleichheit der Mittelwerte der drei Gruppen besteht, verworfen werden

#Post-hoc-Analyse: paarweise Gruppenvergleiche
pairwise.t.test(E2$lz_20, E2$jp26, p.adjust="bonferroni")
#Erkenntnis: zwischen Gruppe 1 und 2 besteht kein Unterschied (p>0,05), zwischen 1 und 3 sowie 2 und 3 besteht Unterschied (p<0,05)

#Effektstärke berechnen:

#Eta² berechnen
EtaSq <- EtaSq(anova_training)
eta.sq <- EtaSq[1]

#f-Wert berechnen
sqrt(eta.sq/(1-eta.sq))
#Erkenntnis: da 0,1 < f < 0,25 ist, ist ein schwacher Unterschied der Mittelwerte vorhanden, H1.1.3 kann somit bestätigt werden

#ANCOVA durchführen
ancova_model<- aov(E2$lz_20 ~ E2$jp26 + E2$altervoll_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Alter

ancova_model <- aov(E2$lz_20 ~ E2$jp26 + E2$geschlecht_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Geschlecht

ancova_model <- aov(E2$lz_20 ~ E2$jp26 + E2$isced_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Bildung

#Da Unterschied der Mittelwerte bei zwei von drei Sub-Hypothesen festgestellt werden kann (H1.1.1 und H1.1.3), kann H1.1 und somit auch H1 bestätigt werden

###################################################

#H2 The subjective well-being among individuals reporting contact reduction during the pandemic differs for older adults and individuals in their middle age.
#--> (jp 19, jp 21, jp24 als UV, lz_20 als AV) 

#mit ANOVA und ANCOVA überprüfen

#Individuen ohne Angaben entfernen
F1 <- subset(df, jp19 >= 1)
F2 <- subset(F1, lz_20 >= 1)

#Descriptive Statistik nach Gruppen (Veränderung des Kontakts zu Partner (oder Eltern?) häufiger geworden/gleich geblieben/seltener geworden bzw. 1/2/3)
describeBy(F2$lz_20, F2$jp19)
#Erkenntnis: Mittelwerte unterscheiden sich bei Gruppe 3 (3,81) kaum von Gruppe 1 (3,83) und Gruppe 2 (3,84)

#Varianzhomogenität überprüfen mit Levene-Test

leveneTest(F2$lz_20, F2$jp19)
#Erkenntnis: Varianzhomogenität ist gegeben, da p > 0,05, erkennbar auch daran, das Standardabweichungen der drei Gruppen in etwa gleich sind (0,72/0,72/0,73)
#Voraussetzungen für ANOVA-Test sind damit erfüllt

#ANOVA durchführen
anova_training <- aov(F2$lz_20 ~ F2$jp19)
summary(anova_training)
#Erkenntnis: da p > 0,05, kann die Nullhypothese, dass Gleichheit der Mittelwerte der drei Gruppen besteht, nicht verworfen werden

#Post-hoc-Analyse: paarweise Gruppenvergleiche
pairwise.t.test(F2$lz_20, F2$jp19, p.adjust="bonferroni")
#Erkenntnis: zwischen den drei Gruppen bestehen keine paarweisen Unterschiede in den Mittelwerten (p>0,05 immer gegeben)

#Effektstärke berechnen:

#Eta² berechnen
EtaSq <- EtaSq(anova_training)
eta.sq <- EtaSq[1]

#f-Wert berechnen
sqrt(eta.sq/(1-eta.sq))
#Erkenntnis: da f < 0,1 ist, ist kein Unterschied der Mittelwerte vorhanden

#ANCOVA durchführen
ancova_model<- aov(F2$lz_20 ~ F2$jp19 + F2$altervoll_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p > 0,05, ist kein Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Alter

ancova_model <- aov(F2$lz_20 ~ F2$jp19 + F2$geschlecht_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p > 0,05, ist kein Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Geschlecht

ancova_model <- aov(F2$lz_20 ~ F2$jp19 + F2$isced_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p > 0,05, ist kein Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Bildung

#########################################

#Individuen ohne Angaben entfernen
G1 <- subset(df, jp21 >= 1)
G2 <- subset(G1, lz_20 >= 1)

#Descriptive Statistik nach Gruppen (Veränderung des Kontakts zur Familie häufiger geworden/gleich geblieben/seltener geworden bzw. 1/2/3)
describeBy(G2$lz_20, G2$jp21)
#Erkenntnis: Mittelwerte unterscheiden sich bei Gruppe 3 (3,78) leicht von Gruppe 1 (3,93) und Gruppe 2 (3,9)

#Varianzhomogenität überprüfen mit Levene-Test

leveneTest(G2$lz_20, G2$jp21)
#Erkenntnis: Varianzhomogenität ist nicht gegeben, da p < 0,05, erkennbar auch daran, das Standardabweichungen der drei Gruppen in nicht gleich sind (0,67/0,69/0,74)
#Voraussetzungen für ANOVA-Test sind eigentlich nicht erfüllt

#ANOVA durchführen
anova_training <- aov(G2$lz_20 ~ G2$jp21)
summary(anova_training)
#Erkenntnis: da p < 0,05, kann die Nullhypothese, dass Gleichheit der Mittelwerte der drei Gruppen besteht, verworfen werden

#Post-hoc-Analyse: paarweise Gruppenvergleiche
pairwise.t.test(G2$lz_20, G2$jp21, p.adjust="bonferroni")
#Erkenntnis: zwischen Gruppe 1 und 2 besteht kein Unterschied (p>0,05), zwischen 1 und 3 sowie 2 und 3 besteht Unterschied (p<0,05)

#Effektstärke berechnen:

#Eta² berechnen
EtaSq <- EtaSq(anova_training)
eta.sq <- EtaSq[1]

#f-Wert berechnen
sqrt(eta.sq/(1-eta.sq))
#Erkenntnis: da f < 0,1 ist, ist kein Unterschied der Mittelwerte vorhanden

#ANCOVA durchführen
ancova_model<- aov(G2$lz_20 ~ G2$jp21 + G2$altervoll_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Alter

ancova_model <- aov(G2$lz_20 ~ G2$jp21 + G2$geschlecht_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Geschlecht

ancova_model <- aov(G2$lz_20 ~ G2$jp21 + G2$isced_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Bildung

##############################################

#Individuen ohne Angaben entfernen
J1 <- subset(df, jp24 >= 1)
J2 <- subset(J1, lz_20 >= 1)

#Descriptive Statistik nach Gruppen (Veränderung des Kontakts zU Freunden häufiger geworden/gleich geblieben/seltener geworden bzw. 1/2/3)
describeBy(J2$lz_20, J2$jp24)
#Erkenntnis: Mittelwerte unterscheiden sich bei Gruppe 3 (3,85) kaum von Gruppe 1 (3,86) und Gruppe 2 (3,89)

#Varianzhomogenität überprüfen mit Levene-Test

leveneTest(J2$lz_20, J2$jp24)
#Erkenntnis: Varianzhomogenität ist gegeben, da p > 0,05, erkennbar auch daran, das Standardabweichungen der drei Gruppen in etwa gleich sind (0,69/0,71/0,69)
#Voraussetzungen für ANOVA-Test sind damit erfüllt

#ANOVA durchführen
anova_training <- aov(J2$lz_20 ~ J2$jp24)
summary(anova_training)
#Erkenntnis: da p > 0,05, kann die Nullhypothese, dass Gleichheit der Mittelwerte der drei Gruppen besteht, nicht verworfen werden

#Post-hoc-Analyse: paarweise Gruppenvergleiche
pairwise.t.test(J2$lz_20, J2$jp24, p.adjust="bonferroni")
#Erkenntnis: zwischen den drei Gruppen bestehen keine paarweisen Unterschiede in den Mittelwerten (p>0,05 immer gegeben)

#Effektstärke berechnen:

#Eta² berechnen
EtaSq <- EtaSq(anova_training)
eta.sq <- EtaSq[1]

#f-Wert berechnen
sqrt(eta.sq/(1-eta.sq))
#Erkenntnis: da f < 0,1 ist, ist kein Unterschied der Mittelwerte vorhanden

#ANCOVA durchführen
ancova_model<- aov(J2$lz_20 ~ J2$jp24 + J2$altervoll_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Alter

ancova_model <- aov(J2$lz_20 ~ J2$jp24 + J2$geschlecht_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Geschlecht

ancova_model <- aov(J2$lz_20 ~ J2$jp24 + J2$isced_20)
Anova(ancova_model, type = "III")
#Erkenntnis: Da p < 0,05, ist Einfluss vorhanden, auch unter Berücksichtigung der Kontrollvariable Bildung

########################################################

#H3 Individuals who feel lonelier during the pandemic report lower subjective well-being.
#mit Korrelation + Regression überprüfen

#Individuen ohne Angabe (-6) entfernen
K1 = subset(df, lone6_20 >= 1)
describe(K1$lone6_20)
K2 = subset(K1, lz_20 >= 1)
describe(K2$lz_20)
#Diagramm erstellen
plot(x = K2$lone6_20, y = K2$lz_20, xlab= "Einsamkeit", ylab = "Lebenszufriedenheit")
#Korrelation berechnen
cor(x = K2$lone6_20, y = K2$lz_20)
cor.test(x = K2$lone6_20, y = K2$lz_20)
#Regression durchführen
lm = lm(K2$lz_20 ~ K2$lone6_20)
abline(lm)
#Ergebnis: mittelstarke negative Korrelation ist vorhanden (-0,45), p-value ist < 0,05, daher kann H3 als wahr angenommen werden














