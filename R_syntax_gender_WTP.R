#### HAUSARBEIT KLIMAWANDEL BEHNKE ####
#### load packages ####

dir()
library(tidyverse)
library(haven)
library(sjPlot)
library(ggplot2)
library(dplyr)
library(magrittr)
library(psych)
#### Laden des Datensatzes ####

dat<-read.csv2("Umfrage_Corona_Klimawandel.csv")
names(dat)
attach(dat)

#### UNIVARIATE ANALYSE ####
    #### GESCHLECHT####

        table(v_382)
        mann<-v_382
        table(mann)
        mann[mann==2]<-0
        table(mann)
        with(dat, table(mann)
             %>% prop.table())
        #0=Frau, 1=Mann
        
        
#### Übersicht über mehrere Variablen zur allgemeinen Ansicht ####
    #### Alter ####
table(v_1033)
alter<-v_1033
        alter[alter==5]<-NA
        table(alter)
        barplot(table(alter))
        hist(alter)
        mean(alter, na.rm = T)
        
       #### zusammenfassen von 3 Altersgruppen #### 
        dat %<>% within({
          age <- alter
          age[alter %in% c(17:30)] <- 1
          age[alter %in% c(31:50)] <- 2
          age[alter %in% c(51:86)] <- 3
        }) 
        
        dat%<>% within({
          age <- factor(age, levels = c(1:3), 
                                         labels = c("bis 30","bis 50", "bis 86"))
        }) 
        table(dat$age)
        prop.table(table(dat$age))
        
    #### Bildung #####
        table(v_384)
        bildung<-v_384
        table(bildung)
        prop.table(table(bildung))
        barplot(table(bildung))
        hist(bildung)
        mean(bildung, na.rm = T)       
        
    #### Einkommen ####
        table(v_318)
        einkommen<-v_318
        einkommen[einkommen==-77]<-NA
        table(einkommen)
        prop.table(table(einkommen))
        barplot(table(einkommen))
        hist(einkommen )
        mean(einkommen, na.rm = T) 
        

#### POLITISCHE ORIENTIERUNG ####

        table(v_235)
        lire_selbst<-v_235
        lire_selbst[lire_selbst==-77]<-NA
        table(lire_selbst)
        barplot(table(lire_selbst))

    #### VARIABLE DICHOTOMISIEREN ####

            dat %<>% within({
              lire_selbst_dichotom <- v_235
              lire_selbst_dichotom[v_235 %in% c(1, 2, 3, 4, 5)] <- 1
              lire_selbst_dichotom[v_235 %in% c(6, 7, 8, 9, 10, 11)] <- 2
            }) 
            
            dat%<>% within({
              lire_selbst_dichotom <- factor(lire_selbst_dichotom, levels = c(1,2), 
                                             labels = c("liberaler","konservativer"))
            }) 
            table(dat$lire_selbst_dichotom)
            table(lire_selbst)

    #### Mean und SD ####

            mean(lire_selbst, na.rm = T)
            sd(lire_selbst, na.rm = T)

#### WTP ####

        wp_steuern <- v_221
        wp_konsum <- v_222
        wp_lebensstandard <- v_223
        wp_benzin <- v_224

    #### wp_steuern ####

            table(wp_steuern)
            wp_steuern[wp_steuern==-77]<-NA
            table(wp_steuern)
            barplot(table(wp_steuern))
            hist(wp_steuern)

    #### wp_konsum ####

            table(wp_konsum)
            wp_konsum[wp_konsum==-77]<-NA
            table(wp_konsum)
            hist(wp_konsum)

    #### wp_lebensstandard ####

            table(wp_lebensstandard)
            wp_lebensstandard[wp_lebensstandard==-77]<-NA
            table(wp_lebensstandard)
            hist(wp_lebensstandard)

    ####wp_benzin####

            table(wp_benzin)
            wp_benzin[wp_benzin==-77]<-NA
            table(wp_benzin)
            hist(wp_benzin)

    #### Mittelwert & SD####

            mean(willpay,na.rm=T)
            ?sd
            sd(willpay, na.rm=T)
            par(mfrow=c(2,2))
            hist(wp_steuern,xlim=range(0,100,10),ylim = c(0,300))
            hist(wp_konsum,xlim=range(0,100,10),ylim = c(0,300))
            hist(wp_lebensstandard,xlim=range(0,100,10),ylim = c(0,300))
            hist(wp_benzin,xlim=range(0,100,10),ylim = c(0,300))

    #### Gesamtindex der WTP####
            willpay<-(wp_steuern+wp_konsum+wp_lebensstandard+wp_benzin)/4
            par(mfrow=c(1,1))
            table(willpay)
            hist(willpay,xlim=range(0,100,10),ylim = c(0,200))
            mean(willpay, na.rm=T)
            
#### UMWELTWERTE ####
    #--> Durchschnitt der drei Ausprägungen davon zusammen
            
        env_values <-(v_268+v_275+v_343)/3
        table(env_values)
        env_values[env_values<0]<-NA
        table(env_values)
        mean(env_values, na.rm=T)
        str(env_values)
        show(env_values)
        head(env_values)
        hist(env_values,xlim=range(1,5))

    #### Mittelwert & SD ####
            mean(v_268)
            mean(v_275)
            mean(v_343)
            mean(env_values, na.rm = T)
            sd(env_values, na.rm=T)
            
#### CFC ####
    #### Reverse Scoring der Variable v_274####
            show(v_274)
            summary(v_274)
            str(v_274)
            class(v_274)
            is.numeric(v_274)
            v_274[v_274<0]<-NA
            table(v_274)
            
    v_274_1<-(v_274-max(v_274, na.rm = T))*-1
            # Newvar <- (oldvar-max(oldvar) ) * -1
            table(v_274)
            table(v_274_1)
            
            
            
   #### Zusammenfügen der CFC Variablen ####       
            
      #table(dat$v_269)
           # dat$v_269[dat$v_269<0]<-NA
           # table(dat$v_269)
            #table(v_273)
            #v_273[dat$v_273<0]<-NA
            
            
            cfc <-(v_269+v_273+v_274_1+v_342+v_346)/5
            table(cfc)
            cfc[cfc<0]<-NA
            table(cfc)
            mean(cfc, na.rm=T)
            sd(cfc, na.rm = T)
            str(cfc)
            show(cfc)
            head(cfc)
            hist(cfc,xlim=range(1,5))
            
            
            



#### BIVARIATE ANALYSE ####
    #### WTP, Geschlecht, politische Orientierung, Umweltwerte --> Multiple Regression####
          
            lm(willpay~v_382+lire_selbst+env_values)
            summary(lm(willpay~v_382+lire_selbst+env_values))

            par(mfrow=c(1,1))
            plot(v_382,willpay)
            plot(v_382+rnorm(788,0,0.3),willpay)
            abline(coef(lm(willpay~v_382)))
            
            
    #### +++ politische Einstellung (AV) & Geschlecht (UV) --> chi quadrat test####
              # je liberaler die politische Einstellung, desto höher ist die Geschlechterdifferenz
              # H0: die Mittelwerte sind gleich
              # H1: die Mittelwerte sind ungleich
        #Durchführen des statistischen Tests
            
            chisq.test(table(dat$v_382, dat$lire_selbst_dichotom))

            # Aufstellen der Kreuztabelle
            
            sjt.xtab(dat$lire_selbst_dichotom, dat$v_382, show.col.prc = T)
    #### politische Einstellung (UV) & Umweltwerte (AV) --> Mittelwertsvergleich t-test ####
            
            table(dat$lire_selbst_dichotom)
            mean(env_values[dat$lire_selbt_dichotom=="liberaler"], na.rm = T)
            mean(env_values[dat$lire_selbst_dichotom=="konservativer"],na.rm=T)
            tapply(env_values,dat$lire_selbst_dichotom,mean,na.rm=T)

            t.test(env_values~dat$lire_selbst_dichotom, data=dat, 
                   var.equal=TRUE,
                   conf.level=0.95)

    #### politische Einstellung (UV) & WTP (AV) --> Mittelwertsvergleich t-test ####
              
            table(dat$lire_selbst_dichotom)
            mean(willpay[dat$lire_selbt_dichotom=="liberaler"],na.rm=T)
            mean(willpay[dat$lire_selbst_dichotom=="konservativer"],na.rm=T)
            tapply(willpay,dat$lire_selbst_dichotom,mean,na.rm=T)
            
            t.test(willpay~dat$lire_selbst_dichotom, data=dat, 
                   var.equal=TRUE,
                   conf.level=0.95)
            

    #### CFC (AV) & Geschlecht (UV)--> t-test/Mittelwertvergleich ####

            
            table(cfc)
            mean(cfc[v_382=="1"],na.rm=T)
            mean(cfc[v_382=="2"],na.rm=T)
            tapply(cfc,v_382,mean,na.rm=T)
            
            t.test(cfc~dat$v_382, data=dat, 
                   var.equal=TRUE,
                   conf.level=0.95)
            
            
            
    #### CFC (UV) & WTP (AV) --> lineare Regression ####
            lm(willpay~cfc)
            summary(lm(willpay~cfc))
          
            plot(cfc+rnorm(788,0,0.3),
                 willpay, 
                 main = "Zusammenhang von CFC und WTP", 
                 xlab = "Sorge um zukünftige Konsequenzen",
                 ylab = "Zahlungsbereitschaft")
            abline(coef(lm(willpay~cfc)), col="red")   
            #?abline