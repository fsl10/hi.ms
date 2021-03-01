# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  cat('##############################################################################
        ### clean
        rm(list=ls())
        gc()
        gc(reset=T)
        gc()
        # rm(list=ls()[!grepl("^DT",ls())])
        ##############################################################################
        ### set dir
        setwd("~/statin_final/")
        medpath<-"~/medcode/"
        saspath<-"~/sas_table/"
        sickpath<-"~/sickcode/"
        statinpath<-"~/statin/"
        basepath<-"~/base_dt/"
        savepath<-"~/statin_final/"
        
        ##############################################################################
        ### library
        library(data.table)
        library(haven)
        library(lubridate)
        library(tableone)
        library(moonBook)
        library(ztable)
        library(bit64)
        library(devtools)
        library(parallel)
        library(MatchIt)
        library(survival)
        library(survminer)
        library(emil) #pv
        
        # devtools::install_github("fsl10/hi.ms")
        ##############################################################################
        ### Function
        SICK_CD<-function(DT20,DT40,CODE) {
          setkey(DT20, MAIN_SICK)
          setkey(CODE, V1)
          SICK_MAIN <- DT20[CODE,.(JID,MID,Dx_CODE=MAIN_SICK,Dx_DATE=RECU_FR_DD),nomatch=0]
          
          setkey(DT20, SUB_SICK)
          setkey(CODE, V1)
          SICK_SUB <- DT20[CODE,.(JID,MID,Dx_CODE=SUB_SICK,Dx_DATE=RECU_FR_DD),nomatch=0]
          
          SICK_DT20<-funion(SICK_MAIN,SICK_SUB)
          
          setkey(DT40, SICK_CD)
          setkey(CODE, V1)
          SICK_DT40 <- DT40[CODE,.(JID,MID,Dx_CODE=SICK_CD,Dx_DATE=RECU_FR_DD),nomatch=0]
          DT2040<-funion(SICK_DT20,SICK_DT40)
          
          return(DT2040)
        }
        DIV_CD<-function(DT30,DT50,CODE){
          setkey(DT30,DIV_CD)
          setkey(DT50,DIV_CD)
          setkey(CODE,V1)
          
          DIV_CD_30<-DT30[CODE,.(JID,MID,DIV_DATE=RECU_FR_DD,DIV_CD),nomatch=0]
          DIV_CD_50<-DT50[CODE,.(JID,MID,DIV_DATE=RECU_FR_DD,DIV_CD),nomatch=0]
          
          DIV_CD_3050<-funion(DIV_CD_30,DIV_CD_50)
          return(DIV_CD_3050)
        }
        DIV_CD_2<-function(DT30,DT50,CODE){
          setkey(DT30,DIV_CD)
          setkey(DT50,DIV_CD)
          setkey(CODE,V1)
          
          DIV_CD_30<-DT30[CODE,.(JID,MID,DIV_DATE=RECU_FR_DD,DIV_CD,TOT_USE_QTY_OR_EXEC_FQ),nomatch=0]
          DIV_CD_50<-DT50[CODE,.(JID,MID,DIV_DATE=RECU_FR_DD,DIV_CD,TOT_USE_QTY_OR_EXEC_FQ),nomatch=0]
          
          DIV_CD_3050<-funion(DIV_CD_30,DIV_CD_50)
          return(DIV_CD_3050)
        }
        GNL_CD<-function(DT30,DT50,CODE){
          setkey(DT30,GNL_CD)
          setkey(DT50,GNL_CD)
          setkey(CODE,V1)
          
          GNL_CD_30<-DT30[CODE,.(JID,MID,GNL_DATE=RECU_FR_DD,GNL_CD),nomatch=0]
          GNL_CD_50<-DT50[CODE,.(JID,MID,GNL_DATE=RECU_FR_DD,GNL_CD),nomatch=0]
          
          GNL_CD_3050<-funion(GNL_CD_30,GNL_CD_50)
          return(GNL_CD_3050)
        }
        GNL_CD_2<-function(DT30,DT50,CODE){
          setkey(DT30,GNL_CD)
          setkey(DT50,GNL_CD)
          setkey(CODE,V1)
          
          GNL_CD_30<-DT30[CODE,.(JID,MID,GNL_DATE=RECU_FR_DD,GNL_CD,TOT_USE_QTY_OR_EXEC_FQ),nomatch=0]
          GNL_CD_50<-DT50[CODE,.(JID,MID,GNL_DATE=RECU_FR_DD,GNL_CD,TOT_USE_QTY_OR_EXEC_FQ),nomatch=0]
          
          GNL_CD_3050<-funion(GNL_CD_30,GNL_CD_50)
          return(GNL_CD_3050)
        }
        loadTable <- function(prefix, fileName) {
          fileName = paste(prefix, fileName, sep = "")
          cat(paste("\nLoading :", fileName, " ..."))
          dt <- fread(fileName, nrows = -1L, header = "auto")
          
          cat(paste("\n  :", fileName, "=> Finished !\n"))
          return(dt)
        }
        saveTable <- function(path,dt,fileName) {
          fileName = paste0(path, fileName)
          fwrite(dt, fileName,  quote = "auto", na = "", dec = ".",
                 col.names = TRUE, buffMB = 1024L, nThread = 7)
          
          cat(paste("\nSave CSV : ",fileName, "=> Done !\n"))
        }
        matching_DATE<-function(match_table,m.out){
          match_table<-as.data.frame(match_table)
          row.names(match_table)<-c(match_table$JID)
          
          JID_Dx1<-row.names(m.out$match.matrix)
          JID_matched<-cbind(JID_Dx1,m.out$match.matrix)
          
          colnames(JID_matched)<-c("Dx1","Dx0_1","Dx0_2")
          JID_matched<-as.data.table(JID_matched)
          match_table<-as.data.table(match_table)
          
          setkey(JID_matched,Dx1)
          setkey(match_table,JID)
          match_table[,JID:=as.character(JID)]
          match_Dx<-JID_matched[match_table,.(JID,Dx0_1,Dx0_2,Tx_DATE),nomatch=0]
          
          Dx0_1_date<-match_Dx[,.(Dx0_1,Tx_DATE)]
          Dx0_2_date<-match_Dx[,.(Dx0_2,Tx_DATE)]
          
          setkey(Dx0_1_date,Dx0_1)
          a<-Dx0_1_date[match_table,.(JID,Tx_DATE,i.Tx_DATE)]
          setnames(a,"i.Tx_DATE","Tx_DATE_01")
          
          setkey(a,JID)
          setkey(Dx0_2_date,Dx0_2)
          a<-Dx0_2_date[a,.(JID,Tx_DATE,Tx_DATE_01,i.Tx_DATE)] 
          a[is.na(a)]<-0
          a[Tx_DATE==0,Tx_DATE:=i.Tx_DATE]
          a[Tx_DATE==0,Tx_DATE:=Tx_DATE_01]
          
          a[,i.Tx_DATE:=NULL]
          a[,Tx_DATE_01:=NULL]
          
          return(a)
        }
        # ##############################################################################
        # DT20<-loadTable(basepath,"DT20_all.csv")
        # DT20_s<-DT20[,.(JID,MID,SEX_TP_CD,PAT_AGE,PAT_STC_AGE,FOM_TP_CD,MAIN_SICK,SUB_SICK,RECU_FR_DD,PRCL_SYM_TP_CD,INSUP_TP_CD,RVD_PLC_CD)]
        # DT40<-loadTable(basepath,"DT40_all.csv")
        # 
        # rm(DT20)
        # gc(reset=T)
        # 
        # P_DIA_t30_all<-loadTable(savepath,"P_DIA_t30_all_20190712.csv")
        
        DT20_s<-loadTable(savepath,"DT20_s_20200106.csv")
        
        
        # ##############################################################################
        # ### load basetable
        ##############################################################################
        
        # DT20_ID<-DT20[,.(JID,MID,RECU_FR_DD)]
        # saveTable(basepath,DT20_ID,"DT20_ID.csv")
        # ##############################################################################
        # 1) dialysis patient
        ################################################################################
        ### HD
        # P_HD_V<-DT20[grepl("V001",PRCL_SYM_TP_CD)]
        # P_HD_V<-P_HD_V[,.(JID,DIA_DATE=RECU_FR_DD)]
        # P_HD_V<-unique(P_HD_V)
        # ----------------------------------------------------
        # saveTable(savepath,P_HD_V,"P_HD_V.csv")
        # P_HD_V<-loadTable(savepath,"P_HD_V.csv") 
        # ----------------------------------------------------
        # # HD code O7020, O7021
        # P_DIA_HD_1<-P_DIA_t30_all[grepl("^O7020",DIV_CD)]
        # P_DIA_HD_2<-P_DIA_t30_all[grepl("^O7021",DIV_CD)]
        # P_HD_DIV<-rbind(P_DIA_HD_1,P_DIA_HD_2)
        # P_HD_DIV<-P_HD_DIV[,.(JID,DIA_DATE=RECU_FR_DD)]
        # P_HD_DIV<-unique(P_HD_DIV)
        # # -------------------------------------------------------
        # saveTable(savepath,P_HD_DIV,"P_HD_DIV.csv")
        # P_HD_DIV<-loadTable(savepath,"P_HD_DIV.csv")
        ####################################################################################
        # unique(P_HD_DIV,by="JID") #142820
        # unique(P_HD_V,by="JID") #178603
        #  
        # P_HD_all<-rbind(P_HD_DIV,P_HD_V)
        # P_HD_all<-unique(P_HD_all)
        # ------------------------------------------------------------
        # saveTable(savepath,P_HD_all,"P_HD_all_20190808.csv")
        # P_HD_all<-loadTable(savepath,"P_HD_all_20190808.csv")
        # unique(P_HD_all,by="JID") #181115
        ####################################################################################
        # 90일 이내 12회
        # P_HD_all[,DIA_DATE:=ymd(DIA_DATE)]
        # P_HD_all[,start:=ymd(DIA_DATE)]
        # P_HD_all[,end:=ymd(DIA_DATE)+90]
        # P_HD_all[,n:=.SD[.SD,on=.(JID,DIA_DATE>=start,DIA_DATE<=end),.N,by=.EACHI]$N]
        
        # P_HD_all_12<-P_HD_all[n>=12]
        # saveTable(savepath,P_HD_all_12,"P_HD_all_12.csv")
        # P_HD_all_12<-loadTable(savepath,"P_HD_all_12.csv")
        
        # unique(P_HD_all_12,by="JID") #102976
        ##############################################################################
        # ### PD
        # P_PD_V<-DT20[grepl("V003",PRCL_SYM_TP_CD)] #34102
        # P_PD_V<-P_PD_V[,.(JID,DIA_DATE=RECU_FR_DD)]
        # P_PD_V<-unique(P_PD_V)
        # -----------------------------------------------------------------------
        # saveTable(savepath,P_PD_V,"P_PD_V.csv")
        # P_PD_V<-loadTable(savepath,"P_PD_V.csv")
        ##########################################################################
        # # PD code O7061,O7062,O7071,O7072,O7073,O7074,O7075
        # 
        # P_DIA_PD_1<-P_DIA_t30_all[grepl("^O7061",DIV_CD)]
        # P_DIA_PD_2<-P_DIA_t30_all[grepl("^O7062",DIV_CD)]
        # P_DIA_PD_3<-P_DIA_t30_all[grepl("^O7071",DIV_CD)]
        # P_DIA_PD_4<-P_DIA_t30_all[grepl("^O7072",DIV_CD)]
        # P_DIA_PD_5<-P_DIA_t30_all[grepl("^O7073",DIV_CD)]
        # P_DIA_PD_6<-P_DIA_t30_all[grepl("^O7074",DIV_CD)]
        # P_DIA_PD_7<-P_DIA_t30_all[grepl("^O7075",DIV_CD)]
        # 
        # P_PD_DIV<-rbind(P_DIA_PD_1,P_DIA_PD_2,P_DIA_PD_3,
        #                 P_DIA_PD_4,P_DIA_PD_5,P_DIA_PD_6,P_DIA_PD_7)
        # 
        # P_PD_DIV<-P_PD_DIV[,.(JID,DIA_DATE=RECU_FR_DD)]
        # P_PD_DIV<-unique(P_PD_DIV)
        # -----------------------------------------------------------------------
        # saveTable(savepath,P_PD_DIV,"P_PD_DIV.csv")
        # P_PD_DIV<-loadTable(savepath,"P_PD_DIV.csv")
        # unique(P_PD_DIV,by="JID") #23917
        ####################################################################
        # code_PD
        # dt30_pd_med<-read_sas("~/sas_table/dt30_pd_med.sas7bdat")
        # dt50_pd_med<-read_sas("~/sas_table/dt50_pd_med.sas7bdat")
        # dt30_pd_med<-as.data.table(dt30_pd_med)
        # dt50_pd_med<-as.data.table(dt50_pd_med)
        # dt30_pd_med<-dt30_pd_med[,.(MID)]
        # dt50_pd_med<-dt50_pd_med[,.(MID)]
        # 
        # P_PD_GNL<-rbind(dt30_pd_med,dt50_pd_med)
        # P_PD_GNL<-unique(P_PD_GNL)
        # 
        # DT20_ID[,MID:=as.character(MID)]
        # P_PD_GNL[,MID:=as.character(MID)]
        # 
        # setkey(DT20_ID,MID)
        # setkey(P_PD_GNL,MID)
        # P_PD_GNL<-DT20_ID[P_PD_GNL]
        # P_PD_GNL<-unique(P_PD_GNL[,.(JID,DIA_DATE=RECU_FR_DD)])
        # --------------------------------------------------------------------
        # saveTable(savepath,P_PD_GNL,"P_PD_GNL.csv")
        # P_PD_GNL<-loadTable(savepath,"P_PD_GNL.csv")
        
        # unique(P_PD_GNL,by="JID") #23420
        #####################################################################
        # P_PD<-rbind(P_PD_DIV,P_PD_V,P_PD_GNL)
        # P_PD_all<-unique(P_PD)
        # P_PD_all<-P_PD_all[order(JID,DIA_DATE)]
        # -----------------------------------------------------------------
        # saveTable(savepath,P_PD_all,"P_PD_all_20190808.csv")
        # P_PD_all<-loadTable(savepath,"P_PD_all_20190808.csv")
        # unique(P_PD_all,by="JID") #35204
        #####################################################################
        # P_PD_all[,first_day:=.SD[which.min(DIA_DATE)],by="JID"]
        # P_PD_all[,last_day:=.SD[which.max(DIA_DATE)],by="JID"]
        # P_PD_all<-P_PD_all[,day:=ymd(last_day)-ymd(first_day)] #3개월이상 투석
        # P_PD_all<-P_PD_all[day>=90]
        # unique(P_PD_all,by="JID") #24702
        # #####################################################################
        # P_HD_all_12[,start:=NULL]
        # P_HD_all_12[,end:=NULL]
        # P_HD_all_12[,n:=NULL]
        # 
        # P_HD_all_12[,type:="HD"]
        # P_PD_all[,type:="PD"]
        # 
        # P_PD_all<-P_PD_all[,.(JID,DIA_DATE,type)]
        # 
        # P_PD_all[,DIA_DATE:=ymd(DIA_DATE)]
        # P_HD_all_12[,DIA_DATE:=ymd(DIA_DATE)]
        ###################################################################
        # P_PD_all_s<-P_PD_all[,.SD[which.min(DIA_DATE)],by=c("JID")]
        # P_HD_all_12_s<-P_HD_all_12[,.SD[which.min(DIA_DATE)],by=c("JID")]
        ####################################################################
        # P_DIA_type<-rbind(P_PD_all_s,P_HD_all_12_s)
        # P_DIA_type<-unique(P_DIA_type)
        # P_DIA_type<-P_DIA_type[order(JID,DIA_DATE)]
        # 
        # P_DIA_type[,N:=.N,by="JID"]
        # P_DIA_type_2N<-P_DIA_type[N==2]
        # P_DIA_type_2N
        # 
        # P_DIA_type_2N[,day:=DIA_DATE-shift(DIA_DATE),by="JID"]
        # 
        # P_DIA_same_ID<-P_DIA_type_2N[day==0][,.(JID)]
        # 
        # setkey(P_DIA_same_ID,JID)
        # setkey(P_DIA_type,JID)
        # 
        # a<-P_DIA_type[P_DIA_same_ID]
        # P_PD_GNL_s<-P_PD_GNL[,.SD[which.min(DIA_DATE)],by="JID"]
        # setkey(P_PD_GNL_s,JID)
        # setkey(a,JID)
        # aa<-P_PD_GNL_s[a][ymd(DIA_DATE)==ymd(i.DIA_DATE)]
        # aaa<-unique(aa[,.(JID)])
        # both_PD<-aaa
        # 
        # both_PD[,both:=1]
        # 
        # P_DIA_type<-P_DIA_type[,.SD[which.min(DIA_DATE)],by="JID"]
        # 
        # setkey(P_DIA_type,JID)
        # setkey(both_PD,JID)
        # 
        # P_DIA_type_both<-both_PD[P_DIA_type]
        # 
        # P_DIA_type_both[both==1,type:="HD"]
        # P_DIA_type_both
        # 
        # P_DIA_type<-P_DIA_type[,.(JID,type)]
        # P_DIA_type<-unique(P_DIA_type)
        # 
        # table(P_DIA_type$type)
        # ------------------------------------------------------------------------------
        # saveTable(savepath,P_DIA_type,"P_DIA_type_20190924.csv")
        # P_DIA_type<-loadTable(savepath,"P_DIA_type_20190924.csv")
        ######################################################################
        # P_DIA<-rbind(P_PD_all,P_HD_all_12)
        # 
        # P_DIA[,first_day:=.SD[which.min(DIA_DATE)],by="JID"]
        # P_DIA[,last_day:=.SD[which.max(DIA_DATE)],by="JID"]
        # P_DIA<-P_DIA[,day:=ymd(last_day)-ymd(first_day)] #3개월 이상 투석
        # P_DIA[,year:=year(DIA_DATE)]
        # P_DIA_90d<-P_DIA[day>=90]
        # P_DIA_90d[,year:=year(DIA_DATE)]
        # unique(P_DIA_90d,by="JID")
        # 
        # saveTable(savepath,P_DIA_90d,"P_DIA_90d.csv")
        # P_DIA_90d<-loadTable(savepath,"P_DIA_90d.csv")
        # P_DIA_incidence<-P_DIA_90d[,.SD[which.min(ymd(DIA_DATE))],by="JID"]
        # unique(P_DIA_90d,by="JID") #111140
        # ###########################################################################
        # P_DIA_incidence<-P_DIA_90d[,.SD[which.min(DIA_DATE)],by="JID"]
        # # saveTable(savepath,P_DIA_incidence,"P_DIA_incidence.csv")
        # P_DIA_incidence_1016<-P_DIA_incidence_1016[,.(JID,DIA_DATE,year)]
        # 
        # saveTable(savepath,P_DIA_incidence_1016,"P_DIA_incidence_1016_20190813.csv") #67914
        P_DIA_incidence_1016<-loadTable(statinpath,"P_DIA_incidence_1016_20190813.csv")
        P_DIA_incidence_1016
        ##############################################################################
        # unique(DT20,by="JID") #198419
        # P_JK<-DT20[,.(JID,SEX=SEX_TP_CD,AGE=PAT_STC_AGE,year=RECU_FR_DD%/%10000)]
        # P_JK<-unique(P_JK)
        # 
        # P_JK<-P_JK[AGE!=111]
        # P_JK<-P_JK[AGE!=110]
        # 
        # P_JK[AGE==0.9,AGE:=1]
        # P_JK[AGE==0.2,AGE:=1]
        # 
        # P_JK<-unique(P_JK)
        # 
        # P_JK[,N2:=.N,by=c("JID")]
        # P_JK[N!=N2]
        # 
        # P_JK[JID==14773][year==2013]$AGE<-48
        # P_JK[JID==14773]$SEX<-2
        # P_JK[JID==18779][year==2013]$AGE<-56
        # P_JK[JID==49624][year==2013]$AGE<-32
        # P_JK[JID==49624]$SEX<-2
        # P_JK[JID==58604][year==2013]$AGE<-50
        # P_JK[JID==58604]$SEX<-2
        # P_JK[JID==65594][year==2013]$AGE<-34
        # P_JK[JID==89651][year==2013]$AGE<-38
        # P_JK[JID==190549][year==2013]$AGE<-18
        # P_JK[JID==190549]$SEX<-2
        # 
        # P_JK[,N:=NULL]
        # P_JK[,N2:=NULL]
        # P_JK<-unique(P_JK)
        # 
        # # unique(P_JK,by="JID") #198419
        #  
        # saveTable(P_JK,"P_JK.csv")
        # P_JK<-loadTable(statinpath,"P_JK.csv")
        # ###############################################################################
        # setkey(P_DIA_incidence_1016,JID,year)
        # setkey(P_JK,JID,year)
        # 
        # P_DIA_JK<-P_JK[P_DIA_incidence_1016]
        
        # P_DIA_JK_18<-P_DIA_JK[AGE<19]
        # P_DIA_JK_18<-P_DIA_JK[AGE>=19]
        # unique(P_DIA_JK_18,by="JID") #67618
        # # ----------------------------------------------------------------------------
        # saveTable(savepath,P_DIA_JK_18,"P_DIA_JK_18_20190813.csv") #67618
        P_DIA_JK_18<-loadTable(statinpath,"P_DIA_JK_18_20190813.csv")
        ################################################################################
        # get HTN patient
        # dt30_htn_med_1<-as.data.table(read_sas("~/sas_table/dt30_htn_med_1.sas7bdat"))
        # dt30_htn_med_2<-as.data.table(read_sas("~/sas_table/dt30_htn_med_2.sas7bdat"))
        # dt50_htn_med<-as.data.table(read_sas("~/sas_table/dt50_htn_med.sas7bdat"))
        # 
        # dt30_htn_med_1<-dt30_htn_med_1[,.(MID)]
        # dt30_htn_med_2<-dt30_htn_med_2[,.(MID)]
        # dt50_htn_med<-dt50_htn_med[,.(MID)]
        # 
        # HTN_MED<-rbind(dt30_htn_med_1,dt30_htn_med_2,dt50_htn_med)
        # HTN_MED<-unique(HTN_MED)
        # 
        # DT20_s[,MID:=as.character(MID)]
        # HTN_MED[,MID:=as.character(MID)]
        # 
        # setkey(HTN_MED,MID)
        # setkey(DT20_s,MID)
        # 
        # P_HTN_MED<-DT20_s[HTN_MED,.(JID,RECU_FR_DD)]
        # 
        # P_HTN_MED<-P_HTN_MED[order(JID,RECU_FR_DD)]
        # P_HTN_MED<-unique(P_HTN_MED)
        # 
        # unique(P_HTN_MED,by="JID") #181504
        # colnames(P_HTN_MED)<-c("JID","HTN_DATE")
        # 
        # saveTable(savepath,P_HTN_MED,"P_HTN_MED.csv")
        # P_HTN_MED<-loadTable(basepath,"P_HTN_MED.csv")
        # 
        # setkey(P_DIA_incidence_1016,JID)
        # setkey(P_HTN_MED,JID)
        # 
        # P_DIA_HTN_MED<-P_HTN_MED[P_DIA_incidence_1016,nomatch=0]
        # P_DIA_HTN_MED[,day:=ymd(DIA_DATE)-ymd(HTN_DATE)]
        # P_DIA_HTN_MED
        # 
        # P_DIA_HTN_MED_N<-P_DIA_HTN_MED[0<day&day<=365*3]
        # P_DIA_HTN_MED_N
        # 
        # P_DIA_HTN_MED_2N<-P_DIA_HTN_MED_N[,N:=.N,by=JID][N>=2] #투석전 3년이내 투약2회이상
        # unique(P_DIA_HTN_MED_2N,by="JID") #62224
        # ------------------------------------------------------------------------------
        # sickcode_HTN<-loadTable(sickpath,"sickcode_HTN.csv")
        # 
        # P_HTN_SICK<-SICK_CD2(DT20_s,sickcode_HTN)
        # P_HTN_SICK<-unique(P_HTN_SICK[,.(JID,Dx_DATE)])
        # 
        # colnames(P_HTN_SICK)<-c("JID","HTN_DATE")
        # saveTable(savepath,P_HTN_SICK,"P_HTN_SICK_20190930.csv")
        # P_HTN_SICK<-loadTable(savepath,"P_HTN_SICK_20190930.csv")
        # 
        # setkey(P_DIA_incidence_1016,JID)
        # setkey(P_HTN_SICK,JID)
        # 
        # P_DIA_HTN_SICK<-P_HTN_SICK[P_DIA_incidence_1016,nomatch=0]
        # P_DIA_HTN_SICK[,day:=ymd(DIA_DATE)-ymd(HTN_DATE)]
        # 
        # P_DIA_HTN_SICK_N<-P_DIA_HTN_SICK[0<day&day<=365*3]
        # P_DIA_HTN_SICK_2N<-P_DIA_HTN_SICK_N[,N:=.N,by=JID][N>=2] #투석전 3년이내 상병2회이상
        # 
        # unique(P_DIA_HTN_SICK_2N,by="JID") #53992
        # ------------------------------------------------------------------------------
        # ##############################################################################
        # P_HTN_all<-rbind(P_DIA_HTN_SICK_2N,P_DIA_HTN_MED_2N)
        # P_HTN_all<-P_HTN_all[,.SD[which.min(HTN_DATE)],by="JID"]
        # P_HTN_all<-P_HTN_all[,.(JID,HTN_DATE,DIA_DATE)]
        # P_HTN_all[,HTN:=1]
        # P_HTN_all
        # 
        # saveTable(savepath,P_HTN_all,"P_HTN_all_20190930.csv")
        P_HTN_all<-loadTable(savepath,"P_HTN_all_20190930.csv")
        # ##############################################################################
        P_I48<-DT20_s[grepl("I48",paste0(MAIN_SICK,SUB_SICK))]
        saveTable(savepath,P_I48,"P_I48.csv")
        P_I48<-loadTable(savepath,"P_I48.csv")
        
        P_I48<-P_I48[,.(JID,Afib_DATE=RECU_FR_DD)]
        
        # ##############################################################################
        # get DM patient
        # dt30_DM_med<-as.data.table(read_sas("~/sas_table/dt30_dm_med.sas7bdat"))
        # dt50_DM_med<-as.data.table(read_sas("~/sas_table/dt50_dm_med.sas7bdat"))
        # 
        # dt30_DM_med<-dt30_DM_med[,.(MID)]
        # dt50_DM_med<-dt50_DM_med[,.(MID)]
        # 
        # DM_MED<-rbind(dt30_DM_med,dt50_DM_med)
        # DM_MED<-unique(DM_MED)
        # 
        # DT20_s[,MID:=as.character(MID)]
        # DM_MED[,MID:=as.character(MID)]
        # 
        # setkey(DM_MED,MID)
        # setkey(DT20_s,MID)
        # 
        # P_DM_MED<-DT20_s[DM_MED,.(JID,RECU_FR_DD)]
        # 
        # P_DM_MED<-P_DM_MED[order(JID,RECU_FR_DD)]
        # P_DM_MED<-unique(P_DM_MED)
        # 
        # colnames(P_DM_MED)<-c("JID","DM_DATE")
        # 
        # saveTable(savepath,P_DM_MED,"P_DM_MED_20190930.csv")
        # P_DM_MED<-loadTable(savepath,"P_DM_MED.csv")
        # 
        # setkey(P_DIA_incidence_1016,JID)
        # setkey(P_DM_MED,JID)
        # 
        # P_DIA_DM_MED<-P_DM_MED[P_DIA_incidence_1016,nomatch=0]
        # P_DIA_DM_MED[,day:=ymd(DIA_DATE)-ymd(DM_DATE)]
        # 
        # P_DIA_DM_MED_N<-P_DIA_DM_MED[0<day&day<=365*3]
        # 
        # P_DIA_DM_MED_2N<-P_DIA_DM_MED_N[,N:=.N,by=JID][N>=2] #투석전 3년이내 투약2회이상
        # 
        # unique(P_DIA_DM_MED_2N,by="JID") #37936
        # 
        # # ------------------------------------------------------------------------------
        # sickcode_DM<-loadTable(sickpath,"sickcode_DM.csv")
        # P_DM_SICK<-SICK_CD2(DT20_s,sickcode_DM)
        # P_DM_SICK<-unique(P_DM_SICK[,.(JID,Dx_DATE)])
        # 
        # colnames(P_DM_SICK)<-c("JID","DM_DATE")
        # saveTable(savepath,P_DM_SICK,"P_DM_SICK.csv")
        # P_DM_SICK<-loadTable(savepath,"P_DM_SICK.csv")
        # 
        # setkey(P_DIA_incidence_1016,JID)
        # setkey(P_DM_SICK,JID)
        # 
        # P_DIA_DM_SICK<-P_DM_SICK[P_DIA_incidence_1016,nomatch=0]
        # P_DIA_DM_SICK[,day:=ymd(DIA_DATE)-ymd(DM_DATE)]
        # P_DIA_DM_SICK_N<-P_DIA_DM_SICK[0<day&day<=365*3]
        # P_DIA_DM_SICK_2N<-P_DIA_DM_SICK_N[,N:=.N,by=JID][N>=2] #투석전 3년이내 상병2회이상
        # # ------------------------------------------------------------------------------
        # 
        # P_DM_all<-rbind(P_DIA_DM_SICK_2N,P_DIA_DM_MED_2N)
        # P_DM_all<-P_DM_all[,.SD[which.min(DM_DATE)],by="JID"]
        # P_DM_all<-P_DM_all[,.(JID,DM_DATE,DIA_DATE)]
        # P_DM_all[,DM:=1]
        # saveTable(savepath,P_DM_all,"P_DM_all_20190930.csv")
        P_DM_all<-loadTable(savepath,"P_DM_all_20190930.csv")
        # ##############################################################################
        
        # ##############################################################################
        # P_DIA_t30_all_date<-P_DIA_t30_all[,.(JID,RECU_FR_DD)]
        # DT20_death_date<-DT20[,.(JID,RECU_FR_DD)]
        # death_date<-rbind(P_DIA_t30_all_date,DT20_death_date)
        # 
        # death_date<-unique(death_date)
        # death_date_s<-death_date[,.SD[which.max(RECU_FR_DD)],by="JID"]
        # 
        # death_date_s[,day:=ymd(20171231)-ymd(RECU_FR_DD)]
        # 
        # death_date_s[day>=365,DTH_DATE:=RECU_FR_DD]
        # death_date_s[is.na(death_date_s)]<-20171231
        # 
        # P_death<-death_date_s[,.(JID,DTH_DATE)]
        # -----------------------------------------------------------------------------
        # saveTable(savepath,P_death,"P_death.csv")
        P_death<-loadTable(statinpath,"P_death.csv")
        ###############################################################################
        
        ################################################################################
        # get mace patient
        ################################################################################
        # B. MACE 1 입원진단(*FOM_TP_CD:21) +I21 or I22
        # code_MACE1_1<-unique(c(DT20[grepl("I21",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I21",SUB_SICK)]$SUB_SICK))
        # code_MACE1_2<-unique(c(DT20[grepl("I22",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I22",SUB_SICK)]$SUB_SICK))
        # code_MACE1_3<-unique(c(DT20[grepl("I23",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I23",SUB_SICK)]$SUB_SICK))
        # code_MACE1_4<-unique(c(DT20[grepl("I24",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I24",SUB_SICK)]$SUB_SICK))
        # code_MACE1<-as.data.table(c(code_MACE1_1,code_MACE1_2,code_MACE1_3,code_MACE1_4))
        # saveTable(savepath,code_MACE1,"code_MACE1.csv")
        # code_MACE1<-loadTable(savepath,"code_MACE1.csv")
        # P_MACE1<-SICK_CD(DT20_s,DT40,code_MACE1)
        # saveTable(savepath,P_MACE1,"P_MACE1.csv")
        # P_MACE1<-loadTable(savepath,"P_MACE1.csv")
        # P_MACE1<-P_MACE1[order(JID,Dx_DATE)]
        ############################################################################################
        # P_MACE2<-read_sas("~/sas_table/dt30_mace2.sas7bdat")
        # P_MACE2<-as.data.table(P_MACE2)
        # DT20_ID<-DT20_s[,.(JID,MID,RECU_FR_DD)]
        # DT20_ID[,MID:=as.character(MID)]
        # setkey(DT20_ID,MID)
        # setkey(P_MACE2,MID)
        # P_MACE2<-DT20_ID[P_MACE2,.(JID,MID,Dx_CODE=DIV_CD,Dx_DATE=RECU_FR_DD)]
        # saveTable(savepath,P_MACE2,"P_MACE2.csv")
        # P_MACE2<-loadTable(savepath,"P_MACE2.csv")
        # P_MACE2<-P_MACE2[order(JID,Dx_DATE)]
        ############################################################################################
        # code_MACE3_1<-unique(c(DT20[grepl("I60",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I60",SUB_SICK)]$SUB_SICK))
        # code_MACE3_2<-unique(c(DT20[grepl("I61",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I61",SUB_SICK)]$SUB_SICK))
        # code_MACE3_3<-unique(c(DT20[grepl("I62",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I62",SUB_SICK)]$SUB_SICK))
        # code_MACE3_4<-unique(c(DT20[grepl("I63",MAIN_SICK)]$MAIN_SICK,DT20[grepl("I63",SUB_SICK)]$SUB_SICK))
        # code_MACE3<-as.data.table(c(code_MACE3_1,code_MACE3_2,code_MACE3_3,code_MACE3_4))
        # saveTable(savepath,code_MACE3,"code_MACE3.csv")
        # code_MACE3<-loadTable(savepath,"code_MACE3.csv")
        # P_MACE3<-SICK_CD(DT20_s,DT40,code_MACE3)
        # saveTable(savepath,P_MACE3,"P_MACE3.csv")
        # P_MACE3<-loadTable(savepath,"P_MACE3.csv")
        # P_MACE3<-P_MACE3[order(JID,Dx_DATE)]
        ############################################################################################
        # 투석후 1년까지 이전 MACE 제외
        # P_MACE1[,type:="1"]
        # P_MACE2[,type:="2"]
        # P_MACE3[,type:="3"]
        # 
        # P_MACE_all<-rbind(P_MACE1,P_MACE2,P_MACE3)
        # P_MACE_all<-unique(P_MACE_all)
        # 
        # DT20_FOM<-DT20_s[,.(MID,FOM_TP_CD)]
        # DT20_FOM[,MID:=as.character(MID)]
        # DT20_FOM<-DT20_FOM[order(MID)]
        # P_MACE_all[,MID:=as.character(MID)]
        # setkey(DT20_FOM,MID)
        # setkey(P_MACE_all,MID)
        # P_MACE_FOM<-DT20_FOM[P_MACE_all] #mace with fom
        # --------------------------------------------------------------------------------------------
        # saveTable(savepath,P_MACE_FOM,"P_MACE_FOM.csv")
        P_MACE_FOM<-loadTable(savepath,"P_MACE_FOM.csv")
        
        
        # -------------------------------------------------------------------------------------------
        P_DIA_JK_18[,aft_1y:=ymd(DIA_DATE)+365]
        
        setkey(P_MACE_FOM,JID)
        setkey(P_DIA_JK_18,JID)
        
        P_MACE_bf_DIA<-P_MACE_FOM[P_DIA_JK_18,nomatch=0]
        P_MACE_bf_DIA<-P_MACE_bf_DIA[ymd(Dx_DATE)<=ymd(aft_1y)]
        P_MACE_bf_DIA<-unique(P_MACE_bf_DIA)
        
        setcolorder(P_MACE_bf_DIA,c(3,1,2,4,5,6,7,8,9))
        
        P_MACE_bf_DIA[,N:=.N,by="JID"]
        unique(P_MACE_bf_DIA,by="JID")
        
        P_MACE_all_2N<-P_MACE_bf_DIA[N>=2] #2번이상 진단
        P_MACE_all_21<-P_MACE_bf_DIA[N==1&FOM_TP_CD==21] #2754 #Dx+입원력
        
        unique(P_MACE_all_2N,by="JID") #14334
        
        #왜 exclusion 숫자가 더적어? 
        # MACE가 1번이었는데 2번이상으로 바껴서그렇다~
        
        P_MACE_exclusion<-rbind(P_MACE_all_2N[,.(JID)],P_MACE_all_21[,.(JID)])
        P_MACE_exclusion<-unique(P_MACE_exclusion)
        P_MACE_exclusion #17088
        
        #####################################################################################
        #  exclusion count
        # 
        # setkey(P_DIA_incidence_1016,JID)
        # 
        # P_MACE_bf_DIA<-P_MACE_all[P_DIA_incidence_1016,nomatch=0]
        # P_MACE_bf_DIA<-P_MACE_bf_DIA[ymd(Dx_DATE)<=DIA_DATE]
        # P_MACE_bf_DIA<-unique(P_MACE_bf_DIA)
        # 
        # P_MACE_bf_DIA[,N:=.N,by="JID"]
        # unique(P_MACE_bf_DIA,by="JID")
        # 
        # P_MACE_all_2N<-P_MACE_bf_DIA[N>=2] #2번이상 진단
        # P_MACE_all_21<-P_MACE_bf_DIA[FOM_TP_CD==21] #1145046 #입원력
        # 
        # P_MACE_exclusion<-as.data.table(P_MACE_all_2N$JID,P_MACE_all_21$JID)
        # P_MACE_exclusion<-unique(P_MACE_exclusion)
        # P_MACE_exclusion #8406
        # 
        ####################################################################################
        # P_MACE_exclusion<-P_MACE_exclusion[order(JID)]
        # P_MACE_exclusion #17088
        #-----------------------------------------------------------------------
        # setkey(P_MACE_exclusion,JID)
        # setkey(P_DIA_JK_18,JID)
        # P_DIA_nonMACE<-P_DIA_JK_18[!P_MACE_exclusion]
        # 
        # unique(P_DIA_nonMACE,by="JID") #50530
        # ----------------------------------------------------------------------------
        # saveTable(savepath,P_DIA_nonMACE,"P_DIA_nonMACE_20190910.csv")
        P_DIA_nonMACE<-loadTable(statinpath,"P_DIA_nonMACE_20190910.csv") #투석이전에 mace발생환자 제외
        
        ###################################################################################
        # statin
        # dt30_statin<-read_sas("~/sas_table/dt30_statin_2.sas7bdat")
        # dt50_statin<-read_sas("~/sas_table/dt50_statin_2.sas7bdat")
        # dt30_statin<-as.data.table(dt30_statin)
        # dt50_statin<-as.data.table(dt50_statin)
        # 
        # dt30_statin<-dt30_statin[,.(MID,TOT_USE=TOT_USE_QTY_OR_EXEC_FQ,GNL_CD)]
        # dt50_statin<-dt50_statin[,.(MID,TOT_USE=TOT_USE_QTY_OR_EXEC_FQ,GNL_CD)]
        # dt30_statin<-unique(dt30_statin) #5612510
        # dt50_statin<-unique(dt50_statin) #2847039
        # dt_statin<-rbind(dt30_statin,dt50_statin)
        # dt_statin<-unique(dt_statin)
        # 
        # DT20_ID[,MID:=as.character(MID)]
        # 
        # setkey(dt_statin,MID)
        # setkey(DT20_ID,MID)
        # 
        # dt_statin<-DT20_ID[dt_statin]
        # dt_statin<-unique(dt_statin)
        # 
        # dt_statin_s<-dt_statin[,.(JID,Tx_DATE=RECU_FR_DD,TOT_USE,GNL_CD)]
        # dt_statin_s<-dt_statin_s[order(JID,Tx_DATE)]
        # 
        # dt_statin_s[,total_Tx:=sum(TOT_USE),by=c("JID","Tx_DATE")]
        # 
        # dt_statin_s[,N:=.N,by=c("JID","Tx_DATE")]
        # dt_statin_s[N!=1]
        # 
        # dt_statin_s<-dt_statin_s[,.(JID,Tx_DATE,total_Tx)]
        # 
        # dt_statin_s<-unique(dt_statin_s)
        # -----------------------------------------------------------------------
        # saveTable(savepath,dt_statin_s,"dt_statin_s.csv")
        dt_statin_s<-loadTable(statinpath,"dt_statin_s.csv")
        unique(dt_statin_s,by="JID") #132355
        
        #################################################################################
        setkey(P_DIA_nonMACE,JID)
        setkey(dt_statin_s,JID)
        
        P_DIA_statin<-dt_statin_s[P_DIA_nonMACE]
        P_DIA_statin[is.na(P_DIA_statin)]<-20200202
        
        P_DIA_statin_s<-P_DIA_statin[,.SD[which.min(Tx_DATE)],by="JID"]
        P_DIA_statin_ss<-P_DIA_statin_s[ymd(DIA_DATE)>=ymd(Tx_DATE)] #투석 이전 한번이라도 statin이력 #34664
        
        P_statin_bf_DIA_ID<-P_DIA_statin_ss[,.(JID,GNL_CD)]
        
        setkey(P_DIA_statin,JID)
        setkey(P_statin_bf_DIA_ID,JID)
        P_nonstatin_bf_DIA<-P_DIA_statin[!P_statin_bf_DIA_ID]
        
        unique(P_nonstatin_bf_DIA,by="JID") #19950
        unique(P_nonstatin_bf_DIA,by="JID")[Tx_DATE!=20200202] #6070
        unique(P_nonstatin_bf_DIA,by="JID")[Tx_DATE==20200202] #13880
        
        # --------------------------------------------------------------------------------
        saveTable(savepath,P_nonstatin_bf_DIA,"P_nonstatin_bf_DIA_20200113.csv")
        P_nonstatin_bf_DIA<-loadTable(savepath,"P_nonstatin_bf_DIA_20200113.csv")
        
        # ##################################################################################
        # # 투석 이후 1년내 12주이상 7*12=84 statin 처방력이 있는사람
        P_DIA_statin_xxx<-P_nonstatin_bf_DIA[Tx_DATE==20200202]
        P_DIA_statin_ooo<-P_nonstatin_bf_DIA[Tx_DATE!=20200202]
        
        P_DIA_statin_2<-P_DIA_statin_ooo[ymd(DIA_DATE)<ymd(Tx_DATE)&ymd(Tx_DATE)<=ymd(aft_1y)]
        P_DIA_statin_2<-unique(P_DIA_statin_2)
        P_DIA_statin_2[,Tx_84:=sum(total_Tx),by="JID"]
        
        P_DIA_statin_84<-P_DIA_statin_2[Tx_84>=84]
        P_DIA_statin_L84<-P_DIA_statin_2[Tx_84<84]
        
        P_DIA_statin_84_tx_dose<-P_DIA_statin_84[,.(JID,GNL_CD,Tx_84)]
        P_DIA_statin_84_tx_dose<-unique(P_DIA_statin_84_tx_dose)
        P_DIA_statin_84_tx_dose<-P_DIA_statin_84_tx_dose[,.SD[which.max(Tx_84)],by="JID"]
        P_DIA_statin_84_tx_dose
        
        saveTable(savepath,P_DIA_statin_84_tx_dose,"P_DIA_statin_84_tx_dose.csv")
        
        P_statin_Tx<-unique(P_DIA_statin_2[,.(JID,Tx_84)])
        P_statin_Tx
        
        
        # # ---------------------------------------------------------------
        # saveTable(savepath,P_DIA_statin_84,"P_DIA_statin_84_20190910.csv")
        P_DIA_statin_84<-loadTable(savepath,"P_DIA_statin_84_20190910.csv")
        ##################################################################################
        unique(P_DIA_statin_84,by="JID") #1636
        DIA_statin_OOO<-P_DIA_statin_84[,.SD[which.min(Tx_DATE)],by="JID"]
        
        DIA_statin_OOO
        # #######################################################################
        # P_DIA_statin_tx_JID<-P_DIA_statin_84[,.(JID)]
        # P_DIA_statin_tx_JID<-unique(P_DIA_statin_tx_JID)
        # 
        # setkey(P_DIA_statin_tx_JID,JID)
        # setkey(P_DIA_nonstatin,JID)
        # 
        # P_DIA_non_statin<-P_DIA_nonstatin[!P_DIA_statin_tx_JID]
        # 
        # unique(P_DIA_non_statin,by="JID") #18314
        # 
        # P_DIA_non_statin<-P_DIA_non_statin[order(JID,Tx_DATE)]
        # P_DIA_non_statin<-P_DIA_non_statin[,.SD[which.min(Tx_DATE)],by="JID"]
        # P_DIA_non_statin #18314
        # #################################################################
        # saveTable(savepath,P_DIA_non_statin,"P_DIA_non_statin_20190910.csv")
        DIA_statin_XXX<-loadTable(statinpath,"P_DIA_non_statin_20190910.csv")
        
        DIA_statin_XXX
        DIA_statin_XXX[,Tx_DATE:=20200202]
        
        DIA_statin_XXX[,statin:=0]
        DIA_statin_XXX[,total_Tx:=NULL]
        
        DIA_statin_XXX[,aft_1y:=NULL]
        DIA_statin_OOO[,Tx_84:=NULL]
        
        DIA_statin_OOO[,statin:=1]
        
        ######################################################################################
        # MACE
        ######################################################################################
        P_MACE_FOM[,Dx_CODE:=NULL]
        P_MACE_FOM[,MID:=NULL]
        setcolorder(P_MACE_FOM,c(2,3,1))
        P_MACE_FOM<-unique(P_MACE_FOM)
        
        # P_MACE_FOM_type<-P_MACE_FOM[type==3]
        
        P_MACE_dx<-P_MACE_FOM_type[FOM_TP_CD==21] #1회이상진단+입원력
        P_MACE_dx<-P_MACE_dx[order(JID)]
        
        P_MACE_fisrt_onset_all<-P_MACE_dx[,.SD[which.min(Dx_DATE)],by="JID"]
        P_MACE_fisrt_onset_all[,FOM_TP_CD:=NULL]
        
        P_MACE_fisrt_onset_all #mace발생환자선정
        
        ##################################################################################
        # mace환자 붙이기
        setkey(DIA_statin_OOO,JID)
        setkey(P_MACE_fisrt_onset_all,JID)
        DIA_statin_mace<-P_MACE_fisrt_onset_all[DIA_statin_OOO]
        DIA_statin_mace[is.na(DIA_statin_mace)]<-20200202
        
        # dia<tx<mace
        
        DIA_statin_mace<-DIA_statin_mace[DIA_DATE<ymd(Dx_DATE)] #투석이전mace제외
        DIA_statin_mace<-DIA_statin_mace[Tx_DATE<Dx_DATE] #statin처방이전 dx제외
        
        DIA_statin_mace[Dx_DATE!=20200202,mace:=1]
        DIA_statin_mace[is.na(DIA_statin_mace)]<-0
        
        ############
        
        setkey(DIA_statin_XXX,JID)
        setkey(P_MACE_fisrt_onset_all,JID)
        DIA_nonstatin_mace<-P_MACE_fisrt_onset_all[DIA_statin_XXX]
        DIA_nonstatin_mace[is.na(DIA_nonstatin_mace)]<-20200202
        
        DIA_nonstatin_mace<-DIA_nonstatin_mace[DIA_DATE<ymd(Dx_DATE)] #투석이전mace제외
        DIA_nonstatin_mace[Dx_DATE!=20200202,mace:=1]
        DIA_nonstatin_mace[is.na(DIA_nonstatin_mace)]<-0
        
        DIA_statin_mace_total<-rbind(DIA_statin_mace,DIA_nonstatin_mace)
        ######################################################################################
        DIA_statin_mace_total[type==20200202,type:=0]
        
        P_KT<-loadTable(basepath,"P_KT_s.csv")
        P_KT[,KT_ID:=NULL]
        P_KT_s<-P_KT[,.SD[which.min(KT_DATE)],by="JID"]
        
        setkey(DIA_statin_mace_total,JID)
        setkey(P_KT_s,JID)
        
        DIA_statin_mace_KT<-P_KT_s[DIA_statin_mace_total]
        DIA_statin_mace_KT[is.na(DIA_statin_mace_KT)]<-20200202
        
        DIA_statin_mace_KT[KT_DATE!=20200202,KT:=1]
        DIA_statin_mace_KT[is.na(DIA_statin_mace_KT)]<-0
        
        setcolorder(DIA_statin_mace_KT,c(1,5,6,7,8,4,2,3,10,11,9))
        
        DIA_statin_mace_KT[Tx_DATE!=20200202&statin==0]
        
        # saveTable(savepath,DIA_statin_mace_KT,"DIA_statin_mace_KT.csv")
        # saveTable(savepath,DIA_statin_mace_KT,"DIA_statin_KT_mace1.csv")
        # saveTable(savepath,DIA_statin_mace_KT,"DIA_statin_KT_mace2.csv")
        # saveTable(savepath,DIA_statin_mace_KT,"DIA_statin_KT_mace3.csv")
        
        
        DIA_statin_mace_all<-loadTable(savepath,"DIA_statin_mace_KT.csv")
        # DIA_statin_mace1<-loadTable(savepath,"DIA_statin_KT_mace1.csv")
        # DIA_statin_mace2<-loadTable(savepath,"DIA_statin_KT_mace2.csv")
        # DIA_statin_mace3<-loadTable(savepath,"DIA_statin_KT_mace3.csv")
        
        
        # DIA_statin_mace_KT<-DIA_statin_mace3
        # DIA_statin_mace_KT<-DIA_statin_mace2
        # DIA_statin_mace_KT<-DIA_statin_mace3
        
        # 63379/67914
        # 39447/67914
        ######################################################################################
        ######################################################################################
        setkey(DIA_statin_mace_KT,JID)
        setkey(P_DIA_type,JID)
        setkey(P_DM_all,JID)
        setkey(P_HTN_all,JID)
        
        P_DM_s<-P_DM_all[,.(JID,DM)]
        P_HTN_s<-P_HTN_all[,.(JID,HTN)]
        
        P_mace_base<-P_DIA_type[DIA_statin_mace_KT]
        P_mace_base<-P_DM_s[P_mace_base]
        P_mace_base<-P_HTN_s[P_mace_base]
        P_mace_base[is.na(P_mace_base)]<-0
        
        # P_mace_base[,E78:=as.integer(E78)]
        P_mace_base[,HTN:=as.integer(HTN)]
        P_mace_base[,DM:=as.integer(DM)]
        
        P_mace_base[,KT_DATE:=as.integer(KT_DATE)]
        P_mace_base[,Dx_DATE:=as.integer(Dx_DATE)]
        P_mace_base[,statin:=as.integer(statin)]
        P_mace_base[,KT:=as.integer(KT)]
        P_mace_base[,mace:=as.integer(mace)]
        ###################################################
        setkey(P_mace_base,JID)
        setkey(P_death,JID)
        
        P_mace_base<-P_death[P_mace_base]
        
        P_mace_base[Tx_DATE==20200202,Tx_DATE:=20171231]
        P_mace_base[Dx_DATE==20200202,Dx_DATE:=20171231]
        P_mace_base[KT_DATE==20200202,KT_DATE:=20171231]
        
        setcolorder(P_mace_base,c(1,3,4,5,6,7,8,9,10,11,12,13,2,14,15,16))
        
        P_mace_base[DTH_DATE!=20171231,DTH:=1]
        P_mace_base[is.na(P_mace_base)]<-0
        setnames(P_mace_base,"i.type","mace_type")
        
        P_mace_base[,dia_aft1y:=ymd(DIA_DATE)+365]
        
        # P_mace_base[statin==1&KT==0&mace==0&DTH==0] #1019
        # P_mace_base[statin==1&KT==1&mace==0&DTH==0] #236
        # P_mace_base[statin==1&KT==1&mace==1&DTH==0] #7
        # P_mace_base[statin==0&KT==1&mace==1&DTH==0] #54
        
        # # -------------------------------------------------------------------------------
        # saveTable(savepath,P_mace_base,"P_mace_base_20191001.csv")
        P_mace_base<-loadTable(statinpath,"P_mace_base_20191001.csv")
        
        P_mace_base[,dia_aft1y:=ymd(DIA_DATE)+365]
        
        P_mace_base[ymd(dia_aft1y)>ymd(KT_DATE),KT:=0] #투석1년후 이전 이식은 이식으로보지않음
        P_mace_base[ymd(dia_aft1y)>ymd(KT_DATE),KT_DATE:=20171231]
        
        P_mace_base[KT==0&mace==0&DTH==0,end_DATE:=20171231] #000
        P_mace_base[,end_DATE:=as.integer(end_DATE)]
        
        P_mace_base[KT==1&mace==0&DTH==0,end_DATE:=KT_DATE] #100
        P_mace_base[KT==0&mace==1&DTH==0,end_DATE:=Dx_DATE] #010
        P_mace_base[KT==0&mace==0&DTH==1,end_DATE:=DTH_DATE] #001
        
        P_mace_base[KT==1&mace==1&DTH==0&ymd(KT_DATE)<=ymd(Dx_DATE),end_DATE:=KT_DATE] #110
        P_mace_base[KT==1&mace==1&DTH==0&ymd(KT_DATE)>ymd(Dx_DATE),end_DATE:=Dx_DATE] #110
        
        P_mace_base[KT==1&mace==0&DTH==1,end_DATE:=KT_DATE] #101
        P_mace_base[KT==0&mace==1&DTH==1,end_DATE:=Dx_DATE] #011
        
        P_mace_base[KT==1&mace==1&DTH==1&ymd(KT_DATE)<=ymd(Dx_DATE),end_DATE:=KT_DATE]
        P_mace_base[KT==1&mace==1&DTH==1&ymd(KT_DATE)>ymd(Dx_DATE),end_DATE:=Dx_DATE]
        
        P_mace_base[KT==1&mace==1&DTH==1&ymd(KT_DATE)<=ymd(Dx_DATE),mace:=0]
        P_mace_base[KT==1&mace==1&DTH==1&ymd(KT_DATE)<=ymd(Dx_DATE),DTH:=0]
        P_mace_base[KT==1&mace==1&DTH==1&ymd(KT_DATE)>ymd(Dx_DATE),KT:=0] #110
        P_mace_base[KT==1&mace==1&DTH==1&ymd(KT_DATE)>ymd(Dx_DATE),DTH:=0] #110
        
        P_mace_base[KT==1&mace==1&DTH==0&ymd(KT_DATE)<=ymd(Dx_DATE),mace:=0]
        P_mace_base[KT==1&mace==1&DTH==0&ymd(KT_DATE)>ymd(Dx_DATE),KT:=0]
        
        P_mace_base[KT==0&mace==1&DTH==1&ymd(Dx_DATE)<=ymd(DTH_DATE),DTH:=0]
        P_mace_base[mace==0,mace_type:=0]
        
        P_mace_base[,time:=ymd(end_DATE)-ymd(dia_aft1y)]
        P_mace_base[,time_Y:=time/365.25]
        P_mace_base<-P_mace_base[time>0] #투석후1년이내 사망환자 제외
        
        # saveTable(savepath,P_mace_base,"P_mace_base_20191003.csv")
        # saveTable(savepath,P_mace_base,"P_mace_base_20200107.csv")
        # saveTable(savepath,P_mace_base,"P_mace_base_20200108.csv")
        saveTable(savepath,P_mace_base,"P_mace_base_20200108_2.csv")
        
        # P_mace_base<-loadTable(statinpath,"P_mace_base_20191003.csv")
        # P_mace_base2<-loadTable(savepath,"P_mace_base_20200107.csv")
        # P_mace_base<-loadTable(savepath,"P_mace_base_20200108.csv")
        P_mace_base2<-loadTable(savepath,"P_mace_base_20200108_2.csv")
        
        setkey(P_mace_base2,JID)
        setkey(P_I48,JID)
        
        P_mace_base_2<-P_I48[P_mace_base2]
        P_mace_base_afib<-P_mace_base_2[Afib_DATE<dia_aft1y]
        
        
        P_mace_base_afib[,N:=.N,by="JID"]
        P_mace_base_afib_2N<-P_mace_base_afib[N>=2]
        
        P_mace_base_afib_2N<-P_mace_base_afib_2N[,.SD[which.min(Afib_DATE)],by="JID"]
        P_mace_base_afib_2N<-P_mace_base_afib_2N[,.(JID,Afib_DATE)]
        
        setkey(P_mace_base_afib_2N,JID)
        setkey(P_mace_base2,JID)
        
        P_mace_base3<-P_mace_base_afib_2N[P_mace_base2]
        P_mace_base3[!is.na(Afib_DATE)]
        
        ##################################################################################
        # DT20_s2<-DT20_s[,.(JID,year=RECU_FR_DD%/%10000,INSUP_TP_CD,RVD_PLC_CD)]
        # 
        # DT20_s2<-unique(DT20_s2)
        # DT20_s2<-DT20_s2[order(JID)]
        # DT20_s2
        # 
        # # urban -  11: 서울, 21: 부산, 22: 인천, 23: 대구, 24: 광주, 25: 대전, 26: 울산, 31: 경기
        # # rural -  32: 강원, 33: 충북, 34: 충남, 35: 전북, 36: 전남, 37: 경북, 38: 경남, 39: 제주, 41: 세종
        # DT20_s2<-na.omit(DT20_s2)
        # DT20_s2[,RVD_PLC_CD:=as.integer(RVD_PLC_CD)]
        # DT20_s2[,place:=RVD_PLC_CD%/%10000]
        # 
        # DT20_s2<-unique(DT20_s2,by=c("JID","year"))
        # 
        # DT20_s2[place<=31,place2:="urban"]
        # DT20_s2[place>=32,place2:="rural"]
        # 
        # DT20_s2[INSUP_TP_CD==4,INSUP:="insurance"]
        # DT20_s2[INSUP_TP_CD==5,INSUP:="benefit"]
        # DT20_s2[INSUP_TP_CD==7,INSUP:="veteran"]
        # 
        # DT20_s2<-DT20_s2[,.(JID,year,place2,INSUP)]
        # 
        # saveTable(savepath,DT20_s2,"DT20_s2.csv")
        DT20_s2<-loadTable(savepath,"DT20_s2.csv")
        ###############################################################
        setkey(P_mace_base3,JID,year)
        setkey(DT20_s2,JID,year)
        
        baseline_table<-DT20_s2[P_mace_base3]
        
        baseline_table[is.na(Afib_DATE),Afib_DATE:=20200202]
        baseline_table[Afib_DATE==20200202,Afib:=0]
        baseline_table[Afib_DATE!=20200202,Afib:=1]
        
        baseline_table[AGE<=40,AGE_group:="<=40"]
        baseline_table[40<AGE&AGE<=60,AGE_group:="40<,<=60"]
        baseline_table[60<AGE,AGE_group:="60<"]
        
        summary(baseline_table[statin==1]$AGE)
        summary(baseline_table[statin==0]$AGE)
        
        baseline_table[mace==1|DTH==1,MACEorDeath:=1]
        baseline_table[is.na(baseline_table)]<-0
        
        saveTable(savepath,baseline_table,"baseline_table.csv")
        baseline_table<-loadTable(savepath,"baseline_table.csv")
        #########################################################################################
        setkey(P_statin_Tx,JID)
        setkey(baseline_table,JID)
        
        baseline_table_Tx<-P_statin_Tx[baseline_table]
        baseline_table_Tx[is.na(baseline_table_Tx)]<-0
        
        saveTable(statinpath,baseline_table_Tx,"baseline_table_Tx.csv")
        baseline_table_Tx<-loadTable(statinpath,"baseline_table_Tx.csv")
        
        baseline_table_Tx[INSUP=="veteran",INSUP2:="Other"]
        baseline_table_Tx[INSUP=="insurance",INSUP2:="Other"]
        baseline_table_Tx[INSUP=="benefit",INSUP2:="Benefit"]
        
        
        a<-mytable(statin~AGE+AGE_group+SEX+place2+INSUP2+HTN+DM+Afib+type+mace_type+mace+MACEorDeath+Tx_84,data=baseline_table_Tx)
        
        mycsv(a,file="baseline_table_20200115.csv")
        
        # # nearest
        m.out1<-matchit(statin ~ factor(year)+factor(AGE_group)+factor(SEX)+factor(HTN)+factor(DM)+
                          factor(type)+factor(place2)+factor(INSUP2)+factor(Afib), data=baseline_table_Tx, method = "nearest", ratio=1)
        statin_matched<-as.data.table(match.data(m.out1))
        statin_matched[,distance:=NULL]
        statin_matched[,weights:=NULL]
        
        cox1 <- coxph(Surv(time_Y,DTH)~statin,data=statin_matched)
        cox2 <- coxph(Surv(time_Y,MACEorDeath)~statin,data=statin_matched)
        cox3 <- coxph(Surv(time_Y,mace)~statin,data=statin_matched)
        
        pvalue(cox1)
        pvalue(cox2)
        pvalue(cox3)
        
        mytable(statin~AGE+AGE_group+SEX+place2+INSUP2+HTN+DM+Afib+type+mace_type+mace+MACEorDeath+Tx_84,method=3,data=statin_matched)
        
        saveTable(savepath,statin_matched,"statin_matched_place_INSUP.csv")
        # statin_matched_place<-loadTable(savepath,"statin_matched_place.csv")
        statin_matched_place_INSUP<-loadTable(savepath,"statin_matched_place_INSUP.csv")
        statin_matched_place_INSUP
        
        #####################################################################################################
        # statin_matched<-loadTable(savepath,"statin_matched_final.csv")
        # statin_matched[INSUP=="veteran",INSUP2:="Other"]
        # statin_matched[INSUP=="insurance",INSUP2:="Other"]
        # statin_matched[INSUP=="benefit",INSUP2:="Benefit"]
        # 
        # saveTable(savepath,statin_matched,"statin_matched_20200115.csv")
        
        
        a<-mytable(statin~AGE+AGE_group+SEX+place2+INSUP2+HTN+DM+Afib+type+mace_type+mace+MACEorDeath+Tx_84,method=3,data=statin_matched_place_INSUP)
        a
        mycsv(a,file="1. matched_table_20200121.csv")
    ')
}
