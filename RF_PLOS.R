pacman::p_load(ggpubr,RColorBrewer,missRanger,ranger,utils,zip,
               randomForestSRC,rfPermute,tidyverse,curl,
               dplyr,data.table, dtplyr, tidyfast, ggplot2,scales,hrbrthemes)

theme_set(theme_pubr(base_size=10,base_family="sans"))



##DOWNLOAD & READ DATA 

mydir=getwd() 
download.file("https://github.com/jbgb13/PredictorsCCP_PLOS/raw/main/data.zip", destfile ="file.zip",mode="wb" )
zip::unzip(zipfile = "file.zip", exdir = mydir)

data=as.data.table(read.csv("data.csv",colClasses=c(race="factor",country="factor",NAME_2="factor",female="factor",agro="factor",edu="factor",urban="factor",w_language="factor",cc_aware="factor",cc_stop="factor",religion="factor",religious="factor")))%>%
  .[,2:150]

data_aware=as.data.table(data)%>%
  .[,c(5:8,14:80,91,102:109)]
data_aware=data_aware[complete.cases(data_aware)]

data_human=as.data.table(data)%>%
  .[,c(5:7,10,14:80,91,102:109)]%>%
  .[!cc_human==1,b_human:=0]%>%
  .[cc_human==1,b_human:=1]%>%
  .[,b_human:=as.factor(b_human)]
data_human=data_human[complete.cases(data_human)]

data_life=as.data.table(data)%>%
  .[,c(5:7,10,11,14:80,91,102:109)]%>%
  .[!cc_life>0,b_life:=0]%>%
  .[cc_life>0,b_life:=1]%>%
  .[,b_life:=as.factor(b_life)]
data_life=data_life[complete.cases(data_life)]


data_stop=as.data.table(data)%>%
  .[,c(5:7,10:12,14:80,91,102:109)]
data_stop=data_stop[complete.cases(data_stop)]


data_agency=as.data.table(data)%>%
  .[,c(5:7,10:80,91,102:109)]%>%
  .[cc_agency<0,b_agency:=0]%>%
  .[cc_agency>=0,b_agency:=1]%>%
  .[,b_agency:=as.factor(b_agency)]
data_agency=data_agency[complete.cases(data_agency)]




#Formulas

form_cc_aware=cc_aware~
  cc_cond+cc_drought+cc_flood+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_radio+news_television+news_papers+news_internet+news_social+internet+pov_food+pov_water+pov_medicines+
  pov_fuel+pov_cash+corr_govt+corr_parlmt+corr_ngo+phobia_relig+phobia_ethn+phobia_immig+civ_lib_say+civ_lib_org+
  civ_lib_vote+dem_real+dem_satisf+trust_govt+trust_parlmt+comm_voluntary+comm_meeting+comm_issue+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_human=b_human~
  cc_cond+cc_drought+cc_flood+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_radio+news_television+news_papers+news_internet+news_social+internet+pov_food+pov_water+pov_medicines+
  pov_fuel+pov_cash+corr_govt+corr_parlmt+corr_ngo+phobia_relig+phobia_ethn+phobia_immig+civ_lib_say+civ_lib_org+
  civ_lib_vote+dem_real+dem_satisf+trust_govt+trust_parlmt+comm_voluntary+comm_meeting+comm_issue+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_life=b_life~
  cc_cond+cc_drought+cc_flood+cc_human+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_radio+news_television+news_papers+news_internet+news_social+internet+pov_food+pov_water+pov_medicines+
  pov_fuel+pov_cash+corr_govt+corr_parlmt+corr_ngo+phobia_relig+phobia_ethn+phobia_immig+civ_lib_say+civ_lib_org+
  civ_lib_vote+dem_real+dem_satisf+trust_govt+trust_parlmt+comm_voluntary+comm_meeting+comm_issue+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_stop=cc_stop~
  cc_cond+cc_drought+cc_flood+cc_human+cc_life+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_radio+news_television+news_papers+news_internet+news_social+internet+pov_food+pov_water+pov_medicines+
  pov_fuel+pov_cash+corr_govt+corr_parlmt+corr_ngo+phobia_relig+phobia_ethn+phobia_immig+civ_lib_say+civ_lib_org+
  civ_lib_vote+dem_real+dem_satisf+trust_govt+trust_parlmt+comm_voluntary+comm_meeting+comm_issue+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_agency=b_agency~
  cc_cond+cc_drought+cc_flood+cc_human+cc_life+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_radio+news_television+news_papers+news_internet+news_social+internet+pov_food+pov_water+pov_medicines+
  pov_fuel+pov_cash+corr_govt+corr_parlmt+corr_ngo+phobia_relig+phobia_ethn+phobia_immig+civ_lib_say+civ_lib_org+
  civ_lib_vote+dem_real+dem_satisf+trust_govt+trust_parlmt+comm_voluntary+comm_meeting+comm_issue+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country



#CC_AWARE

set.seed(124)
rf.Model = ranger(formula = form_cc_aware, 
                  data = data_aware,
                  num.trees = 1000,
                  min.node.size=5,
                  importance="impurity_corrected",
                  classification = T,
                  verbose=TRUE)

var_imp1=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_social",variable:="News soc. media"]%>%   
  .[variable=="news_internet",variable:="News internet"]%>%   
  .[variable=="internet",variable:="Internet use"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_papers",variable:="Newspapers"]%>%   
  .[variable=="news_television",variable:="News TV"]%>%   
  .[variable=="news_radio",variable:="News radio"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="c2_tmp_mean",variable:="Mean temp. vs. last 5y"]%>%
  .[variable=="c2_tmx_mean",variable:="Max temp. vs. last 5y"]%>%
  .[variable=="c2_pre_mean",variable:="Precip. vs. last 5y"]%>%
  .[variable=="auth_party",variable:="Pro one-party rule"]%>%
  .[variable=="migrate",variable:="Migration intention"]%>%
  .[variable=="female",variable:="Gender"]%>%
  .[variable=="auth_presi",variable:="Pro one-man rule"]%>%  
  .[variable=="cc_drought",variable:="Drought percep."]%>%
  .[variable=="edu",variable:="Education level"]%>%
  .[variable=="country",variable:="Country"]%>%
  .[variable=="trust_govt",variable:="Trust govt."]%>%   
  .[variable=="trust_parlt",variable:="Trust parlmt."]%>%   
  .[variable=="dem_real",variable:="Real democracy"]%>%   
  .[variable=="dem_satisf",variable:="Satisf. with dem."]

var_imp1=as.data.table(var_imp1%>%mutate(importance=importance/max(importance,na.rm=T)*100)%>%
                         arrange(desc(importance))%>%
                         head(15))

n1=format(rf.Model$num.samples,big.mark = ",")
acc1=round((1-rf.Model$prediction.error)*100,digits=1)

caption11=(paste0("Accuracy: ",acc1,"%"))
caption12=paste0("N = ",n1)

bluecols <- brewer.pal(9, 'Blues')
pie(rep(1,9), col = bluecols)
newcol <- colorRampPalette(bluecols)
ncols <- 100
bluecols2 <- newcol(ncols)
pie(rep(1, ncols), col = bluecols2, border = NA, labels = NA)

limit=100
g1=var_imp1%>%
  dplyr::arrange(desc(importance))%>%
  ggplot(aes(reorder(variable,importance),importance))+
  geom_col(aes(fill=importance),colour="black")+
  coord_flip()+
  labs(caption="Actual Impurity Reduction (normalized)")+
  scale_x_discrete(name=NULL)+
  scale_y_percent(name=NULL,suffix="%",scale=1,limits=c(0,limit))+
  annotate("text",x=2,y=0.3*limit,label=caption11,hjust = 0, vjust = 1, size = 3.5)+
  annotate("text",x=3,y=0.3*limit,label=caption12,hjust = 0, vjust = 1, size = 3.5)+
  scale_fill_gradientn(colours = bluecols2)+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        legend.position = "none",
        axis.ticks.y = element_line(colour="white"),
        axis.line.y =  element_blank(),
        plot.margin = ggplot2::margin(1,1.25,0.25,0.25,"cm"))

g1


##PDP CC AWARE

set.seed(124)
modelT = rfsrc.fast(formula = form_cc_aware, 
                    data = data_aware
                    ,ntree = 1000,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)

pvp = plot.variable(modelT, xvar.names = c("cc_cond","news_internet","auth_party","female"), npts=25,
                    target=2, class.type="prob", partial=T)


pdp_box1=data.table(Prob1=pvp$pData[[1]]$yhat*100)%>%
  .[,Prob2:=pvp$pData[[2]]$yhat*100]%>%
  .[,Prob3:=pvp$pData[[3]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[1]]$yhat)]
for (i in 0:4){
  pdp_box1=pdp_box1[N<=(modelT$n)*(i+1) & N>(modelT$n)*(i),"group":=(i-2)]
}

pdp_box2=data.table(Prob4=pvp$pData[[4]]$yhat*100)%>%
  .[,N:=(1:length(pvp$pData[[4]]$yhat))+modelT$n]

pdp_box2=pdp_box2[N>modelT$n*2]

pdp_all=merge.data.table(pdp_box1,pdp_box2,by="N",all.x = T)

caption1="Agric. cond."
caption2="Internet use"
caption3="Authoritarian"
caption4="Female"

top=59.8
gap=0.8

g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob2,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob3,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob4,group=group),color="#253494",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=-1.7,y=top-2*gap,label=caption1,hjust = 0, size = 3,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-2*gap,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-gap,label=caption2,hjust = 0, size = 3,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-gap,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top,label=caption4,hjust = 0, size = 3,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-3*gap,label=caption3,hjust = 0, size = 3,color="#238443",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-3*gap,color="#238443",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(52,60),scale=1)+
  labs(caption="Likert scale")+
  ylab("Probability CC awareness")+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        plot.margin = ggplot2::margin(1,0.25,0.25,0.25,"cm"))
  

g2


ggarrange(g1,g2,nrow=1,labels=c("A", "B"))
ggsave("~/GORA EKORRI/TFG_RRII/TeX/all_ccaware.png", width=19.05,height=10,units="cm")




## CC_human

set.seed(124)
rf.Model = ranger(formula = form_cc_human, 
                  data = data_human,
                  num.trees = 1000,
                  min.node.size=5,
                  importance="impurity_corrected",
                  # local.importance = T,
                  classification = T,
                  verbose=TRUE)

var_imp1=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_social",variable:="News soc. media"]%>%   
  .[variable=="news_internet",variable:="News internet"]%>%   
  .[variable=="internet",variable:="Internet use"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_papers",variable:="Newspapers"]%>%   
  .[variable=="news_television",variable:="News TV"]%>%   
  .[variable=="news_radio",variable:="News radio"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_govt",variable:="Trust govt."]%>%   
  .[variable=="trust_parlt",variable:="Trust parlmt."]%>%   
  .[variable=="dem_real",variable:="Real democracy"]%>%   
  .[variable=="dem_satisf",variable:="Satisf. with dem."]%>%
  .[variable=="pov_condtn",variable:="Living conditions"]%>%
  .[variable=="pov_quintile",variable:="Income quintile"]%>%
  .[variable=="auth_party",variable:="Pro one-party rule"]%>%
  .[variable=="migrate",variable:="Migration intention"]%>%
  .[variable=="female",variable:="Gender"]%>%
  .[variable=="auth_presi",variable:="Pro one-man rule"]%>%  
  .[variable=="cc_drought",variable:="Drought percep."]%>%
  .[variable=="community",variable:="Community action"]%>%
  .[variable=="edu",variable:="Education level"]%>%
  .[variable=="auth_army",variable:="Pro military rule"]%>%
  .[variable=="racist",variable:="Intolerant"]%>%
  .[variable=="country",variable:="Country"]%>%
  .[variable=="hh_size",variable:="Household size"]%>%
  .[variable=="w_language",variable:="Western language"]%>%
  .[variable=="religion",variable:="Religion"]
  
var_imp1=as.data.table(var_imp1%>%mutate(importance=importance/max(importance,na.rm=T)*100)%>%
                         arrange(desc(importance))%>%
                         head(15))

n1=format(rf.Model$num.samples,big.mark = ",")
acc1=round((1-rf.Model$prediction.error)*100,digits=1)

caption11=(paste0("Accuracy: ",acc1,"%"))
caption12=paste0("N = ",n1)

g1=var_imp1%>%
  dplyr::arrange(desc(importance))%>%
  ggplot(aes(reorder(variable,importance),importance))+
  geom_col(aes(fill=importance),colour="black")+
  coord_flip()+
  labs(caption="Actual Impurity Reduction (normalized)")+
  scale_x_discrete(name=NULL)+
  scale_y_percent(name=NULL,suffix="%",scale=1,limits=c(0,limit))+
  annotate("text",x=2,y=0.3*limit,label=caption11,hjust = 0, vjust = 1, size = 3.5)+
  annotate("text",x=3,y=0.3*limit,label=caption12,hjust = 0, vjust = 1, size = 3.5)+
  scale_fill_gradientn(colours = bluecols2)+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        legend.position = "none",
        axis.ticks.y = element_line(colour="white"),
        axis.line.y =  element_blank(),
        plot.margin = ggplot2::margin(1,1.25,0.25,0.25,"cm"))

g1


# PDP cc_human
set.seed(124)
modelT = rfsrc.fast(formula = form_cc_human, 
                    data = data_human,
                    ntree = 1000,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)


pvp = plot.variable(modelT, xvar.names = c("cc_drought","a2_tmp_mean","w_language","news_social"), npts=25,
                    partial=T, class.type = "prob", target=2)

pdp_1=data.table("x2"=pvp$pData[[2]]$x.uniq,"Prob2"=pvp$pData[[2]]$yhat*100,
  "ymin2"=(pvp$pData[[2]]$yhat-pvp$pData[[2]]$yhat.se)*100, "ymax2"=(pvp$pData[[2]]$yhat+pvp$pData[[2]]$yhat.se)*100,
  N=(1:25))


pdp_2=data.table(Prob3=pvp$pData[[3]]$yhat*100)%>%
  .[,N:=(1:length(pvp$pData[[3]]$yhat))+modelT$n*2]%>%
  .[N<(modelT$n*3+1),N:=N-modelT$n]

pdp_3=data.table(Prob4=pvp$pData[[4]]$yhat*100)%>%
  .[,Prob1:=pvp$pData[[1]]$yhat*100]%>%
  .[,N:=(1:length(pvp$pData[[4]]$yhat))]

pdp_box=merge.data.table(pdp_2,pdp_3,by="N",all.y=T)
for (i in 0:4){
  pdp_box=pdp_box[N<=(modelT$n)*(i+1) & N>(modelT$n)*(i),"group":=(i-2)]
}

pdp_all=merge.data.table(pdp_box,pdp_1,by="N",all.x = T)

caption1="Drought percep."
caption2="Temp. anom. (SD)"
caption3="Western language"
caption4="News soc. media"


top=55
gap=0.6
  
g2= pdp_all%>%
  ggplot()+
  geom_ribbon(aes(x=x2,ymax=ymax2,ymin=ymin2),alpha=0.2, color="#253494",fill="#253494")+
  geom_point(aes(x=x2,y=Prob2),color="#253494",alpha=1,size=1,shape=16)+
  geom_boxplot(aes(x=group,y=Prob1,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(x=group,y=Prob3,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(x=group,y=Prob4,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=-1.7,y=top-3*gap,label=caption3,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-3*gap,color="#238443",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top,label=caption2,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-2*gap,label=caption4,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-2*gap,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-gap,label=caption1,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-gap,color="#41b6c4",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(49.5,55.2),scale=1)+
  labs(caption="Likert scale | Weather anomaly (SD)")+
  ylab("Probability CC human cause")+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        plot.margin = ggplot2::margin(1,0.25,0.25,0.25,"cm"))
g2


ggarrange(g1,g2,labels = c("A","B"))
ggsave("~/GORA EKORRI/TFG_RRII/TeX/all_cchuman.png", width=19.05,height=10,units="cm")





## CC_life


set.seed(124)
rf.Model = ranger(formula = form_cc_life, 
                  data = data_life,
                  num.trees = 1000,
                  min.node.size=5,
                  importance="impurity_corrected",
                  classification=T,
                  # local.importance = T,
                  verbose=TRUE)

rf.Model
var_imp2=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_social",variable:="News soc. media"]%>%   
  .[variable=="news_internet",variable:="News internet"]%>%   
  .[variable=="internet",variable:="Internet use"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_papers",variable:="Newspapers"]%>%   
  .[variable=="news_television",variable:="News TV"]%>%   
  .[variable=="news_radio",variable:="News radio"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_govt",variable:="Trust govt."]%>%   
  .[variable=="trust_parlt",variable:="Trust parlmt."]%>%   
  .[variable=="dem_real",variable:="Real democracy"]%>%   
  .[variable=="dem_satisf",variable:="Satisf. with dem."]%>%
  .[variable=="pov_cash",variable:="Lived poverty"]%>%
  .[variable=="pov_quintile",variable:="Income quintile"]%>%
  .[variable=="auth_party",variable:="Pro one-party rule"]%>%
  .[variable=="migrate",variable:="Migration intention"]%>%
  .[variable=="female",variable:="Gender"]%>%
  .[variable=="auth_presi",variable:="Pro one-man rule"]%>%  
  .[variable=="cc_drought",variable:="Drought percep."]%>%
  .[variable=="comm_issue",variable:="Community action"]%>%
  .[variable=="edu",variable:="Education level"]%>%
  .[variable=="auth_army",variable:="Pro military rule"]%>%
  .[variable=="racist",variable:="Intolerant (race/religion)"]%>%
  .[variable=="cc_human",variable:="CC human cause"]%>%
  .[variable=="w_language",variable:="Western language"]%>%
  .[variable=="racist",variable:="Intolerant"]%>%
  .[variable=="country",variable:="Country"]%>%
  .[variable=="race",variable:="Ethnic group"]

var_imp2=as.data.table(var_imp2%>%mutate(importance=importance/max(importance,na.rm=T)*100)%>%
                         arrange(desc(importance))%>%
                         head(15))

n2=format(rf.Model$num.samples,big.mark = ",")
acc2=round((1-rf.Model$prediction.error)*100,digits=1)

caption21=(paste0("Accuracy: ",acc2,"%"))
caption22=paste0("N = ",n2)

g1=var_imp2%>%
  dplyr::arrange(desc(importance))%>%
  ggplot(aes(reorder(variable,importance),importance))+
  geom_col(aes(fill=importance),colour="black")+
  coord_flip()+
  labs(caption="Actual Impurity Reduction (normalized)")+
  scale_x_discrete(name=NULL)+
  scale_y_percent(name=NULL,suffix="%",scale=1,limits=c(0,limit))+
  annotate("text",x=2,y=0.3*limit,label=caption21,hjust = 0, vjust = 1, size = 3.5)+
  annotate("text",x=3,y=0.3*limit,label=caption22,hjust = 0, vjust = 1, size = 3.5)+
  scale_fill_gradientn(colours = bluecols2)+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        legend.position = "none",
        axis.ticks.y = element_line(colour="white"),
        axis.line.y =  element_blank(),
        plot.margin = ggplot2::margin(1,1.25,0.25,0.25,"cm"))
g1


### PDP cc_life

set.seed(124)
modelT = rfsrc.fast(formula = form_cc_life, 
                    data = data_life
                    ,ntree = 1000,
                    mtry=8,nsplit=1,nsize=5, 
                    forest = TRUE)

pvp = plot.variable(modelT, xvar.names = c("cc_human","auth_presi","cc_cond","cc_drought"), npts=25,
                    partial=T,class.type = "prob", target=2)


pdp_box1=data.table(Prob3=pvp$pData[[3]]$yhat*100)%>%
  .[,Prob2:=pvp$pData[[2]]$yhat*100]%>%
  .[,Prob4:=pvp$pData[[4]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[3]]$yhat)]
for (i in 0:4){
  pdp_box1=pdp_box1[N<=(modelT$n)*(i+1) & N>(modelT$n)*(i),"group":=(i-2)]
}

pdp_box2=data.table(Prob1=pvp$pData[[1]]$yhat)%>%
  .[,Prob1:=pvp$pData[[1]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[1]]$yhat)+modelT$n]

pdp_all=merge.data.table(pdp_box1,pdp_box2,by="N",all=T)


caption1="CC human cause"
caption2="Authoritarian"
caption3="Agric. cond."
caption4="Drought percep."

top=74.8
gap=1.3

g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#253494",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob2,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob3,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob4,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=-1.7,y=top,label=caption1,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-2*gap,label=caption3,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-2*gap,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-gap,label=caption4,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-gap,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-3*gap,label=caption2,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-3*gap,color="#238443",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(60,75),scale=1)+
  labs(caption="Likert scale")+
  ylab("Probability CC risk perception")+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        plot.margin = ggplot2::margin(1,0.25,0.25,0.25,"cm"))
g2


ggarrange(g1,g2,labels = c("A","B"))
ggsave("~/GORA EKORRI/TFG_RRII/TeX/all_cclife.png", width=19.05,height=10,units="cm")





## CC STOP

set.seed(124)
rf.Model = ranger(formula = form_cc_stop, 
                  data = data_stop,
                  num.trees = 1000,
                  min.node.size=5,
                  importance="impurity_corrected",
                  # local.importance = T,
                  classification = T,
                  verbose=TRUE)

var_imp1=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
    .[variable=="news_social",variable:="News soc. media"]%>%   .[variable=="news_internet",variable:="News internet"]%>%   .[variable=="internet",variable:="Internet use"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_papers",variable:="Newspapers"]%>%   .[variable=="news_television",variable:="News TV"]%>%   .[variable=="news_radio",variable:="News radio"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_govt",variable:="Trust govt."]%>%   .[variable=="trust_parlt",variable:="Trust parlmt."]%>%   .[variable=="dem_real",variable:="Real democracy"]%>%   .[variable=="dem_satisf",variable:="Satisf. with dem."]%>%
  .[variable=="pov_condtn",variable:="Living conditions"]%>%
  .[variable=="pov_cash",variable:="Lived poverty"]%>%
  .[variable=="auth_party",variable:="Pro one-party rule"]%>%
  .[variable=="migrate",variable:="Migration intention"]%>%
  .[variable=="female",variable:="Gender"]%>%
  .[variable=="auth_presi",variable:="Pro one-man rule"]%>% 
  .[variable=="w_language",variable:="Western language"]%>%  
  .[variable=="cc_drought",variable:="Drought percep."]%>%
  .[variable=="community",variable:="Community action"]%>%
  .[variable=="edu",variable:="Education level"]%>%
  .[variable=="auth_army",variable:="Pro military rule"]%>%
  .[variable=="cc_life",variable:="CC risk percep."]%>%
  .[variable=="cc_human",variable:="CC human cause"]%>%
  .[variable=="racist",variable:="Intolerant"]%>%
  .[variable=="country",variable:="Country"]%>%
  .[variable=="dem_best",variable:="Democratic"]%>%
  .[variable=="corruption",variable:="Corruption percep."]%>%
  .[variable=="religion",variable:="Religion"]

var_imp1=as.data.table(var_imp1%>%mutate(importance=importance/max(importance,na.rm=T)*100)%>%
                         arrange(desc(importance))%>%
                         head(15))

n1=format(rf.Model$num.samples,big.mark = ",")
acc1=round((1-rf.Model$prediction.error)*100,digits=1)

caption11=(paste0("Accuracy: ",acc1,"%"))
caption12=paste0("N = ",n1)

g1=var_imp1%>%
  dplyr::arrange(desc(importance))%>%
  ggplot(aes(reorder(variable,importance),importance))+
  geom_col(aes(fill=importance),colour="black")+
  coord_flip()+
  labs(caption="Actual Impurity Reduction (normalized)")+
  scale_x_discrete(name=NULL)+
  scale_y_percent(name=NULL,suffix="%",scale=1,limits=c(0,limit))+
  annotate("text",x=2,y=0.3*limit,label=caption11,hjust = 0, vjust = 1, size = 3.5)+
  annotate("text",x=3,y=0.3*limit,label=caption12,hjust = 0, vjust = 1, size = 3.5)+
  scale_fill_gradientn(colours = bluecols2)+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        legend.position = "none",
        axis.ticks.y = element_line(colour="white"),
        axis.line.y =  element_blank(),
        plot.margin = ggplot2::margin(1,1.25,0.25,0.25,"cm"))
g1



# PDP cc_stop
set.seed(124)
modelT = rfsrc.fast(formula = form_cc_stop, 
                    data = data_stop,
                    ntree = 1000,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)

pvp = plot.variable(modelT, xvar.names = c("cc_human","a2_tmp_mean","cc_life","cc_cond"), npts=25,
                    partial=T, class.type = "prob", target=2)

pdp=data.table("group"=pvp$pData[[2]]$x.uniq,"Prob2"=pvp$pData[[2]]$yhat*100,"x2"=pvp$pData[[2]]$x.uniq,
     "ymin2"=(pvp$pData[[2]]$yhat-pvp$pData[[2]]$yhat.se)*100, "ymax2"=(pvp$pData[[2]]$yhat+pvp$pData[[2]]$yhat.se)*100)


pdp_box1=data.table(Prob3=pvp$pData[[3]]$yhat*100)%>%
  .[,Prob4:=pvp$pData[[4]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[3]]$yhat)]
for (i in 0:4){
  pdp_box1=pdp_box1[N<=(modelT$n)*(i+1) & N>(modelT$n)*(i),"group":=(i-2)]
}

pdp_box2=data.table(Prob1=pvp$pData[[1]]$yhat)%>%
  .[,Prob1:=pvp$pData[[1]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[1]]$yhat)+modelT$n]

pdp_all=merge.data.table(pdp_box1,pdp_box2,by="N",all=T)
pdp_all=merge.data.table(pdp_all,pdp,by="group",all=T)


caption1="CC human cause"
caption2="Temp. anom. (SD)"
caption3="CC risk percep."
caption4="Agric. cond."

top=84.8
gap=1.1
  
g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  geom_ribbon(aes(x=x2,y=Prob2,ymax=ymax2,ymin=ymin2),color="#253494",alpha=0.2,fill="#253494")+
  geom_point(aes(y=Prob2),color="#253494")+
  geom_boxplot(aes(y=Prob3,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob4,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=-1.7,y=top,label=caption2,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-2*gap,label=caption4,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-2*gap,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-gap,label=caption1,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-gap,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=top-3*gap,label=caption3,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=top-3*gap,color="#238443",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(67,84),scale=1)+
  labs(caption="Likert scale | Temp. anom. (SD)")+
  ylab("Probability need to stop CC")+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        plot.margin = ggplot2::margin(1,0.25,0.25,0.25,"cm"))
g2
  
ggarrange(g1,g2,labels = c("A","B"))
ggsave("~/GORA EKORRI/TFG_RRII/TeX/all_ccstop.png", width=19.05,height=10,units="cm")







## CC AGENCY

set.seed(124)
rf.Model = ranger(formula = form_cc_agency, 
                  data = data_agency,
                  num.trees = 1000,
                  min.node.size=5,
                  importance="impurity_corrected",
                  classification = T,
                  verbose=TRUE)


var_imp2=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_social",variable:="News soc. media"]%>%   
  .[variable=="news_internet",variable:="News internet"]%>%   
  .[variable=="internet",variable:="Internet use"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_papers",variable:="Newspapers"]%>%   
  .[variable=="news_television",variable:="News TV"]%>%   
  .[variable=="news_radio",variable:="News radio"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_govt",variable:="Trust govt."]%>%   
  .[variable=="trust_parlt",variable:="Trust parlmt."]%>%   
  .[variable=="dem_real",variable:="Real democracy"]%>%   
  .[variable=="dem_satisf",variable:="Satisf. with dem."]%>%
  .[variable=="pov_condtn",variable:="Living conditions"]%>%
  .[variable=="pov_quintile",variable:="Income quintile"]%>%
  .[variable=="auth_party",variable:="Pro one-party rule"]%>%
  .[variable=="migrate",variable:="Migration intention"]%>%
  .[variable=="female",variable:="Gender"]%>%
  .[variable=="auth_presi",variable:="Pro one-man rule"]%>% 
  .[variable=="w_language",variable:="Speaks western lang."]%>%  
  .[variable=="cc_drought",variable:="Drought percep."]%>%
  .[variable=="community",variable:="Community action"]%>%
  .[variable=="religion",variable:="Religion"]%>%  
  .[variable=="fem_land",variable:="Women inherit."]%>%
  .[variable=="fem_beating",variable:="Gender violence"]%>%
  .[variable=="race",variable:="Ethnic group"]%>%
  .[variable=="edu",variable:="Education level"]%>%
  .[variable=="auth_army",variable:="Pro military rule"]%>%
  .[variable=="cc_life",variable:="CC risk percep."]%>%
  .[variable=="cc_human",variable:="CC human cause"]%>%
  .[variable=="racist",variable:="Intolerant"]%>%
  .[variable=="country",variable:="Country"]%>%
  .[variable=="rel_law",variable:="Pro religious law"]%>%
  .[variable=="dem_best",variable:="Democratic"]%>%
  .[variable=="phobia_relig",variable:="Relig. intolerance"]

var_imp2=as.data.table(var_imp2%>%mutate(importance=importance/max(importance,na.rm=T)*100)%>%
                         arrange(desc(importance))%>%
                         head(15))

n2=format(rf.Model$num.samples,big.mark = ",")
acc2=round((1-rf.Model$prediction.error)*100,digits=1)

caption21=(paste0("Accuracy: ",acc2,"%"))
caption22=paste0("N = ",n2)

limit=100
g1=var_imp2%>%
  dplyr::arrange(desc(importance))%>%
  ggplot(aes(reorder(variable,importance),importance))+
  geom_col(aes(fill=importance),colour="black")+
  coord_flip()+
  labs(caption="Actual Impurity Reduction (normalized)")+
  scale_x_discrete(name=NULL)+
  scale_y_percent(name=NULL,suffix="%",scale=1,limits=c(0,limit))+
  annotate("text",x=2,y=0.3*limit,label=caption21,hjust = 0, vjust = 1, size = 3.5)+
  annotate("text",x=3,y=0.3*limit,label=caption22,hjust = 0, vjust = 1, size = 3.5)+
  scale_fill_gradientn(colours = bluecols2)+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        legend.position = "none",
        axis.ticks.y = element_line(colour="white"),
        axis.line.y =  element_blank(),
        plot.margin = ggplot2::margin(1,1.25,0.25,0.25,"cm"))
g1


# PDP cc_agency

set.seed(124)
modelT = rfsrc.fast(formula = form_cc_agency, 
                    data = data_agency,
                    ntree = 1000,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)

pvp = plot.variable(modelT, xvar.names = c("cc_human","a2_tmp_mean","cc_life","news_radio"), npts=25,
                    partial=T, class.type = "prob", target=2)


pdp=data.table("group"=pvp$pData[[2]]$x.uniq,"Prob2"=pvp$pData[[2]]$yhat*100,"x2"=pvp$pData[[2]]$x.uniq,
               "ymin2"=(pvp$pData[[2]]$yhat-pvp$pData[[2]]$yhat.se)*100, "ymax2"=(pvp$pData[[2]]$yhat+pvp$pData[[2]]$yhat.se)*100)


pdp_box1=data.table(Prob3=pvp$pData[[3]]$yhat*100)%>%
  .[,Prob4:=pvp$pData[[4]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[3]]$yhat)]
for (i in 0:4){
  pdp_box1=pdp_box1[N<=(modelT$n)*(i+1) & N>(modelT$n)*(i),"group":=(i-2)]
}

pdp_box2=data.table(Prob1=pvp$pData[[1]]$yhat)%>%
  .[,Prob1:=pvp$pData[[1]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[1]]$yhat)+modelT$n]

pdp_all=merge.data.table(pdp_box1,pdp_box2,by="N",all=T)
pdp_all=merge.data.table(pdp_all,pdp,by="group",all=T)


caption1="CC human cause"
caption2="Temp. anom. (SD)"
caption3="CC risk percep."
caption4="News (radio)"


top=68
gap=1.2
g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  geom_ribbon(aes(x=x2,y=Prob2,ymax=ymax2,ymin=ymin2),color="#253494",alpha=0.2,fill="#253494")+
  geom_point(aes(y=Prob2),color="#253494")+
  geom_boxplot(aes(y=Prob3,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob4,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=0.3,y=top,label=caption2,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=top,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=top-gap,label=caption1,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=top-gap,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=top-2*gap,label=caption4,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=top-2*gap,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=top-3*gap,label=caption3,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=top-3*gap,color="#238443",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(62,75),scale=1)+
  labs(caption="Likert scale | Temp. anom. (SD)")+
  ylab("Probability self-efficacy")+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        plot.margin = ggplot2::margin(1,0.25,0.25,0.25,"cm"))
g2

ggarrange(g1,g2,labels = c("A","B"))

ggsave("~/GORA EKORRI/TFG_RRII/TeX/all_ccagency.png", width=19.05,height=10,units="cm")



#summary

# setnames(var_imp1,old="importance",new="imp1")
# setnames(var_imp2,old="importance",new="imp2")
# setnames(var_imp3,old="importance",new="imp3")
# setnames(var_imp4,old="importance",new="imp4")
# setnames(var_imp5,old="importance",new="imp5")
# varimp=merge.data.table(var_imp1,var_imp2,by="variable",all=T)
# varimp=merge.data.table(varimp,var_imp3,by="variable",all=T)
# varimp=merge.data.table(varimp,var_imp4,by="variable",all=T)
# varimp=merge.data.table(varimp,var_imp5,by="variable",all=T)
# 
# imp=data.table("var"=varimp$variable,"imp"=rowMeans(varimp[,2:6],na.rm=T))
# imp=imp[,imp:=imp/max(imp)*100]

