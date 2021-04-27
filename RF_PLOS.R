pacman::p_load(ggpubr,RColorBrewer,missRanger,ranger,
               randomForestSRC,rfPermute,tidyverse,
               dplyr,data.table, dtplyr, tidyfast, ggplot2,scales)

theme_set(theme_pubr(base_size=10,base_family="sans"))


#ranger

form_cc_aware=cc_aware~
  cc_cond+cc_drought+cc_flood+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_tech+news_trad+poverty+corruption+racist+civil_lib+trust_instit+community+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_human=b_human~
  cc_cond+cc_drought+cc_flood+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_tech+news_trad+poverty+corruption+racist+civil_lib+trust_instit+community+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_life=b_life~
  cc_cond+cc_drought+cc_flood+cc_human+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_tech+news_trad+poverty+corruption+racist+civil_lib+trust_instit+community+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_stop=cc_stop~
  cc_cond+cc_drought+cc_flood+cc_human+cc_life+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_tech+news_trad+poverty+corruption+racist+civil_lib+trust_instit+community+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country

form_cc_agency=b_agency~
  cc_cond+cc_drought+cc_flood+cc_human+cc_life+religion+religious+rel_group+rel_law+migrate+pol_talk+pol_march+dem_best+
  dem_account+dem_elections+ eco_past+eco_future+pov_condtn+pov_quintile+
  job+auth_party+auth_army+auth_presi+free_speech+free_media+free_move+free_vigilance+fem_beating+fem_leader+
  fem_job+fem_land+fem_domestic+minority_unfair+race+age+edu+hh_size+agro+urban+female+w_language+
  news_tech+news_trad+poverty+corruption+racist+civil_lib+trust_instit+community+
  a2_tmp_mean+a2_pre_mean+a2_tmx_mean+a2_spei_mean+country






#CC_AWARE

set.seed(124)
rf.Model = ranger(formula = form_cc_aware, 
                  data = data.imp2,
                  num.trees = 1000,
                  min.node.size=5,
                  importance="impurity_corrected",
                  classification = T,
                  verbose=TRUE)


rf.Model

paco=importance_pvalues(rf.Model, method="altmann",num.permutations = 100,formula=form_a2,data=data.imp2)
varimp=as.data.table(paco)

var_imp1=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_tech",variable:="News (tech)"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_trad",variable:="News (tradit.)"]%>%
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
  .[variable=="community",variable:="Community action"]%>%
  .[variable=="edu",variable:="Education level"]%>%
  .[variable=="country",variable:="Country"]%>%
  .[variable=="trust_instit",variable:="Trust instit."]



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
bluecols2 <- newcol(ncols)#apply the function to get 100 colours
pie(rep(1, ncols), col = bluecols2, border = NA, labels = NA)

blupur <- brewer.pal(9, 'BuPu')
pie(rep(1,9), col = blupur)
newcol <- colorRampPalette(blupur)
ncols <- 100
blupur2 <- newcol(ncols)#apply the function to get 100 colours
pie(rep(1, ncols), col = blupur2, border = NA, labels = NA)

yel <- brewer.pal(9, 'YlGnBu')
pie(rep(1,9), col = yel)
newcol <- colorRampPalette(yel)
ncols <- 100
yel2 <- newcol(ncols)#apply the function to get 100 colours
pie(rep(1, ncols), col = yel2, border = NA, labels = NA)

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
modelT = rfsrc.fast(formula = form_a2, 
                    data = data.imp2
                    ,ntree = 500,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)

pvp = plot.variable(modelT, xvar.names = c("cc_cond","news_tech","auth_party"), npts=25,
                    target=2, class.type="prob", partial=T)


pdp_box1=data.table(Prob1=pvp$pData[[1]]$yhat*100)%>%
  .[,Prob2:=pvp$pData[[3]]$yhat*100]%>%
  .[,N:=1:length(pvp$pData[[1]]$yhat)]
for (i in 0:4){
  pdp_box1=pdp_box1[N<=(modelT$n)*(i+1) & N>(modelT$n)*(i),"group":=(i-2)]
}


pdp_cont=data.table("group"=rescale(pvp$pData[[2]]$x.uniq,to=c(-2,2)),"Prob4"=pvp$pData[[2]]$yhat*100,"error"=pvp$pData[[2]]$yhat.se*100)

pdp_all=merge.data.table(pdp_box1,pdp_box2,by="N",all.x = T)
pdp_all=merge.data.table(pdp_all,pdp_cont,by="group",all = T)%>%
  .[,max:=Prob4+error]%>%
  .[,min:=Prob4-error]%>%
  .[!is.na(error),group2:=group]

caption1="Agric. cond."
caption2="Authoritarian"
caption3="News tech"

g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_ribbon(aes(x=group2,ymax=max,ymin=min),alpha=0.2, color="#253494",fill="#253494")+
  geom_point(aes(y=Prob4),color="#253494",alpha=1,size=1,shape=16)+
  geom_boxplot(aes(y=Prob1,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob2,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=-1.7,y=60.5,label=caption1,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=60.5,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=62,label=caption2,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=62,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=63.5,label=caption3,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=63.5,color="#253494",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(52,64),scale=1)+
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


paco=importance_pvalues(rf.Model, method="altmann",num.permutations = 100,formula=form_cc_human,data=data_human)
varimp=as.data.table(paco)


rf.Model
var_imp1=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_tech",variable:="News (tech)"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_trad",variable:="News (tradit.)"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_instit",variable:="Trust instit."]%>%
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
  .[variable=="hh_size",variable:="Household size"]


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
                    ntree = 500,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)

pvp = plot.variable(modelT, xvar.names = c("a2_pre_mean","a2_tmp_mean","trust_instit","news_tech"), npts=25,
                    partial=T, class.type = "prob", target=2)

pdp_all=data.table("x1"=pvp$pData[[1]]$x.uniq,"Prob1"=pvp$pData[[1]]$yhat*100,
  "ymin1"=(pvp$pData[[1]]$yhat-pvp$pData[[1]]$yhat.se)*100, "ymax1"=(pvp$pData[[1]]$yhat+pvp$pData[[1]]$yhat.se)*100,
  "x2"=pvp$pData[[2]]$x.uniq,"Prob2"=pvp$pData[[2]]$yhat*100,
  "ymin2"=(pvp$pData[[2]]$yhat-pvp$pData[[2]]$yhat.se)*100, "ymax2"=(pvp$pData[[2]]$yhat+pvp$pData[[2]]$yhat.se)*100,
  "x3"=rescale(pvp$pData[[3]]$x.uniq,to=c(-2,2)),"Prob3"=pvp$pData[[3]]$yhat*100,
  "ymin3"=(pvp$pData[[3]]$yhat-pvp$pData[[3]]$yhat.se)*100, "ymax3"=(pvp$pData[[3]]$yhat+pvp$pData[[3]]$yhat.se)*100,
  "x4"=rescale(pvp$pData[[4]]$x.uniq,to=c(-2,2)),"Prob4"=pvp$pData[[4]]$yhat*100,
  "ymin4"=(pvp$pData[[4]]$yhat-pvp$pData[[4]]$yhat.se)*100, "ymax4"=(pvp$pData[[4]]$yhat+pvp$pData[[4]]$yhat.se)*100)

caption1="Precip. anom. (SD)"
caption2="Temp. anom. (SD)"
caption3="Trust institutions"
caption4="News tech"

g2= pdp_all%>%
  ggplot()+
  geom_ribbon(aes(x=x1,ymax=ymax1,ymin=ymin1),alpha=0.2, color="#41b6c4",fill="#41b6c4")+
  geom_point(aes(x=x1,y=Prob1),color="#41b6c4",alpha=1,size=1,shape=16)+
  geom_ribbon(aes(x=x2,ymax=ymax2,ymin=ymin2),alpha=0.2, color="#253494",fill="#253494")+
  geom_point(aes(x=x2,y=Prob2),color="#253494",alpha=1,size=1,shape=16)+
  geom_ribbon(aes(x=x3,ymax=ymax3,ymin=ymin3),alpha=0.2, color="#addd8e",fill="#addd8e")+
  geom_point(aes(x=x3,y=Prob3),color="#addd8e",alpha=1,size=1,shape=16)+
  geom_ribbon(aes(x=x4,ymax=ymax4,ymin=ymin4),alpha=0.2, color="#238443",fill="#238443")+
  geom_point(aes(x=x4,y=Prob4),color="#238443",alpha=1,size=1,shape=16)+
  annotate("text",x=-1.7,y=47.5,label=caption4,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=47.5,color="#238443",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=49,label=caption2,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=49,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=48,label=caption3,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=48,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=48.5,label=caption1,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=48.5,color="#41b6c4",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(47,53),scale=1)+
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

paco=importance_pvalues(rf.Model, method="altmann",num.permutations = 100,formula=form_cc_human,data=data_human)
varimp=as.data.table(paco)
beep()


rf.Model
var_imp2=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_tech",variable:="News (tech)"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_trad",variable:="News (tradit.)"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_instit",variable:="Trust instit."]%>%
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
  .[variable=="racist",variable:="Intolerant (race/religion)"]%>%
  .[variable=="cc_human",variable:="CC human cause"]%>%
  .[variable=="w_language",variable:="Western language"]%>%
  .[variable=="racist",variable:="Intolerant"]%>%
  .[variable=="country",variable:="Country"]


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
                    ,ntree = 500,
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


g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#253494",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob2,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob3,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob4,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=-1.7,y=74.5,label=caption1,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=74.5,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=71.5,label=caption3,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=71.5,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=73,label=caption4,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=73,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=-1.7,y=70,label=caption2,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=-2,xmin=-2.2,xmax = -1.8,y=70,color="#238443",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(55,75),scale=1)+
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

paco=importance_pvalues(rf.Model, method="altmann",num.permutations = 100,formula=form_cc_stop,data=data_stop)
varimp=as.data.table(paco)
beep()

rf.Model
var_imp1=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_tech",variable:="News (tech)"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_trad",variable:="News (tradit.)"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_instit",variable:="Trust instit."]%>%
  .[variable=="pov_condtn",variable:="Living conditions"]%>%
  .[variable=="pov_quintile",variable:="Income quintile"]%>%
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
  .[variable=="corruption",variable:="Corruption percep."]



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
                    ntree = 500,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)


pvp = plot.variable(modelT, xvar.names = c("cc_human","a2_tmp_mean","cc_life"), npts=25,
                    partial=T, class.type = "prob", target=2)

pdp=data.table("group"=pvp$pData[[2]]$x.uniq,"Prob2"=pvp$pData[[2]]$yhat*100,"x2"=pvp$pData[[2]]$x.uniq,
     "ymin2"=(pvp$pData[[2]]$yhat-pvp$pData[[2]]$yhat.se)*100, "ymax2"=(pvp$pData[[2]]$yhat+pvp$pData[[2]]$yhat.se)*100)


pdp_box1=data.table(Prob3=pvp$pData[[3]]$yhat*100)%>%
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


  g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  geom_ribbon(aes(x=x2,y=Prob2,ymax=ymax2,ymin=ymin2),color="#253494",alpha=0.2,fill="#253494")+
  geom_point(aes(y=Prob2),color="#253494")+
  geom_boxplot(aes(y=Prob3,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=0.3,y=70,label=caption2,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=70,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=68,label=caption1,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=68,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=66,label=caption3,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=66,color="#addd8e",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(63,82),scale=1)+
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

paco=importance_pvalues(rf.Model, method="altmann",num.permutations = 100,formula=form_cc_agency,data=data_agency)
varimp=as.data.table(paco)
beep()

rf.Model
var_imp2=data.table("variable"=names(rf.Model$variable.importance),"importance"=unname(rf.Model$variable.importance))%>%
  .[variable=="news_tech",variable:="News (tech)"]%>%
  .[variable=="cc_cond",variable:="Agric. condn."]%>%
  .[variable=="news_trad",variable:="News (tradit.)"]%>%
  .[variable=="pol_talk",variable:="Talks politics"]%>%
  .[variable=="a2_tmp_mean",variable:="Mean temp. anom."]%>%
  .[variable=="a2_tmx_mean",variable:="Max temp. anom."]%>%
  .[variable=="a2_pre_mean",variable:="Precip. anom."]%>%
  .[variable=="a2_spei_mean",variable:="Annual SPEI"]%>%
  .[variable=="cc_flood",variable:="Flood percep."]%>%
  .[variable=="poverty",variable:="Lived poverty"]%>%
  .[variable=="eco_past",variable:="Eco. past"]%>%
  .[variable=="eco_future",variable:="Eco. future"]%>%
  .[variable=="trust_instit",variable:="Trust instit."]%>%
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
  .[variable=="corruption",variable:="Corruption percep."]

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
                    ntree = 500,
                    mtry=8,nsplit=1, 
                    nodesize=5,forest = TRUE)


pvp = plot.variable(modelT, xvar.names = c("cc_human","a2_tmp_mean","cc_life","auth_presi"), npts=25,
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
caption4="Authoritarian"

g2= pdp_all%>%
  ggplot(aes(x=group))+
  geom_boxplot(aes(y=Prob1,group=group),color="#41b6c4",alpha=1,na.rm=T,width=0.5)+
  geom_ribbon(aes(x=x2,y=Prob2,ymax=ymax2,ymin=ymin2),color="#253494",alpha=0.2,fill="#253494")+
  geom_point(aes(y=Prob2),color="#253494")+
  geom_boxplot(aes(y=Prob3,group=group),color="#addd8e",alpha=1,na.rm=T,width=0.5)+
  geom_boxplot(aes(y=Prob4,group=group),color="#238443",alpha=1,na.rm=T,width=0.5)+
  annotate("text",x=0.3,y=66.5,label=caption2,hjust = 0, size = 3.5,color="#253494",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=66.5,color="#253494",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=65,label=caption1,hjust = 0, size = 3.5,color="#41b6c4",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=65,color="#41b6c4",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=63.5,label=caption3,hjust = 0, size = 3.5,color="#addd8e",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=63.5,color="#addd8e",alpha=1,size=0.25)+
  annotate("text",x=0.3,y=62,label=caption4,hjust = 0, size = 3.5,color="#238443",fontface="bold")+
  annotate("pointrange",x=0,xmin=-0.2,xmax = 0.2,y=62,color="#238443",alpha=1,size=0.25)+
  scale_x_continuous(name=NULL,breaks = c(-2:2))+
  scale_y_percent(limits=c(60,75),scale=1)+
  labs(caption="Likert scale | Temp. anom. (SD)")+
  ylab("Probability self-efficacy")+
  theme(plot.caption = element_text(face="plain",size=10,hjust = 0.5),
        panel.grid = element_line(linetype="dotted"),
        plot.margin = ggplot2::margin(1,0.25,0.25,0.25,"cm"))
g2

ggarrange(g1,g2,labels = c("A","B"))

ggsave("~/GORA EKORRI/TFG_RRII/TeX/all_ccagency.png", width=19.05,height=10,units="cm")



