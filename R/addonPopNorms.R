# sample norms

rm(list=ls())

library(jsonlite)
library(ggplot2)
library(presize)
library(reshape2)
library(knitr)

# load utility functions
source("./R/mobqol_util.R")

# define dimension ID
ds_dims = c("AC","CO","PD","IN","SE","ME","AX")

# load raw json files
mq_jsons = read_json("./data/respones_anonymised.json")

# GP
eligible_ids_gp = read.csv("./data/gp_anonymised.csv")$id
gp = retrieveMobQoLPUFs(raw_jsons = mq_jsons, eligible_ids = eligible_ids_gp)

eligible_ids_mi = read.csv("./data/mi_anonymised.csv")$id
mi = retrieveMobQoLPUFs(raw_jsons = mq_jsons, eligible_ids = eligible_ids_mi)


# social value set - gp
social_model = matrix(apply(gp$pufs_trimmed,2,mean), ncol = length(ds_dims))
ds_expanded = expand.grid(lapply(1:7, \(x) 1:4))
ds_str = apply(ds_expanded,1,\(x) paste0(x,collapse=""))
mq_vs = c()
for(i in 1:nrow(ds_expanded)){
  v_ = 1
  for(j in seq_along(ds_expanded[i,])){
    v_ = v_ - social_model[ds_expanded[i,j],j]
  }
  mq_vs = c(mq_vs, v_)
}

# social value set - MI
social_model_mi = matrix(apply(mi$pufs_trimmed,2,mean), ncol = length(ds_dims))
mi_vs = c()
for(i in 1:nrow(ds_expanded)){
  v_ = 1
  for(j in seq_along(ds_expanded[i,])){
    v_ = v_ - social_model_mi[ds_expanded[i,j],j]
  }
  mi_vs = c(mi_vs, v_)
}


# GP VALUES GP NORMS

gp_pop = gp$groups_df[gp$groups_df$group %in% c("gender","age"),]
gp_pop = reshape(gp_pop,direction = "wide",timevar="group")
names(gp_pop) = c("id","gender","age")
gp_pop = merge(gp_pop, gp$own_state_df, by = "id")
hs_exp_gp = do.call(rbind,strsplit(gp_pop$state, ""))
hs_exp_gp = t(apply(hs_exp_gp,1,as.numeric))

gp_util = c()
for(i in 1:nrow(hs_exp_gp)){
  v_ = 1
  for(j in seq_along(hs_exp_gp[i,])){
    v_ = v_ - social_model[hs_exp_gp[i,j],j]
  }
  gp_util = c(gp_util, v_)
}

gp_pop$util = gp_util

norms_gp_gp = aggregate(util ~ age+gender,gp_pop, \(x){
  round(c("mean"=mean(x),"sd"=sd(x),"n"=length(x)),3)
} )
norms_gp_gp = cbind(norms_gp_gp[,-3],norms_gp_gp[,3])
names(norms_gp_gp)= c("age","gender","mean_MRQoL","SD","n")
write.csv(norms_gp_gp,"./output/norms_gp_gp.csv", row.names = F)



# GP VALUES MI SAMPLE

mi_pop = mi$groups_df[mi$groups_df$group %in% c("gender","age"),]
mi_pop = reshape(mi_pop,direction = "wide",timevar="group")
names(mi_pop) = c("id","gender","age")
mi_pop = merge(mi_pop, mi$own_state_df, by = "id")
hs_exp_mi = do.call(rbind,strsplit(mi_pop$state, ""))
hs_exp_mi = t(apply(hs_exp_mi,1,as.numeric))

mi_util_by_gp = mi_util_by_mi = c()
for(i in 1:nrow(hs_exp_mi)){
  v_ = 1
  w_ = 1
  for(j in seq_along(hs_exp_mi[i,])){
    v_ = v_ - social_model[hs_exp_mi[i,j],j]
    w_ = w_ - social_model_mi[hs_exp_mi[i,j],j]
  }
  mi_util_by_gp = c(mi_util_by_gp, v_)
  mi_util_by_mi = c(mi_util_by_mi, w_)
}

mi_pop$util_by_gp = mi_util_by_gp
mi_pop$util_by_mi = mi_util_by_mi


norms_mi_by_gp = aggregate(cbind(util_by_gp,mi_util_by_mi) ~ age+gender,mi_pop, \(x){
  round(c("mean"=mean(x),"sd"=sd(x),"n"=length(x)),3)
} )
norms_mi_by_gp = cbind(norms_mi_by_gp[,-c(3,4)],norms_mi_by_gp[,3],norms_mi_by_gp[,4])
names(norms_mi_by_gp)= c("age","gender","mean_MRQoL_by_GP","SD_by_GP","n","mean_MRQoL_by_MI","SD_by_MI","n")
norms_mi_by_gp = norms_mi_by_gp[,-5]
write.csv(norms_mi_by_gp,"./output/norms_mi.csv", row.names = F)





