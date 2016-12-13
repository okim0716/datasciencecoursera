### Step 6b ============
df<- read.csv("step5b_claims_etg_costs.csv")
df<- read.csv("/mapr/datalake/optum/optuminsight/sandbox2/dsu/redflag/v2/step5b_claims_etg_costs.csv")
dim(df) ### 486899 6

d5a<- read.csv("/mapr/datalake/optum/optuminsight/sandbox2/dsu/redflag/v2/step5a_etg_summary_status.csv")
dim(d5a) ### 6621687      44

data2014 <- d5a[((d5a$start_date >= 20140101) & (d5a$end_date <= 20141231)),]
dim(data2014) ### 3399860      44

# extracts individual id, member age and gender
agegender2014 <- unique(data2014[,c('nhi_individual_id', 'age', 'gender')])
dim(agegender2014) ### 507857      3

 agegender2014<-agegender2014[agegender2014$gender %in% c('M','F'),]
dim(agegender2014) ### 507849      3
agegender2014<- merge(df, agegender2014, by='nhi_individual_id', all=FALSE)
> dim(agegender2014) ### 486891      8

# this function creates dynamic age buckets for members to be grouped in and creates
# 1 column for each bucket with binary entries for each member
buckets <- quantile(agegender2014$age, c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
previous <- -1
for (age in buckets) {
    agegender2014[paste('age', as.character(previous + 1),'to', as.character(age), sep='_')] <- as.factor(ifelse(((agegender2014$age <= age) & (agegender2014$age > previous)), 1, 0))
    previous <- age
}
agegender2014[paste('age_', as.character(age + 1), '+', sep='')] <- as.factor(ifelse(agegender2014$age > previous, 1, 0))

agegender2014$female <- as.factor(ifelse(agegender2014$gender == 'F', 1, 0)) # binary gender representation
dim(agegender2014)  ### 486891      19

# Write into /v2 folder
write.csv(agegender2014, "/mapr/datalake/optum/optuminsight/sandbox2/dsu/redflag/v2/step6b_age_gender2014.csv")
#############
