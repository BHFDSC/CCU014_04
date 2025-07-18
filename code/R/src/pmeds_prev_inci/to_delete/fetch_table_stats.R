
sql_str_pmeds_prev_pg_linked_stats <- function(){
  
  q <- paste0(
    "with res as (
  select floor(months_between('2023-01-01',DATE_OF_BIRTH)/12) as age,*
  from ",table_skinny," 
  where NHS_NUMBER_DEID in(
    select distinct(NHS_NUMBER_DEID)
    from ",table_pmeds_prev," 
    WHERE in_pmeds =1
    and in_gdppr =1
    and DATE_OF_BIRTH is not null
    AND sex is not null
    AND age_at_event between 1 and 114 
    AND sex in (1,2) 
    AND prescribed_chapter_num between 1 AND 15
  )
)
select count(NHS_NUMBER_DEID) as count_pop, sex,age,ethnicity_description,imd_quintile
from res
group by sex,age,ethnicity_description,imd_quintile"
  
  
  )


fp_data <- paste0(data_root_dir,'prev_linked_stats_counts.csv')
#print(fp_data)
query_to_csv(q,fp_data)

}

#sql_str_pmeds_prev_pg_linked_stats()

fp_data <- paste0(data_root_dir,'prev_linked_stats_counts.csv')
print(fp_data)

dt <- fread(fp_data)
dtx <- ethnic_desc_collapse(dt)
dtx <- dtx[,.(count_pop=sum(count_pop)),by=c("sex","age","ethnicity_description","imd_quintile")]


total <- dt[,sum(count_pop)]

dtx <- dtx[,age_band := age_cut(.SD), by=.I]
#dtx <- dtx[,.(count_pop=sum(count_pop)),by=c("sex","age","ethnicity_description","imd_quintile","age_band")]

strats <- c("sex","ethnicity_description","imd_quintile","age_band")
dt_out <- NULL

for(s in strats){
  dt_ss <- dtx[,sum(count_pop), by=s]
  dt_ss$total <- total
  print(dt[,sum(count_pop)])
  print(total)
  dt_ss$pct <- (dt_ss$V1/dt_ss$total)*100
  dt_ss$strat_key <- s
  setnames(dt_ss,s,'strat_value')
  dt_out <- rbind(dt_out,dt_ss)
}

fp_data <- paste0(data_root_dir,'table1_full.csv')
save_dt_to_csv(dt_out,fp_data)

dt_out <- dt_out[,c("V1","total"):=NULL]
fp_data <- paste0(data_root_dir,'table1_slim.csv')
save_dt_to_csv(dt_out,fp_data)



