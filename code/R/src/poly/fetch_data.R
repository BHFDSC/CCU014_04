### poly

fp_data <- paste0(data_root_dir,'poly_cs_counts_all.csv')
q <- paste0('select * from ',table_poly_counts)
query_to_csv(q,fp_data)
print(fp_data)

poly_strats<- c('sex','imd_quintile','ethnicity_description')
age_at_date <- '2023-01-01'

fp_data <- paste0(data_root_dir,"cs_counts_dec_2023.csv")
sql_age <- paste0("floor(months_between('",age_at_date,"',DATE_OF_BIRTH)/12) ")

q <- paste0("
    select count_cs, count(distinct(NHS_NUMBER_DEID)) as count,
    ",sql_age," as age   
     from ",table_poly_counts_month, 
            ' group by count_cs,',sql_age)

print(q)
query_to_csv(q,fp_data)
print(fp_data)


for(strat_col in poly_strats){
  
  fp_data <- paste0(data_root_dir,"cs_counts_",strat_col,"_dec_2023.csv")
  sql_age <- paste0("floor(months_between('",age_at_date,"',DATE_OF_BIRTH)/12) ")
  
  q <- paste0("
    select count_cs, count(distinct(NHS_NUMBER_DEID)) as count,
    ",sql_age," as age, ",strat_col,"   
     from ",table_poly_counts_month, 
              ' group by count_cs,',sql_age,",",strat_col)
  
  print(q)
  query_to_csv(q,fp_data)
  print(fp_data)
  
}




