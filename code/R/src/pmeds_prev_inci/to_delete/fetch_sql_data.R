prev_cols <- list(c('sex','sex','pmeds_age_chapter_sex'),
                  c('imd_quintile','imd_quintile','pmeds_age_chapter_imd_quintile'),
                  c('imd_deci','imd_deci','pmeds_age_chapter_imd_deci'),
                  c('ethnicity_description','ethnicity_description','pmeds_age_chapter_ethnic')
                  
)

inci_cols <- list(c('sex','sex','pmeds_age_chapter_sex'),
                  c('imd_quintile','imd_quintile','pmeds_age_chapter_imd_quintile'),
                  c('ethnicity_description','ethnicity_description','pmeds_age_chapter_ethnic'),
                  c('age_at_evet as age','age_at_event','pmeds_age_chapter_age_band')
)

sql_str_pmeds_prev_pg_linked <- function(table_name,s_col, g_col){
  
  q <- paste0(
    "SELECT year, age_at_event as age, prescribed_chapter_num,  
      count(*) as count, count(DISTINCT(NHS_NUMBER_DEID)) as count_pop
      ,",s_col," as strat_value 
       FROM ",table_name," 
                WHERE in_pmeds =1
                and in_gdppr =1
                and DATE_OF_BIRTH is not null
                AND sex is not null
                AND age_at_event between 1 and 114 
                AND sex in (1,2) 
                AND prescribed_chapter_num between 1 AND 15 
                GROUP BY year, prescribed_chapter_num, age_at_event,", g_col
    
    
  )
  q
}

sql_str_pmeds_prev_pg_linked_rates <- function( g_col,which_year){
  
  gdppr_years <- which_year - 5
  
  q <- paste0(
    "
      with res as (select 
floor(months_between('",which_year,"-01-01',DATE_OF_BIRTH)/12) as age,
case when year(date_of_death) = ",which_year," then 0.5 else 1 end as rate_value,
*
from " ,table_skinny," 
where NHS_NUMBER_DEID in(
select distinct(NHS_NUMBER_DEID)
from ", table_gdppr,"
WHERE year between ",gdppr_years," and ",which_year,"
)
AND (date_of_death is null or year(date_of_death) = ",which_year,")
)
select sum(rate_value) as rate_value, age, ", g_col," as strat_value 
from res
group by age,",g_col

  )
q
}

## loop each strat key, in run_variables

prev_strat_linked_data <- function(){
  
  for(c in prev_cols){
    
    ## prev linked
    q <-  sql_str_pmeds_prev_pg_linked(table_pmeds_prev,c[1],c[2])
    #print(q)
    fp_data <- paste0(data_root_dir,c[3],'_prev_linked.csv')
    #print(fp_data)
    query_to_csv(q,fp_data)
    
  }
}


prev_strat_linked_rates_data <- function(){
  
  which_year = 2023
  
  for(c in prev_cols){
    
    ## prev linked
    q <-  sql_str_pmeds_prev_pg_linked_rates(c[2],which_year)
    #print(q)
    fp_data <- paste0(data_root_dir,c[3],'_prev_linked_rates_',which_year,".csv")
    #print(fp_data)
    query_to_csv(q,fp_data)
    
  }
}

prev_strat_linked_data()

prev_strat_linked_rates_data()
