
inci_cols <- list(c('sex','sex','pmeds_age_chapter_sex'),
                  c('imd_quintile','imd_quintile','pmeds_age_chapter_imd_quintile'),
                  c('imd_deci','imd_deci','pmeds_age_chapter_imd_deci'),
                  c('ethnic','ethnic','pmeds_age_chapter_ethnic'),
                  c('age_band','age_band','pmeds_age_chapter_age_band')
)

sql_inci_bnf_col <- function(table_name,col){
  
  q <- paste0(
    "
select year,month, count(*) as count_pop,",col," 
from ",table_name," 
WHERE prescribed_chapter_num between (1,15) 
and in_pmeds = 1
and in_gdppr = 1
and sex in (1,2)
and date_of_birth is not null 
group by year, month,",col
  )
  
  fp_data <- paste0(data_root_dir,'pmeds_',col,'_inci_linked.csv')
  print(fp_data)
  query_to_csv(q,fp_data)
}



inci_bnf_chapter <- function(){
  
  q <- paste0(
    "
select year,month,sum(count_pop) as count_pop,prescribed_chapter_num
from ",table_pmeds_inci_cn," 
group by year, month,prescribed_chapter_num
"
  )
  
  print(q)
  fp_data <- paste0(data_root_dir,'pmeds_chapter_inci_linked.csv')
  print(fp_data)
  query_to_csv(q,fp_data)
}


inci_bnf_section <- function(){
  
  q <- paste0(
    "
select year,month,sum(count_pop) as count_pop,prescribed_section_code
from ",table_pmeds_inci_section," 
group by year, month,prescribed_section_code
"
  )
  
  print(q)
  fp_data <- paste0(data_root_dir,'pmeds_section_inci_linked.csv')
  print(fp_data)
  query_to_csv(q,fp_data)
}

sql_str_inci_chapter_strat <- function(s_col, g_col){
  
  q <- paste0(
    "
select year,month,sum(count_pop) as count_pop,prescribed_chapter_num,
",s_col," as strat_value 
from ",table_pmeds_inci_cn," 
group by year, month,prescribed_chapter_num,",g_col
  )
  q
}


## loop each strat key, in run_variables

inci_strat_linked_data <- function(){
  
  for(c in inci_cols){
    
    ## prev linked
    q <-  sql_str_inci_chapter_strat(c[1],c[2])
    print(q)
    fp_data <- paste0(data_root_dir,c[3],'_inci_linked.csv')
    print(fp_data)
    query_to_csv(q,fp_data)
    
  }
}

inci_bnf_chapter()
inci_strat_linked_data()

sql_inci_bnf_col(table_pmeds_inci_cn,'prescribed_chapter_num')
sql_inci_bnf_col(table_pmeds_inci_section,'prescribed_section_code')
sql_inci_bnf_col(table_pmeds_inci_para,'prescribed_para_code')
sql_inci_bnf_col(table_pmeds_inci_subpara,'prescribed_sub_para_code')

sql_str_cs_by_chapter <- function(chap){
  q <- paste0("
                select year,month,prescribed_chemical_substance_code,sum(count_pop) as count_pop
from dsa_391419_j3w9t_collab.ccu014_00_pmeds_stats_inci_bnf_cs_linked_pg_wdd_141124
where cast(substring(prescribed_chemical_substance_code,1,2) as int) = ",chap,"
group by year,month,prescribed_chemical_substance_code
                ")
  
  fp_data <- paste0(data_root_dir,'cs_by_chap_',chap,'_inci_linked.csv')
  print(fp_data)
  query_to_csv(q,fp_data)
  
}

sql_str_cs_by_chapter(14)
