inci_cols <- list(c('sex','sex','pmeds_age_chapter_sex'),
                  c('imd_quintile','imd_quintile','pmeds_age_chapter_imd_quintile'),
                  c('imd_deci','imd_deci','pmeds_age_chapter_imd_deci'),
                  c('ethnic','ethnic','pmeds_age_chapter_ethnic'),
                  c('age_band','age_band','pmeds_age_chapter_age_band')
)


sql_str_inci_chapter_strat <- function(s_col, g_col){
  
  q <- paste0(
    "
select year,month, count(*) as count_pop,prescribed_chapter_num,
",s_col," as strat_value 
from ",table_pmeds_inci_cn," 
WHERE prescribed_chapter_num between 1 and 15
and in_pmeds = 1
and in_gdppr = 1
and sex in (1,2)
and date_of_birth is not null 
group by year, month,prescribed_chapter_num,",g_col
  )
  q
}

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

inci_strat_linked_data()


sql_inci_bnf_col <- function(table_name,col){
  
  q <- paste0(
    "
    select year,month, count(*) as count_pop,",col," 
    from ",table_name," 
    WHERE prescribed_chapter_num between 1 and 15
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

sql_inci_bnf_col(table_pmeds_inci_cn,'prescribed_chapter_num')
sql_inci_bnf_col(table_pmeds_inci_section,'prescribed_section_code')
sql_inci_bnf_col(table_pmeds_inci_para,'prescribed_para_code')
sql_inci_bnf_col(table_pmeds_inci_subpara,'prescribed_sub_para_code')

