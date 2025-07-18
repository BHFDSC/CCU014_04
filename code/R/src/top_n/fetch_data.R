

sql_str <- function(){
  
  q <- paste0("select Year(event_date) as year, PrescribedBNFCode, count(*) as count,
                count(distinct(NHS_NUMBER_DEID)) as count_pop, 
                combined_age_band, 
                combined_sex",
              " FROM ",table_pmeds_prev,"
                WHERE Year(event_date) = 2023 
                AND NHS_NUMBER_DEID is not null 
                AND combined_age between 1 and 112 
                AND combined_sex in (1,2) 
                AND prescribed_chapter_num between 1 AND 15 
                group by Year(event_date), PrescribedBNFCode,combined_age_band, combined_sex"
  )
  q
}



q <-  sql_str()
out_file <- paste0(data_root_dir,'top_meds_age_sex_linked.csv')
print(out_file)
query_to_csv(q,out_file)

