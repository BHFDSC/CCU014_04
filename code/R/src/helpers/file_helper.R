

#save output to csv
save_dt_to_csv <- function(res,out_file){
  
  ## write without index use row.names = FALSE
  write.csv(res, out_file, row.names = FALSE)
}

#save output from sql query to csv
query_to_csv <- function(q,out_file){
  
  cat('run query : \n',q,'\n')
  cat('write file to ',out_file,'\n')
  res = dbGetQuery(con,q)
  ## write without index use row.names = FALSE
  write.csv(res, out_file, row.names = FALSE)
}


