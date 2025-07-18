
y <- 2023
csv_save_name <- paste0(analysis_data_dir,
                        '/stack_cs_by_age_dec_',y,'.csv')

dt <-fread(csv_save_name)

sums <- dt[,sum := sum(count), by= age]

sums$prop <- format(sums$count/sums$sum, scientific = FALSE)

sums <- sums[,c('count','sum') :=NULL]

out_fp <- paste0(analysis_data_dir,
                 '/prop_data_stack_cs_by_age_dec_',y,'.csv')

write.csv(sums, out_fp, row.names = FALSE)

