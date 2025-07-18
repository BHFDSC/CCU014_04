
cs <- seq(0,25)

colors = c("#1984c5", "#22a7f0", "#63bff0", "#a7d5ed",
           "#e2e2e2", "#e1a692", "#de6e56", "#4F8F23",
           "#e14b31", '#432371', "#a86464", '#005bae',
           "#c23728", "#6d4b4b",  "#AA00FF","#23628F")

plot_labels <- c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15+')

my_list <- list()                     # Create empty list
for(i in cs) { 
  my_list[i] <- colors[which(i ==cs)]
}


date <- seq.Date(from = as.Date("2019-01-01"), to= as.Date("2023/12/31"), by = "month")
month_year<-format(date, "%b_%Y")

date_scale <- seq.Date(from = as.Date("2019-01-01"), to= as.Date("2024/03/31"), by = "3 month")
month_year_scale <-format(date_scale, "%b_%Y")

dt_d <- data.table(date,month_year)

years <- c(2023)

for(y in years){
  
  fp_data <- paste0(data_root_dir,"cs_counts_dec_",y,".csv")
  
  print(fp_data)
  
  dt <- fread(fp_data)
  
  dt <- dt[!is.na(dt$age)]
  
  
  dt <- dt[ dt$age %between% c(1,95)]
  
  dt[is.na(dt)] <- 0
  
  dt <- dt[count_cs > 14 , count_cs := 15] 
  
  dt <- dt[,.(count = sum(count)), by=c('age','count_cs')]
  
  aseq <- seq(1,95)
  
  dt$age <- factor(dt$age, levels= aseq)
  
  cs <- seq(0,21)
  dt$count_cs <- factor(dt$count_cs, levels= cs)
  
  csv_save_name <- paste0(analysis_data_dir,
                          '/stack_cs_by_age_dec_',y,'.csv')
  
  write.csv(dt, csv_save_name, row.names = FALSE)
  
  g <- ggplot(dt, aes(fill=count_cs, y=count, x=age)) + 
    geom_bar(position = position_fill(reverse = TRUE), stat="identity", width=1)+
    scale_fill_manual(values=colors, labels=plot_labels) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_discrete(breaks = c(1,10,20,30,40,50,60,70,80,90,100)) +
    labs(fill="N of Medicines", x="Age",y="Proportion")
  
  plot_save_name <- paste0(analysis_plots_dir,
                           '/stack_cs_by_age_dec_',y,'.pdf')
  print(plot_save_name)
  ggsave(file=plot_save_name, g,  width= 15, height = 10) #saves g
}
