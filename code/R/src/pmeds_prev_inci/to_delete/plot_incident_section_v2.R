
dt_chapters <- get_chapter_colors()
years = c(2019,2020,2021,2022,2023,2024)
plot_dates =  c('2019-11-01','2024-09-01')

fp_data <- paste0(data_root_dir,'pmeds_prescribed_section_code_inci_linked.csv')
print(fp_data)

dt <-fread(fp_data, colClasses = c(prescribed_section_code = "character"))
dt$prescribed_chapter_num <- as.integer(
  substring(dt$prescribed_section_code,1,2))

dt <- dt[year %in% years ]
dt <- add_date_col(dt)
dt <- dt[date %between% plot_dates]



dt <- dt_swap_bnf_chapters(dt_chapters,dt)
dt_section_map <- bnf_sections_list()
dt <- merge (x=dt, y=dt_section_map, by='prescribed_section_code', 
             all.x = T)

dt <- dt[,prescribed_section_code := NULL]

setnames(dt,'BNF_Section','prescribed_section_code')


all_plots <- list()
chaps_seq <- seq(1,15)
#chaps_seq <- c(1, 4, 7, 10, 13,5,2, 6, 9, 15,3,  11, 12, 8, 14)
#chaps_seq <- c(1, 4, 7, 10)

for(c_id in chaps_seq){
  #  stuff, such as
  print(c_id)
  
  
  dt_c <-dt[ dt$prescribed_chapter_num == c_id]
  cc <- unique(dt_c$chapter_color)
  plot_title <- unique(dt_c$chapter_text)
  
  g <- ggplot(dt_c, aes(x=date, y=count_pop, 
                        group=prescribed_section_code
  )) + 
    geom_line(aes(colour=prescribed_section_code)) +
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month") +
    scale_y_continuous(labels = addUnits) +
    geom_vline(xintercept=as.numeric(covid_dates), colour="grey")+
    #scale_color_manual( name="chapter", values=cc) +
    theme_bw() +
    theme(legend.position = "right", 
          legend.title = element_blank(),
          
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = paste0(plot_title), y = "Incident Count", x = "Date") +
    guides(colour=guide_legend(ncol =1))
  
  all_plots[[(length(all_plots) +1)]] <- g
  
}


plot_main_title <- 'Incidents By Section Sep 2019 - Sep 2024'
plot_save_name <- paste0(analysis_plots_dir,'incidents_by_section_v2_1.pdf')

g <- plot_grid(all_plots[[4]],all_plots[[7]],
               all_plots[[5]],all_plots[[3]],all_plots[[8]], align ='v', ncol=1)

print(plot_save_name)
ggsave(file=plot_save_name, g, height=20, width=10) #saves g

plot_save_name <- paste0(analysis_plots_dir,'incidents_by_section_v2_2.pdf')

g <- plot_grid(all_plots[[2]],all_plots[[6]],
               all_plots[[9]],all_plots[[15]],all_plots[[11]], align ='v', ncol=1)

print(plot_save_name)
ggsave(file=plot_save_name, g, height=20, width=10) #saves g

plot_save_name <- paste0(analysis_plots_dir,'incidents_by_section_v2_3.pdf')

g <- plot_grid(all_plots[[1]],all_plots[[10]],
               all_plots[[13]],all_plots[[12]],all_plots[[14]], align ='v', ncol=1)

print(plot_save_name)
ggsave(file=plot_save_name, g, height=20, width=10) #saves g




