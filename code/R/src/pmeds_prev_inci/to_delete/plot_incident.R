

years = c(2019,2020,2021,2022,2023,2024)
plot_dates =  c('2019-09-01','2024-09-01')
#years = c(2019,2020,2021)
#years = c(2023)


dt_chapters <- get_chapter_colors()

fp_data <- paste0(data_root_dir,'pmeds_prescribed_chapter_num_inci_linked.csv')
print(fp_data)

dt <-fread(fp_data)
dt <- dt[year %in% years ]

dt <- add_date_col(dt)
dt <- dt[date %between% plot_dates]

dt <- dt_swap_bnf_chapters(dt_chapters,dt)


all_plots <- list()
chaps_seq <- seq(1,15)
chaps_seq <- c(1, 4, 7, 10, 13,5,2, 6, 9, 15,3,  11, 12, 8, 14)

for(c_id in chaps_seq){
  #  stuff, such as
  print(c_id)
  
  
  dt_c <-dt[ dt$prescribed_chapter_num == c_id]
  cc <- unique(dt_c$chapter_color)
  plot_title <- unique(dt_c$chapter_text)
  
  g <- ggplot(dt_c, aes(x=date, y=count_pop, color=chapter_text)) + geom_line() +
    scale_x_date(date_labels="%b %y",date_breaks  ="3 month") +
    scale_y_continuous(labels = addUnits) +
    geom_vline(xintercept=as.numeric(covid_dates), colour="grey")+
    scale_color_manual( name="chapter", values=cc) +
    theme_bw() +
    theme(legend.position = "none", 
          
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title = paste0(plot_title), y = "Incident Count", x = "Date")
  
  all_plots[[(length(all_plots) +1)]] <- g
  
}

plot_main_title <- 'Incidents By Chapter Sep 2019 - Sep 2024'
plot_main_title <- ''
plot_save_name <- paste0(analysis_plots_dir,'incidents_by_chapter.pdf')

g <- grid.arrange(arrangeGrob(grobs = all_plots,   ncol=3 ),
                  
                  top = textGrob(plot_main_title,gp=gpar(fontsize=30,fontface = 'bold')))


print(plot_save_name)
ggsave(file=plot_save_name, g, height=15, width=15) #saves g



