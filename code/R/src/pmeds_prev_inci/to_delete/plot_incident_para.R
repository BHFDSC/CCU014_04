
dt_chapters <- get_chapter_colors()
years = c(2019,2020,2021,2022,2023,2024)
plot_dates =  c('2019-09-01','2024-09-01')

fp_data <- paste0(data_root_dir,'pmeds_prescribed_para_code_inci_linked.csv')
print(fp_data)

dt <-fread(fp_data, colClasses = c(prescribed_para_code = "character"))
dt <- dt[year %in% years ]
dt <- add_date_col(dt)
dt <- dt[date %between% plot_dates]

dt$prescribed_chapter_num <- as.integer(
  substring(dt$prescribed_para_code,1,2))

dt <- dt_swap_bnf_chapters(dt_chapters,dt)
dt_section_map <-bnf_para_list()

dt <- merge (x=dt, y=dt_section_map, by='prescribed_para_code', 
             all.x = T)

dt <- dt[,prescribed_para_code := NULL]

setnames(dt,'BNF_Paragraph','prescribed_para_code')


#all_plots <- list()
#chaps_seq <- c(1, 4, 7, 10, 13,5)

get_inci_plot_by_chapterx <- function(dt,group_col_name,chaps_seq){
  
  
  for(c_idx in chaps_seq){
    cat('FF - ',c_idx,'\n')
  }
}
get_inci_plot_by_chapter <- function(dt,group_col_name,chaps_seq){
  
  #print(chaps_seq)
  #print(typeof(chaps_seq))
  group_col = sym(group_col_name)
  
  all_plots <- list()
  
  for(c_id in chaps_seq){
    #  stuff, such as
    
    dt_c <- dt[ dt$prescribed_chapter_num == c_id]
    cc <- unique(dt_c$chapter_color)
    plot_title <- unique(dt_c$chapter_text)
    cat('title ',plot_title,'\n')
    
    g <- ggplot(dt_c, aes(x=date, y=count_pop, 
                          group=!!group_col
    )) + 
      geom_line(aes(colour=!!group_col)) +
      scale_x_date(date_labels="%b %y",date_breaks  ="3 month") +
      scale_y_continuous(labels = addUnits) +
      geom_vline(xintercept=as.numeric(covid_dates), colour="grey")+
      #scale_color_manual( name="chapter", values=cc) +
      theme_bw() +
      theme(legend.position = "bottom", 
            legend.title = element_blank(),
            
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = paste0(plot_title), y = "Incident Count", x = "Date") +
      guides(colour=guide_legend(ncol =2))
    
    all_plots[[(length(all_plots) +1)]] <- g
    
  }
  
  all_plots
}

chaps_seq <- list(list(1, 4, 7, 10, 13),list(5,2, 6, 14, 15), list(11, 12, 8, 9))

for(i in 1:length(chaps_seq)){
  # print(i)
  #print(chaps_seq[i])
  
  all_plots <- get_inci_plot_by_chapter(dt,'prescribed_para_code',chaps_seq[[i]])
  
  plot_main_title <- 'Incidents By Paragraph Sep 2019 - Sep 2024'
  plot_save_name <- paste0(analysis_plots_dir,'incidents_by_para_',i,'.pdf')
  
  g <- grid.arrange(arrangeGrob(grobs = all_plots,   ncol=2 ),
                    
                    top = textGrob(plot_main_title,
                                   gp=gpar(fontsize=30,fontface = 'bold')))
  
  
  print(plot_save_name)
  ggsave(file=plot_save_name, g, height=40, width=18) #saves g
  
  
}


