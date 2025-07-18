source('./src/pmeds_prev_inci/plot_incident_vars.R')


do_section <- function(){
  chaps_seq <- c(1, 4, 7, 10, 13,5,2, 6, 9, 15,3,  11, 12, 8, 14)
  g_col <- 'prescribed_section_code'
  dt <- data_section()
  all_plots <- get_inci_plot_by_chapter(dt,g_col,chaps_seq)
  
  plot_main_title <- 'Incidents By Section Nov 2019 - Sep 2024'
  plot_save_name <- paste0(analysis_plots_dir,'incidents_by_section.pdf')
  
  g <- grid.arrange(arrangeGrob(grobs = all_plots,   ncol=2 ),
                    
                    top = textGrob(plot_main_title,
                                   gp=gpar(fontsize=30,fontface = 'bold')))
  
  
  print(plot_save_name)
  ggsave(file=plot_save_name, g, height=40, width=18) #saves g
}


do_para <- function(){
  
  chaps_seq <- list(list(1, 4, 7, 10, 13),list(5,2, 6, 14, 15), list(11, 12, 8, 9))
  g_col <- 'prescribed_para_code'
  dt <- data_para()
  
  for(i in 1:length(chaps_seq)){
    # print(i)
    #print(chaps_seq[i])
    
    all_plots <- get_inci_plot_by_chapter(dt,g_col,chaps_seq[[i]])
    
    plot_main_title <- 'Incidents By Paragraph Nov 2019 - Sep 2024'
    plot_save_name <- paste0(analysis_plots_dir,'incidents_by_para_',i,'.pdf')
    
    g <- grid.arrange(arrangeGrob(grobs = all_plots,   ncol=2 ),
                      
                      top = textGrob(plot_main_title,
                                     gp=gpar(fontsize=30,fontface = 'bold')))
    
    
    print(plot_save_name)
    ggsave(file=plot_save_name, g, height=40, width=18) #saves g
    
    
  }
}

do_chapter <- function(){
  
  all_plots <- list()
  
  chaps_seq <- c(1, 4, 7, 10, 13,5,2, 6, 9, 15,3,  11, 12, 8, 14)
  dt <- data_chapters()
  
  for(c_id in chaps_seq){
    
    dt_c <-dt[ dt$prescribed_chapter_num == c_id]
    cc <- unique(dt_c$chapter_color)
    plot_title <- unique(dt_c$chapter_text)
    
    g <- ggplot(dt_c, aes(x=date, y=count_pop, color=chapter_text)) + geom_line() +
      scale_x_date(
        date_labels="%b %y",
        date_breaks  ="3 month",
        limits= plot_dates_limits,
        expand = c(0,0)
      ) +
      scale_y_continuous(labels = addUnits) +
      geom_vline(xintercept=as.numeric(covid_dates), colour="grey")+
      scale_color_manual( name="chapter", values=cc) +
      theme_bw() +
      theme(legend.position = "none", 
            
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = paste0(plot_title), y = "Incident Count", x = "Date")
    
    all_plots[[(length(all_plots) +1)]] <- g
    
  }
  
  plot_main_title <- 'Incidents By Chapter Nov 2019 - Sep 2024'
  plot_main_title <- ''
  plot_save_name <- paste0(analysis_plots_dir,'incidents_by_chapter.pdf')
  
  g <- grid.arrange(arrangeGrob(grobs = all_plots,   ncol=3 ),
                    
                    top = textGrob(plot_main_title,gp=gpar(fontsize=30,fontface = 'bold')))
  
  
  print(plot_save_name)
  ggsave(file=plot_save_name, g, height=15, width=15) #saves g
}

do_section()
do_para()
do_chapter()
