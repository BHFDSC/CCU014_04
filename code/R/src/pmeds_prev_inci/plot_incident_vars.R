
dt_chapters <- get_chapter_colors()
years = c(2019,2020,2021,2022,2023,2024)
plot_dates =  c('2019-11-01','2024-09-30')
plot_dates_limits =  c(as.Date('2019-11-01', format = "%Y-%m-%d"),
                       as.Date('2024-09-30', format = "%Y-%m-%d"))

data_incident_filter <- function(dt){
  
  dt <- dt[year %in% years ]
  dt <- add_date_col(dt)
  dt <- dt[date %between% plot_dates]
  
  dt <- dt_swap_bnf_chapters(dt_chapters,dt)
  
  dt
}

data_chapters <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_prescribed_chapter_num_inci_linked.csv')
  print(fp_data)
  
  dt <-fread(fp_data)
  
  dt <- data_incident_filter(dt)
  
  
}

data_section <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_prescribed_section_code_inci_linked.csv')
  print(fp_data)
  
  dt <-fread(fp_data, colClasses = c(prescribed_section_code = "character"))
  dt$prescribed_chapter_num <- as.integer(
    substring(dt$prescribed_section_code,1,2))
  
  dt <- data_incident_filter(dt)
  
  dt_map <- bnf_sections_list()
  dt <- merge (x=dt, y=dt_map, by='prescribed_section_code', 
               all.x = T)
  
  dt <- dt[,prescribed_section_code := NULL]
  
  setnames(dt,'BNF_Section','prescribed_section_code')
  
  dt
  
}

data_para <- function(){
  
  fp_data <- paste0(data_root_dir,'pmeds_prescribed_para_code_inci_linked.csv')
  print(fp_data)
  
  dt <-fread(fp_data, colClasses = c(prescribed_para_code = "character"))
  dt$prescribed_chapter_num <- as.integer(
    substring(dt$prescribed_para_code,1,2))
  
  dt <- data_incident_filter(dt)
  
  dt_map <-bnf_para_list()
  
  dt <- merge (x=dt, y=dt_map, by='prescribed_para_code', 
               all.x = T)
  
  dt <- dt[,prescribed_para_code := NULL]
  
  setnames(dt,'BNF_Paragraph','prescribed_para_code')
  
  dt
  
}


get_inci_plot_by_chapter <- function(dt,group_col_name,chaps_seq,legend_pos = 'bottom'){
  
  
  group_col = sym(group_col_name)
  
  all_plots <- list()
  
  for(c_id in chaps_seq){
    #  stuff, such as
    
    dt_c <- dt[ dt$prescribed_chapter_num == c_id]
    cc <- unique(dt_c$chapter_color)
    plot_title <- unique(dt_c$chapter_text)
    
    g <- ggplot(dt_c, aes(x=date, y=count_pop, 
                          group=!!group_col
    )) + 
      geom_line(aes(colour=!!group_col)) +
      scale_x_date(
        date_labels="%b %y",
        date_breaks  ="1 month",
        limits= plot_dates_limits,
        expand = c(0,0)
      ) +
      scale_y_continuous(labels = addUnits) +
      geom_vline(xintercept=as.numeric(covid_dates), colour="grey")+
      #scale_color_manual( name="chapter", values=cc) +
      theme_bw() +
      theme(legend.position = legend_pos, 
            legend.title = element_blank(),
            
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = paste0(plot_title), y = "Incident Count", x = "Date") +
      guides(colour=guide_legend(ncol =2))
    
    all_plots[[(length(all_plots) +1)]] <- g
    
  }
  
  all_plots
}
