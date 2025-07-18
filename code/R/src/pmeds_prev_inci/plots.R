source('./src/pmeds_prev_inci/stack_functions.R')

prev_cols <- list(c('sex','sex','pmeds_age_chapter_sex'),
                  c('imd_quintile','imd_quintile','pmeds_age_chapter_imd_quintile'),
                  c('imd_deci','imd_deci','pmeds_age_chapter_imd_deci'),
                  c('ethnicity_description','ethnicity_description','pmeds_age_chapter_ethnic')
                  
)

years = c(2023)
keep_ages <- c(0,112)
keep_ages_rates <- c(0,75)
dt_chapters <- get_chapter_colors()
prefix_type <- 'prev'
g_ncols = 5


for(c in prev_cols){
  for(which_year in years){
    ## prev linked
    
    fp_data <- paste0(data_root_dir,c[3],'_prev_linked.csv')
    fp_data_rates <- paste0(data_root_dir,c[3],'_prev_linked_rates_',which_year,'.csv')
    
    dt <- fread(fp_data)
    dt_rates <- fread(fp_data_rates)
    
    dt <- dt[year == which_year & age %between% keep_ages]
    dt <- dt[!is.na(strat_value)]
    
    dt_rates <- dt_rates[!is.na(strat_value)]
    
    if (c[1] == 'ethnicity_description'){
      dt <- ethnic_desc_collapse(dt,'strat_value')
      dt <- dt[, .(count=sum(count),
                   count_pop=sum(count_pop)), by=list(year,age,
                                                      strat_value,
                                                      prescribed_chapter_num)]
      
      dt_rates <- ethnic_desc_collapse(dt_rates,'strat_value')
      dt_rates <- dt_rates[, .(rate_value=sum(rate_value)), by=list(age,
                                                                    strat_value)]
      
    }
    
    
    
    
    
    dt_temp <- merge(x=dt,y=dt_rates, by =c('strat_value', 'age'),all.x=T)
    dt_temp$rate <- (dt_temp$count/dt_temp$rate_value)*1
    
    dt_temp <- dt_swap_bnf_chapters(dt_chapters,dt_temp)
    
    col_name = 'count'
    dt_sum <- dt_temp[,.(count = sum(get(col_name))),.(year,age,strat_value)]
    max_y_value <- max(dt_sum$count) + 1000
    #print(max_y)
    
    strat_value_order = F
    colors <- gradient_colors_5()
    
    if(c[1] == 'sex'){
      
      g_ncols =  2
      dt_temp <- dt_temp[ , strat_value := c("Unknown", "Male", "Female")[match(strat_value, c(0,1,2))] ]
      strat_value_order = T
      colors <- sex_colours
      
    } else if (c[1] == 'ethnicity_description'){
      g_ncols =  5
      max_y_value <- 0
      colors <- palette_one()
      
    }else if (c[1] == 'imd_deci'){
      g_ncols =  5
      colors <- gradient_colors()
      
    }else{
      g_ncols =  3
    }
    
    strat_values_unique <- sort(unique(dt_temp$strat_value),decreasing =strat_value_order)
    
    fn <- paste0(prefix_type,'_age_chapter_stacked_',c[1],'_',which_year)
    
    data_save_name <- paste0(analysis_data_dir,fn,'.csv')
    #print(data_save_name)
    save_dt_to_csv(dt_temp,data_save_name)
    
    plot_save_name <- paste0(analysis_plots_dir,fn,'.pdf')
    print(plot_save_name)
    plot_and_save_stacked(dt_temp,strat_values_unique,
                          plot_save_name,keep_ages,
                          max_y_value,g_ncols=g_ncols)
    
    
    plot_save_name <- paste0(analysis_plots_dir,fn,'_rates.pdf')
    print(plot_save_name)
    dt_temp <- dt_temp[dt_temp$age %between% keep_ages_rates]
    plot_rates_by_chapter(dt_temp,which_year,keep_ages_rates,
                          plot_save_name,colors,
                          plot_main_title=" ",
                          legend_title='')
    
    
    
  }
  
}

