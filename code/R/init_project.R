source('./load_libraries.R')

output_data_timestamp = ''
output_analysis_timestamp = ''


### project tables
table_pmeds_prev <- ''
table_pmeds_inci_cn <- ''
table_pmeds_inci_section <- ''
table_pmeds_inci_para <- ''
table_pmeds_inci_subpara <- ''
table_pmeds_inci_cs <- ''

table_gdppr <- ''
table_skinny <- ''

table_poly_counts <- ''
table_poly_counts_month <- ''

covid_dates = c(as.Date("2020-03-23"),as.Date("2020-10-31"),as.Date("2021-01-06"))
age_bands = c('0','1-4','5-9','10-14','15-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90+')



##create dirs if not exists !! IN ORDER
data_root_dir <- paste0(getwd(),'/data/')
analysis_root_dir <- paste0(getwd(),'/analysis/')
print(analysis_root_dir)

if(!dir.exists(data_root_dir)) {dir.create(data_root_dir)}
if(!dir.exists(analysis_root_dir)) {dir.create(analysis_root_dir)}

data_root_dir = paste0(data_root_dir,output_data_timestamp,'/')
analysis_root_dir = paste0(analysis_root_dir,output_analysis_timestamp,'/')

print(analysis_root_dir)

if(!dir.exists(data_root_dir)) {dir.create(data_root_dir)}
if(!dir.exists(analysis_root_dir)) {dir.create(analysis_root_dir)}

analysis_data_dir = paste0(analysis_root_dir,'data/')
analysis_plots_dir = paste0(analysis_root_dir,'plots/')
print(analysis_data_dir)
print(analysis_plots_dir)

if(!dir.exists(analysis_data_dir)) {dir.create(analysis_data_dir)}
if(!dir.exists(analysis_plots_dir)) {dir.create(analysis_plots_dir)}

data_lists_dir <- paste0(getwd(),'/data/')

helpers_path <- "./src/helpers/"
for (f in list.files(path=helpers_path,pattern="*.R")) {
  source(paste0(helpers_path,f))
  print(paste0(helpers_path,f))
}

