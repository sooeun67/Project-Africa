# load required packages
library(WDI)
library(countrycode)
library(rworldmap)
library(ggplot2)
library(tidyr)
library(forecast)
library(dplyr)
library(DataExplorer)   # for EDA
library(dlookr)
library(naniar)

##################################### Share of GDP on Infrastructure  #######################################
# Load data for all countries
raw <- WDI(country = 'all', indicator = 'NV.IND.TOTL.ZS', 
           start = 1990, end = 2018, extra = TRUE, cache = NULL)
names(raw)
head(raw)
plot_str(raw)
plot_scatterplot(raw, "NV.IND.TOTL.ZS", size = 1)

# Keep only iso3c, country, and number of beds
new_raw <- subset(raw, select = c('year', 'NV.IND.TOTL.ZS', 'iso3c', 'country'))

names(new_raw)
head(new_raw)
NROW(new_raw)

# Keep the sub-saharan africa
#['Angola', 'Burundi', 'Benin', 'Burkina Faso', 'Botswana', 
#  'Central African Republic', 'Cameroon', 'Dem. Rep. Congo', "Cote d'Ivoire", 
#  'Congo', 'Djibouti', 'Eritrea', 'Ethiopia', 'Gabon',
#  'Ghana', 'Guinea', 'Guinea-Bissau', 'Kenya', 'Madagascar', 'Mali', 
#  'Mozambique', 'Mauritania', 'Mauritius',
#  'Malawi', 'Namibia', 'Niger', 'Nigeria', 'Rwanda', 'Senegal', 
#  'Sierra Leone', 'Swaziland', 'Chad', 'Togo', 'Tanzania', 'Uganda',
#  'South Africa', 'Zambia', 'Zimbabwe']

# 
africa <- new_raw[new_raw$iso3c == 'AGO' | new_raw$iso3c == 'BDI' | new_raw$iso3c == 'BEN' | new_raw$iso3c == 'BFA' | 
                    new_raw$iso3c == 'BWA' | new_raw$iso3c == 'CAF' | new_raw$iso3c == 'CMR' | new_raw$iso3c == 'COD' | 
                    new_raw$iso3c == 'CIV' | new_raw$iso3c == 'COG' | new_raw$iso3c == 'DJI' | new_raw$iso3c == 'ERI' | 
                    new_raw$iso3c == 'EST' | new_raw$iso3c == 'GAB' | new_raw$iso3c == 'GHA' | new_raw$iso3c == 'GIN' | 
                    new_raw$iso3c == 'GNB' | new_raw$iso3c == 'KEN' | new_raw$iso3c == 'MDG' | new_raw$iso3c == 'MLI' | 
                    new_raw$iso3c == 'MOZ' | new_raw$iso3c == 'MRT' | new_raw$iso3c == 'MWI' | new_raw$iso3c == 'NAM' | 
                    new_raw$iso3c == 'NER' | new_raw$iso3c == 'NGA' | new_raw$iso3c == 'RWA' | new_raw$iso3c == 'SEN' | 
                    new_raw$iso3c == 'SLE' | new_raw$iso3c == 'TCD' | new_raw$iso3c == 'TGO' | new_raw$iso3c == 'TZA' |
                    new_raw$iso3c == 'UGA' | new_raw$iso3c == 'ZAF' | new_raw$iso3c == 'ZMB' | new_raw$iso3c == 'ZWE', ]

# africa <- new_raw[new_raw$iso3c %in% c('AGO', 'BDI', 'BEN'), ]
# advanced indexing 
africa <- dplyr::filter(africa, !is.na(iso3c))

# Find top 5 countries based on their mean
ommited_africa = na.omit(africa)
ommited_africa = aggregate(ommited_africa$NV.IND.TOTL.ZS, list(country=ommited_africa$country), mean)
sorted <- ommited_africa[order(-ommited_africa$x),]
sorted[c(1,2,3,4,5),]$country

# Find out Top 5 Country and create new dataframe
#top_10_africa <- africa[africa$iso3c %in% c('AGO', 'COD', 'GAB', 'BWA', 'NGA', 'ZMB', 'GNB', 'COG', 
#                                            'MRT', 'ZAF'), ]
top_5_africa <- africa[africa$iso3c %in% c('COD', 'GAB', 'BWA', 'NGA', 'ZMB'), ]

######################## Make a plot of Top 5 countries ############################################

ggplot(top_5_africa, aes(x = year, y = NV.IND.TOTL.ZS, colour = iso3c)) +
  geom_line( size = 1) +
  theme(plot.title = element_text(hjust = 0.5)) +    # Center title
  labs(title = "Share of GDP on Infrastructure, total (%)", 
       subtitle = "Top 5 Countries Based on the Share of GDP on Infrastructure",
       x = "Year",
       y = "Share of GDP on Infrastructure [%]",
       fill = "Region") +
  scale_colour_hue(labels=c('Congo, Rep.', 'Gabon', 'Botswana', 'Nigeria', 'Zambia'), name = " ") +       # Change legend name
  theme(legend.background = element_rect( fill = 'grey100', size = 1)) +   # Decorate legend
  theme(text = element_text(size = 12))


                
