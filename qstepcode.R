#installing and loading the needed libraries
install.packages(c("plm", "psych", "zoo", "stargazer", "Hmisc", "ggplot2", "GPArotation"))
library("plm")
library("psych") 
library("zoo")
library("stargazer")
library("Hmisc")
library("GPArotation")
library("ggplot2")

#uploading Polity IV 2018 Dataset
p4v2018 <- read_excel("./p4v2018.xls")

#Choosing the democracies with polity2 score of more than 5 over the 1999-2018 period
p4v2018 <- p4v2018[c("country", "year", "polity2")]
no_continuous_democracy = subset(p4v2018, (year>1998 & polity2<6))
p4_democratic_countries <- subset(p4v2018, !(is.element(country, no_continuous_democracy$country)) & year>1998)

#I proceed to remove the countries that are too young to have 20 years of continuous democracy, but were not undemocratic between 1999-2018 (Kosovo, Montenegro, Timor Leste, Serbia, Serbia and Montenegro, East Timor) or there isn't polity2 data for some data points (Bosnia and Lebanon)
p4_democratic_countries <- subset(p4_democratic_countries, !(is.element(country, "Kosovo") | is.element(country, "Montenegro") | is.element(country, "Timor Leste") | is.element(country, "Serbia") | is.element(country, "Serbia and Montenegro")| is.element(country, "East Timor") | is.element(country, "Bosnia") | is.element(country, "Lebanon"))) 

#Encoding as a factor
p4_democratic_countries$country <- factor(p4_democratic_countries$country)

#Creating a vector with 65 democracies that fulfill the criteria:
unique(p4_democratic_countries$countryname)

#I upload the datasets: QoG Basic Time Series 2020 Dataset, QoG Standard Time Series 2020 Dataset, Cabinet Data, Database of Political Institutions
qog_bas <- read.csv("./qog_bas_ts_20.csv")
qog_std <- read.csv("./qog_std_ts_20.csv.crdownload")
cabinet_data <- read_excel("./cabinet_data.xlsx")
dpi_data <- read_excel("./DPI2017_basefile_Jan2018.xlsx")

#I created Cabinet Data panel dataset in Excel. I classified 65 democracies as consensual or majoritarian on the basis of their cabinet. I used Lijphart’s classification. The dataset contains all the data points for 1999-2017 for every country. I used multiple electoral datasets and websites to compile cabinet data where the data was missing
cabinet_data_2017 <- subset(cabinet_data, cabinet_data$year == 2017) cabinet_data_2017

#Creating reduced QoG Standard Dataset
independent_variables = c("gol_enpp", "iaep_constlam", "gtm_unit")
control_variables=c("undp_hdi", "wdi_pop")
dependent_economic=c("ffp_ued", "wdi_gini", "lis_gini", "wdi_incsh10h", "wdi_incsh10l", "wdi_incsh20h", "wdi_incsh20l")
dependent_gender= c("bl_asyf", "bl_lhf", "bl_lpf", "bl_lsf", "bl_luf", "vdem_gender", "ipu_l_sw", "ipu_u_sw", "ciri_wecon", "ciri_wopol", "ciri_wosoc", "gii_gii", "wwbi_fsprpemp", "cai_cai1")
dependent_ethnic = c("iaep_ebbp", "bti_ci", "bti_eo", "ciri_polpris")
qog_std_reduced <- qog_std[, c("cname", "year", independent_variables, control_variables, dependent_economic, dependent_gender, dependent_ethnic, "fe_etfra")]

#Creating reduced QoG Basic Dataset
qog_bas_reduced <- qog_bas[, c("cname", "year", "cbi_cbiw")]

#Creating reduced DPI_data dataset
dpi_data_reduced <- dpi_data[, c("countryname", "year", "totalseats", "gov1seat", "gov1vote", "gov2seat", "gov2vote", "gov3seat", "gov3vote", "govothst", "govothvt", "opp1seat", "opp1vote", "opp2seat", "opp2vote", "opp3seat", "opp3vote", "oppothst", "oppothvt")]

#Renaming the variable names so that they match
colnames(qog_std_reduced)[colnames(qog_std_reduced) == "cname"] <- "countryname"
colnames(qog_bas_reduced)[colnames(qog_bas_reduced) == "cname"] <- "countryname"
colnames(p4_democratic_countries)[colnames(p4_democratic_countries) == "country"] <- "countryname" 
colnames(cabinet_data)[colnames(cabinet_data) == "Year"] <- "year"

#Renaming DPI and cabinet_data datasets so that the country names match
cabinet_data$countryname <- gsub("C. Verde Is.", "Cape Verde", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("C. Verde Is.", "Cape Verde", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("Cyprus", "Cyprus (1975-)", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("Cyprus", "Cyprus (1975-)", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("Czech Rep.", "Czech Republic", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("Czech Rep.", "Czech Republic", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("Dom. Rep.", "Dominican Republic", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("Dom. Rep.", "Dominican Republic", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("France", "France (1963-)", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("France", "France (1963-)", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("FRG/Germany", "Germany", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("FRG/Germany", "Germany", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("Macedonia", "North Macedonia", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("Macedonia", "North Macedonia", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("ROK", "Korea, South", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("ROK", "Korea, South", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("S. Africa", "South Africa", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("S. Africa", "South Africa", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("Trinidad-Tobago", "Trinidad and Tobago", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("Trinidad-Tobago", "Trinidad and Tobago", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("UK", "United Kingdom", cabinet_data$countryname)
dpi_data_reduced$countryname <- gsub("UK", "United Kingdom", dpi_data_reduced$countryname)
dpi_data_reduced$countryname <- gsub("USA", "United States", dpi_data_reduced$countryname)
cabinet_data$countryname <- gsub("USA", "United States", cabinet_data$countryname)

#Renaming the p4_democratic_countries dataset
p4_democratic_countries$countryname <- gsub("Cyprus", "Cyprus (1975-)", p4_democratic_countries$countryname)
p4_democratic_countries$countryname <- gsub("France", "France (1963-)", p4_democratic_countries$countryname)
p4_democratic_countries$countryname <- gsub("Korea South", "Korea, South", p4_democratic_countries$countryname)
p4_democratic_countries$countryname <- gsub("Macedonia", "North Macedonia", p4_democratic_countries$countryname)
p4_democratic_countries$countryname <- gsub("Slovak Republic", "Slovakia", p4_democratic_countries$countryname)

#Conducting the sanity check and merging the datasets
stopifnot(levels(qog_bas_reduced$countryname) == levels(qog_std_reduced$countryname))
panel1 <- merge(qog_bas_reduced, qog_std_reduced, by=c("countryname", "year"))
stopifnot(levels(cabinet_data$countryname) == levels(panel1$countryname))
panel2 <- merge(panel1, cabinet_data, by=c("countryname", "year"))
stopifnot(levels(panel2$countryname) == levels(dpi_data_reduced$countryname))
panel3 <- merge(panel2, dpi_data_reduced, by=c("countryname", "year"))

#Restricting the panel to years 1999 to 2018 (by merging with p4v2018), so that there aren't any biases
stopifnot(levels(panel3$countryname) == levels(p4_democratic_countries$countryname))
panel_data <- merge(panel3, p4_democratic_countries, by=c("countryname", "year"))

#Creating a vector of unique country names for extrapolation and interpolation functions
country_name <- unique(panel_data$countryname)

#Here, I extrapolate and interpolate the missing data to ensure the balance of the dataset. Additionally, I calculate or insert the missing data manually. I use functions that ensure minimal validity problems

#Extrapolating Years Since Last Constitutional Amendment for years 1999-2012 for Bolivia, Chile, Hungary, Israel, New Zealand, Portugal, UK and Uruguay
panel_data$iaep_constlam[panel_data$countryname == country_name[4] & is.na(panel_data$iaep_constlam) & panel_data$year < 2013]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[4] & panel_data$year < 2013], panel_data$iaep_constlam[panel_data$countryname == country_name[4] & panel_data$year < 2013], xout = subset(panel_data$year, panel_data$countryname == country_name[4] & is.na(panel_data$iaep_constlam) & panel_data$year < 2013), method = "linear", na.rm = FALSE)$y

#Extrapolating Federalism/Bicameralism Index
for(i in 1:65) { 
  panel_data$gtm_unit[panel_data$countryname == country_name[i] & is.na(panel_data$gtm_unit) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$gtm_unit[panel_data$countryname == country_name[i] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$gtm_unit) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}

#Calculate Effective Number of Parliamentary Parties for Botswana, Guyana, Namibia and South Africa 1999-2018. Calculate for 1999 in Mexico
for(i in 1:1300)
{ if(is.na(panel_data$gol_enpp[i])) {panel_data$gol_enpp[i] <- (1/((panel_data$gov1seat[i]/panel_data$totalseats[i])^2 + (panel_data$gov2seat[i]/panel_data$totalseats[i])^2 + (panel_data$gov3seat[i]/panel_data$totalseats[i])^2 + (panel_data$govothst[i]/panel_data$totalseats[i])^2 + (panel_data$opp1seat[i]/panel_data$totalseats[i])^2 + (panel_data$opp2seat[i]/panel_data$totalseats[i])^2 + (panel_data$opp3seat[i]/panel_data$totalseats[i])^2 + (panel_data$oppothst[i]/panel_data$totalseats[i])^2))}
}

#Extrapolating CBI index for Cape Verde, Cyprus and Mauritius for 1999-2012
for(i in c(11,15,39)) {
  panel_data$cbi_cbiw[panel_data$countryname == country_name[i] & is.na(panel_data$cbi_cbiw) & panel_data$year < 2013]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2013], panel_data$cbi_cbiw[panel_data$countryname == country_name[i] & panel_data$year < 2013], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$cbi_cbiw) & panel_data$year < 2013), method = "linear", na.rm = FALSE)$y
}

#Extrapolating HDI for North Macedonia and Cape Verde in 1999, and for 2018 in all countries but Taiwan
for(i in c(1:60, 62:65)) {
  panel_data$undp_hdi[panel_data$countryname == country_name[i] & is.na(panel_data$undp_hdi) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$undp_hdi[panel_data$countryname == country_name[i] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$undp_hdi) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}

#Inserting HDI values for Taiwan manually, using the data from the Statistical Bureau of the Republic of China
panel_data$undp_hdi[panel_data$countryname == 'Taiwan'] <- c(0.881,0.890,0.894, 0.902, 0.909, 0.917, 0.854, 0.859, 0.868, 0.868, 0.871, 0.873, 0.874, 0.879, 0.882, 0.882, 0.885, 0.903, 0.907, 0.911)

#Inserting Population values for Taiwan manually
panel_data$wdi_pop[panel_data$countryname == 'Taiwan'] <- c(21742815, 21928591, 22092387, 22276672, 22405568, 22520776, 22604550, 22689122, 22770383, 22876527, 22958360, 23037031, 23119772, 23162123, 23224912, 23315822, 23373517, 23433753, 23492074, 23539816)

#Interpolating and Extrapolating Income Shares held by proportions of population in for all countries but Guyana, Japan, New Zealand, Taiwan and Trinidad and Tobago for years 1999-2018
for(i in c(1:25,27:33,35:44,46:60,63:65)) {
  panel_data$wdi_incsh10h[panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh10h) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$wdi_incsh10h[panel_data$countryname == country_name[i] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh10h) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:25,27:33,35:44,46:60,63:65)) {
  panel_data$wdi_incsh10l[panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh10l) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$wdi_incsh10l[panel_data$countryname == country_name[i] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh10l) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:25,27:33,35:44,46:60,63:65)) {
  panel_data$wdi_incsh20h[panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh20h) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$wdi_incsh20h[panel_data$countryname == country_name[i] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh20h) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:25,27:33,35:44,46:60,63:65)) {
  panel_data$wdi_incsh20l[panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh20l) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$wdi_incsh20l[panel_data$countryname == country_name[i] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$wdi_incsh20l) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}

#Interpolating the data for Average Schooling Years of Women between 2000-2010 in every country but Cape Verde and North Macedonia
for(i in c(1:10,12:46,48:65)) {
  panel_data$bl_asyf[panel_data$countryname == country_name[i] & is.na(panel_data$bl_asyf) & panel_data$year < 2011 & panel_data$year > 1999]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], panel_data$bl_asyf[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$bl_asyf) & panel_data$year < 2011 & panel_data$year > 1999), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:10,12:46,48:65)) {
  panel_data$bl_lhf[panel_data$countryname == country_name[i] & is.na(panel_data$bl_lhf) & panel_data$year < 2011 & panel_data$year > 1999]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], panel_data$bl_lhf[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$bl_lhf) & panel_data$year < 2011 & panel_data$year > 1999), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:10,12:46,48:65)) {
  panel_data$bl_lpf[panel_data$countryname == country_name[i] & is.na(panel_data$bl_lpf) & panel_data$year < 2011 & panel_data$year > 1999]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], panel_data$bl_lpf[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$bl_lpf) & panel_data$year < 2011 & panel_data$year > 1999), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:10,12:46,48:65)) {
  panel_data$bllsf[panel_data$countryname == country_name[i] & is.na(panel_data$bllsf) & panel_data$year < 2011 & panel_data$year > 1999]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], panel_data$bllsf[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$bllsf) & panel_data$year < 2011 & panel_data$year > 1999), method = "linear", na.rm = FALSE)$y
}
for(i in c(1:10,12:46,48:65)) {
  panel_data$bl_luf[panel_data$countryname == country_name[i] & is.na(panel_data$bl_luf) & panel_data$year < 2011 & panel_data$year > 1999]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], panel_data$bl_luf[panel_data$countryname == country_name[i] & panel_data$year < 2011 & panel_data$year > 1999], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$bl_luf) & panel_data$year < 2011 & panel_data$year > 1999), method = "linear", na.rm = FALSE)$y
}

#Extrapolating Share of Women in Lower House for Bolivia, Chile, Colombia, Guyana, Moldova, Paraguay and Taiwan. Interpolating for Botswana, Costa Rica, Indonesia, Jamaica, Panama and Trinidad and Tobago
for(i in c(1:10,12:46,48:65)) {
  panel_data$ipu_l_sw[panel_data$countryname == country_name[i] & is.na(panel_data$ipu_l_sw) & panel_data$year < 2019]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019], panel_data$ipu_l_sw[panel_data$countryname == country_name[4] & panel_data$year < 2019], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$ipu_l_sw) & panel_data$year < 2019), method = "linear", na.rm = FALSE)$y
}

#Women’s Economic Rights and Women’s Political Rights - extrapolating for Cape Verde and US, interpolating for Cape Verde, US and Namibia respectively
for(i in c(11, 43, 64)) {
  panel_data$ciri_wecon[panel_data$countryname == country_name[i] & is.na(panel_data$ciri_wecon) & panel_data$year < 2018]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2018], panel_data$ciri_wecon[panel_data$countryname == country_name[i] & panel_data$year < 2018], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$ciri_wecon) & panel_data$year < 2018), method = "linear", na.rm = FALSE)$y
}
for(i in c(11, 43, 64)) {
  panel_data$ciri_wopol[panel_data$countryname == country_name[i] & is.na(panel_data$ciri_wopol) & panel_data$year < 2018]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2018], panel_data$ciri_wopol[panel_data$countryname == country_name[i] & panel_data$year < 2018], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$ciri_wopol) & panel_data$year < 2018), method = "linear", na.rm = FALSE)$y
}

#Gender Inequality Index - interpolating for years 2000-2018 all countries but Taiwan, North Macedonia and Cape Verde, extrapolating Canada, Czech Republic, India, Romania, Slovenia, Trinidad and Tobago and US in 2000-2004
for(i in c(1:10,12:45,47:60,62:65)) {
  panel_data$gii_gii[panel_data$countryname == country_name[i] & is.na(panel_data$gii_gii) & panel_data$year < 2019 & panel_data$year > 1999]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2019 & panel_data$year > 1999], panel_data$gii_gii[panel_data$countryname == country_name[i] & panel_data$year < 2019 & panel_data$year > 1999], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$gii_gii) & panel_data$year < 2019 & panel_data$year > 1999), method = "linear", na.rm = FALSE)$y
}

#Extrapolating Ethnicity Based Banning of Parties for 2012 in Bolivia, Chile, Uruguay
for(i in c(6,12,65)) {
  panel_data$iaep_ebbp[panel_data$countryname == country_name[i] & is.na(panel_data$iaep_ebbp) & panel_data$year < 2013]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[i] & panel_data$year < 2013], panel_data$iaep_ebbp[panel_data$countryname == country_name[i] & panel_data$year < 2013], xout = subset(panel_data$year, panel_data$countryname == country_name[i] & is.na(panel_data$iaep_ebbp) & panel_data$year < 2013), method = "linear", na.rm = FALSE)$y
}

#Extrapolate Political Imprisonment for Cape Verde in 1999-2000
panel_data$ciri_polpris[panel_data$countryname == country_name[11] & is.na(panel_data$ciri_polpris) & panel_data$year < 2003 & panel_data$year > 1998]<- approxExtrap(panel_data$year[panel_data$countryname == country_name[11] & panel_data$year < 2003 & panel_data$year > 1998], panel_data$ciri_polpris[panel_data$countryname == country_name[11] & panel_data$year < 2003 & panel_data$year > 1998], xout = subset(panel_data$year, panel_data$countryname == country_name[11] & is.na(panel_data$ciri_polpris) & panel_data$year < 2003 & panel_data$year > 1998), method = "linear", na.rm = FALSE)$y

#Calculate Gallagher Index, but when any value is NA, then I don’t calculate it. This ensures that the results are not influenced by false data
if (panel_data$gov1seat!=0 && panel_data$gov1seat!=0 && !is.na(panel_data$gov1vote) && !is.na(panel_data$gov2vote) && !is.na(panel_data$gov1vote) && !is.na(panel_data$gov3vote) && !is.na(panel_data$govothvt) && !is.na(panel_data$opp1vote) &&
    !is.na(panel_data$opp2vote) && !is.na(panel_data$opp3vote) && !is.na(panel_data$oppothvt) && !is.na(panel_data$gov1seat) && !is.na(panel_data$gov2seat) && !is.na(panel_data$gov3seat) && !is.na(panel_data$govothst) && !is.na(panel_data$opp1seat) && !is.na(panel_data$opp2seat) && !is.na(panel_data$opp3seat) && !is.na(panel_data$oppothst) && !is.na(panel_data$totalseats) ) {
  panel_data$disp_gal <- sqrt(((panel_data$gov1vote-(panel_data$gov1seat/panel_data$totalseats))^2 + 
                                 (panel_data$gov2vote-(panel_data$gov2seat/panel_data$totalseats))^2 + 
                                 (panel_data$gov3vote-(panel_data$gov3seat/panel_data$totalseats))^2 + 
                                 (panel_data$govothvt-(panel_data$govothst/panel_data$totalseats))^2 + 
                                 (panel_data$opp1vote-(panel_data$opp1seat/panel_data$totalseats))^2 + 
                                 (panel_data$opp2vote-(panel_data$opp2seat/panel_data$totalseats))^2 + 
                                 (panel_data$opp3vote-(panel_data$opp3seat/panel_data$totalseats))^2 + 
                                 (panel_data$oppothvt-(panel_data$oppothst/panel_data$totalseats))^2)*0.5)
  
} else {
  panel_data$disp_gal <- NA
}

#Create 10/10 and 20/20 ratios, noting the NAs
if(!is.na(panel_data$wdi_incsh10h && !is.na(panel_data$wdi_incsh10l))) {
  panel_data$ratio10_10 <- panel_data$wdi_incsh10h / panel_data$wdi_incsh10l
} else { panel_data$ratio10_10 <- NA}
if(!is.na(panel_data$wdi_incsh20h && !is.na(panel_data$wdi_incsh20l))) {
  panel_data$ratio20_20 <- panel_data$wdi_incsh20h / panel_data$wdi_incsh20l
} else { panel_data$ratio20_20 <- NA}

#Here I construct my independent variables, run tests and panel regressions

#Reversing the directions of Gallagher Index and Federalism/Bicameralism Index. The bigger the value of every independent variable, the more consensual the democracy is
panel_data$disp_gal <- 100 - panel_data$disp_gal
panel_data$gtm_unit <- 2 - panel_data$gtm_unit

#Creating Executives-Parties Dimension
exp_dim <- panel_data[c("gol_enpp", "disp_gal", "cons1_maj0")]

#Scaling the independent variables, so that PCA is not flawed by disproportional variances
panel_data$gtm_unit2 <- scale(panel_data$gtm_unit)
panel_data$cbi_cbiw2 <- scale(panel_data$cbi_cbiw)
panel_data$iaep_constlam2 <- scale(panel_data$iaep_constlam)

#Creating Federal-Unitary Dimension
fun_dim <- panel_data[c("gtm_unit2", "iaep_constlam2", "cbi_cbiw2")]

#Running internal consistency tests
summary(omega(scale(exp_dim)))
summary(omega(fun_dim))

#Executives-Parties Index consistent enough. Federal-Unitary Index internally inconsistent. Nonetheless, since Lijphart uses the variables for the index, I use them too

#Running the PCA. Scaling Executives-Parties Dimension variables.
Executives_Parties_PCA <- prcomp(!is.na(exp_dim), center = TRUE, scale. = TRUE)
Federal_Unitary_PCA <- prcomp(!is.na(fun_dim))
screeplot(Executives_Parties_PCA)
screeplot(Federal_Unitary_PCA)
summary(Executives_Parties_PCA)
summary(Federal_Unitary_PCA)

#For Executives-Parties Dimension two components explain 99% of variance, while 1 only 84%. I decide on 2 components. This is why two components can be seen in regression tables. For Federal-Unitary Dimension one component explains 95% of variance. One component is enough

#I construct a panel of Lijphart’s 36 countries, which meet my criteria. In total, 32 countries 
panel_lij <- subset(panel_data, countryname == 'Argentina' | countryname == 'Australia'| countryname == 'Austria' | countryname == 'Belgium' | countryname == 'Botswana' | countryname == 'Canada' | countryname == 'Costa Rica' | countryname == 'Denmark' | countryname == 'Finland' | countryname == 'France (1963-)' | countryname == 'Germany' | countryname == 'Greece' | countryname == 'India' | countryname == 'Ireland' | countryname == 'Israel' | countryname == 'Italy' | countryname == 'Jamaica' | countryname == 'Japan' | countryname == 'Korea, South' | countryname == 'Luxembourg' | countryname == 'Mauritius' | countryname == 'Netherlands' | countryname == 'Norway' | countryname == 'New Zealand' | countryname == 'Portugal' | countryname == 'Spain' | countryname == 'Sweden' | countryname == 'Switzerland' | countryname == 'Trinidad and Tobago' | countryname == 'United Kingdom' | countryname == 'United States' | countryname == 'Uruguay')

#I construct dimensions for Lijphart’s set of countries
exp_dim_lij <- panel_lij[c("gol_enpp", "disp_gal", "cons1_maj0")]
fun_dim_lij <- panel_lij[c("gtm_unit", "iaep_constlam", "cbi_cbiw")]
exp_dim_lij <- scale(exp_dim_lij)
fun_dim_lij <- scale(fun_dim_lij)


#Logging and scaling the population
panel_data$wdi_pop_log <- scale(log(panel_data$wdi_pop))
panel_lij$wdi_pop_log <- scale(log(panel_lij$wdi_pop))

#Scaling the HDI
panel_data$undp_hdi <- scale(panel_data$undp_hdi)
panel_lij$undp_hdi <- scale(panel_lij$undp_hdi)

#Constructing variables for Principal Components Regression
exp_principal <- principal(exp_dim, nfactors = 2, rotate="none", scores = T)
panel_data$exp <- exp_principal$scores
fun_principal <- principal(fun_dim, nfactors = 1, rotate="none", scores = T)
panel_data$fun <- fun_principal$scores

exp_principal_lij <- principal(exp_dim_lij, nfactors = 2, rotate="none", scores = T)
panel_lij$exp <- exp_principal_lij$scores
fun_principal_lij <- principal(fun_dim_lij, nfactors = 1, rotate="none", scores = T)
panel_lij$fun <- fun_principal_lij$scores 

#Panel regression for Lijphart's 32 democracies - Economic Inequality
lij_gini_exp_baseline <- lm(panel_lij$wdi_gini ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_gini_exp <- plm(panel_lij$wdi_gini ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_gini_fun_baseline <- lm(panel_lij$wdi_gini ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_gini_fun  <- plm(panel_lij$wdi_gini ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_ratio10_exp_baseline  <- lm(panel_lij$ratio10_10 ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ratio10_exp <- plm(panel_lij$ratio10_10 ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_ratio10_fun_baseline <- lm(panel_lij$ratio10_10 ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ratio10_fun  <- plm(panel_lij$ratio10_10 ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_ratio20_exp_baseline <- lm(panel_lij$ratio20_20 ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ratio20_exp<- plm(panel_lij$ratio20_20 ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_ratio20_fun_baseline <- lm(panel_lij$ratio20_20 ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ratio20_fun  <- plm(panel_lij$ratio20_20 ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

#Panel regression for Lijphart's 32 democracies - Gender Inequality
lij_bl_asyf_exp_baseline <- lm(panel_lij$bl_asyf ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_bl_asyf_exp <- plm(panel_lij$bl_asyf ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_bl_asyf_fun_baseline <- lm(panel_lij$bl_asyf ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_bl_asyf_fun  <- plm(panel_lij$bl_asyf ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_vdem_gender_exp_baseline <- lm(panel_lij$vdem_gender ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_vdem_gender_exp <- plm(panel_lij$vdem_gender ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_vdem_gender_fun_baseline <- lm(panel_lij$vdem_gender ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_vdem_gender_fun  <- plm(panel_lij$vdem_gender ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_ipu_l_sw_exp_baseline <- lm(panel_lij$ipu_l_sw ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ipu_l_sw_exp <- plm(panel_lij$ipu_l_sw ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_ipu_l_sw_fun_baseline <- lm(panel_lij$ipu_l_sw ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ipu_l_sw_fun  <- plm(panel_lij$ipu_l_sw ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_ciri_wecon_exp_baseline <- lm(panel_lij$ciri_wecon ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ciri_wecon_exp <- plm(panel_lij$ciri_wecon ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_ciri_wecon_fun_baseline <- lm(panel_lij$ciri_wecon ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ciri_wecon_fun  <- plm(panel_lij$ciri_wecon ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_ciri_wopol_exp_baseline <- lm(panel_lij$ciri_wopol ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ciri_wopol_exp <- plm(panel_lij$ciri_wopol ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_ciri_wopol_fun_baseline <- lm(panel_lij$ciri_wopol ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ciri_wopol_fun  <- plm(panel_lij$ciri_wopol ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_gii_gii_exp_baseline <- lm(panel_lij$gii_gii ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_gii_gii_exp <- plm(panel_lij$gii_gii ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_gii_gii_fun_baseline <- lm(panel_lij$gii_gii ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_gii_gii_fun  <- plm(panel_lij$gii_gii ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_cai_cai1_exp_baseline <- lm(panel_lij$cai_cai1 ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_cai_cai1_exp <- plm(panel_lij$cai_cai1 ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_cai_cai1_fun_baseline <- lm(panel_lij$cai_cai1 ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_cai_cai1_fun  <- plm(panel_lij$cai_cai1 ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")


#Panel regression for Lijphart's 32 democracies - Ethnic Inequality
lij_ciri_polpris_exp_baseline <- lm(panel_lij$ciri_polpris ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ciri_polpris_exp <- plm(panel_lij$ciri_polpris ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_ciri_polpris_fun_baseline <- lm(panel_lij$ciri_polpris ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_ciri_polpris_fun  <- plm(panel_lij$ciri_polpris ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_bti_ci_exp_baseline <- lm(panel_lij$bti_ci ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_bti_ci_exp <- plm(panel_lij$bti_ci ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_bti_ci_fun_baseline <- lm(panel_lij$bti_ci ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_bti_ci_fun  <- plm(panel_lij$bti_ci ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_bti_eo_exp_baseline <- lm(panel_lij$bti_eo  ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_bti_eo_exp <- plm(panel_lij$bti_eo  ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_bti_eo_fun_baseline <- lm(panel_lij$bti_eo  ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_bti_eo_fun  <- plm(panel_lij$bti_eo  ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")

lij_iaep_ebbp_exp_baseline <- lm(panel_lij$iaep_ebbp ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_iaep_ebbp_exp <- plm(panel_lij$iaep_ebbp ~ panel_lij$exp + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")
lij_iaep_ebbp_fun_baseline <- lm(panel_lij$iaep_ebbp ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log)
lij_iaep_ebbp_fun  <- plm(panel_lij$iaep_ebbp ~ panel_lij$fun + panel_lij$undp_hdi + panel_lij$wdi_pop_log, panel_lij, effect="twoways", method="within")



#Panel regression for Economic Inequality variables - baseline and fixed effects
gini_exp_baseline <- lm(panel_data$wdi_gini ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
gini_exp <- plm(panel_data$wdi_gini ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
gini_fun_baseline <- lm(panel_data$wdi_gini ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
gini_fun  <- plm(panel_data$wdi_gini ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

ratio10_exp_baseline  <- lm(panel_data$ratio10_10 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
ratio10_exp <- plm(panel_data$ratio10_10 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
ratio10_fun_baseline <- lm(panel_data$ratio10_10 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
ratio10_fun  <- plm(panel_data$ratio10_10 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

ratio20_exp_baseline <- lm(panel_data$ratio20_20 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
ratio20_exp<- plm(panel_data$ratio20_20 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
ratio20_fun_baseline <- lm(panel_data$ratio20_20 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
ratio20_fun  <- plm(panel_data$ratio20_20 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

#Panel regression for Gender Inequality variables - baseline and fixed effects
bl_asyf_exp_baseline <- lm(panel_data$bl_asyf ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
bl_asyf_exp <- plm(panel_data$bl_asyf ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
bl_asyf_fun_baseline <- lm(panel_data$bl_asyf ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
bl_asyf_fun  <- plm(panel_data$bl_asyf ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

vdem_gender_exp_baseline <- lm(panel_data$vdem_gender ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
vdem_gender_exp <- plm(panel_data$vdem_gender ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
vdem_gender_fun_baseline <- lm(panel_data$vdem_gender ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
vdem_gender_fun  <- plm(panel_data$vdem_gender ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

ipu_l_sw_exp_baseline <- lm(panel_data$ipu_l_sw ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
ipu_l_sw_exp <- plm(panel_data$ipu_l_sw ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
ipu_l_sw_fun_baseline <- lm(panel_data$ipu_l_sw ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
ipu_l_sw_fun  <- plm(panel_data$ipu_l_sw ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

ciri_wecon_exp_baseline <- lm(panel_data$ciri_wecon ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
ciri_wecon_exp <- plm(panel_data$ciri_wecon ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
ciri_wecon_fun_baseline <- lm(panel_data$ciri_wecon ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
ciri_wecon_fun  <- plm(panel_data$ciri_wecon ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

ciri_wopol_exp_baseline <- lm(panel_data$ciri_wopol ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
ciri_wopol_exp <- plm(panel_data$ciri_wopol ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
ciri_wopol_fun_baseline <- lm(panel_data$ciri_wopol ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
ciri_wopol_fun  <- plm(panel_data$ciri_wopol ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

gii_gii_exp_baseline <- lm(panel_data$gii_gii ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
gii_gii_exp <- plm(panel_data$gii_gii ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
gii_gii_fun_baseline <- lm(panel_data$gii_gii ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
gii_gii_fun  <- plm(panel_data$gii_gii ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

cai_cai1_exp_baseline <- lm(panel_data$cai_cai1 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
cai_cai1_exp <- plm(panel_data$cai_cai1 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
cai_cai1_fun_baseline <- lm(panel_data$cai_cai1 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
cai_cai1_fun  <- plm(panel_data$cai_cai1 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")


#Panel regression for Ethnic Inequality variables - baseline and fixed effects
ciri_polpris_exp_baseline <- lm(panel_data$ciri_polpris ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
ciri_polpris_exp <- plm(panel_data$ciri_polpris ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
ciri_polpris_fun_baseline <- lm(panel_data$ciri_polpris ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
ciri_polpris_fun  <- plm(panel_data$ciri_polpris ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

bti_ci_exp_baseline <- lm(panel_data$bti_ci ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
bti_ci_exp <- plm(panel_data$bti_ci ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
bti_ci_fun_baseline <- lm(panel_data$bti_ci ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
bti_ci_fun  <- plm(panel_data$bti_ci ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

bti_eo_exp_baseline <- lm(panel_data$bti_eo  ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
bti_eo_exp <- plm(panel_data$bti_eo  ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
bti_eo_fun_baseline <- lm(panel_data$bti_eo  ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
bti_eo_fun  <- plm(panel_data$bti_eo  ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")

iaep_ebbp_exp_baseline <- lm(panel_data$iaep_ebbp ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log)
iaep_ebbp_exp <- plm(panel_data$iaep_ebbp ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")
iaep_ebbp_fun_baseline <- lm(panel_data$iaep_ebbp ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log)
iaep_ebbp_fun  <- plm(panel_data$iaep_ebbp ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", method="within")


#In this part I construct regression tables for baseline and fixed effects PCR

#Economic Inequality regression tables - Lijphart's 32 democracies
stargazer(lij_gini_exp_baseline, lij_ratio10_exp_baseline, lij_ratio20_exp_baseline, title="Baseline panel regression on Executives-Parties variables for Lijphart’s 32 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Lij_Economic_exp_baseline2.html') 
stargazer(lij_gini_exp, lij_ratio10_exp, lij_ratio20_exp, title="Fixed effects panel regression on Executives-Parties variables for Lijphart’s 32 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Lij_Economic_exp.html') 
stargazer(lij_gini_fun_baseline, lij_ratio10_fun_baseline, lij_ratio20_fun_baseline, title="Baseline panel regression on Federal-Unitary variables for Lijphart’s 32 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Lij_Economic_fun_baseline2.html') 
stargazer(lij_gini_fun, lij_ratio10_fun, lij_ratio20_fun, title="Fixed effects panel regression on Federal-Unitary variables for Lijphart’s 32 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Lij_Economic_fun.html') 

#Gender Inequality regression tables - Lijphart's 32 democracies
stargazer(lij_bl_asyf_exp_baseline, lij_vdem_gender_exp_baseline, lij_ipu_l_sw_exp_baseline, lij_ciri_wecon_exp_baseline, lij_ciri_wopol_exp_baseline, lij_gii_gii_exp_baseline, lij_cai_cai1_exp_baseline, title="Baseline panel regression on Executives-Parties variables for Lijphart’s 32 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Lij_Gender_exp_baseline2.html') 
stargazer(lij_bl_asyf_exp, lij_vdem_gender_exp, lij_ipu_l_sw_exp, lij_ciri_wecon_exp, lij_ciri_wopol_exp, lij_gii_gii_exp, lij_cai_cai1_exp, title="Fixed effects panel regression on Executives-Parties variables for Lijphart’s 32 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Lij_Gender_exp.html') 
stargazer(lij_bl_asyf_fun_baseline, lij_vdem_gender_fun_baseline, lij_ipu_l_sw_fun_baseline, lij_ciri_wecon_fun_baseline, lij_ciri_wopol_fun_baseline, lij_gii_gii_fun_baseline, lij_cai_cai1_fun_baseline, title="Baseline panel regression on Federal-Unitary variables for Lijphart’s 32 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Lij_Gender_fun_baseline2.html') 
stargazer(lij_bl_asyf_fun, lij_vdem_gender_fun, lij_ipu_l_sw_fun, lij_ciri_wecon_fun, lij_ciri_wopol_fun, lij_gii_gii_fun, lij_cai_cai1_fun, title="Fixed effects panel regression on Federal-Unitary variables for Lijphart’s 32 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Lij_Gender_fun.html') 

#Ethnic Inequality regression tables - Lijphart's 32 democracies
stargazer(lij_ciri_polpris_exp_baseline, lij_bti_ci_exp_baseline, lij_bti_eo_exp_baseline, lij_iaep_ebbp_exp_baseline, title="Baseline panel regression on Executives-Parties variables for Lijphart’s 32 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Lij_Ethnic_exp_baseline2.html') 
stargazer(lij_ciri_polpris_exp, lij_bti_ci_exp, lij_bti_eo_exp, lij_iaep_ebbp_exp, title="Fixed effects panel regression on Executives-Parties variables for Lijphart’s 32 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Lij_Ethnic_exp2.html') 
stargazer(lij_ciri_polpris_fun_baseline, lij_bti_ci_fun_baseline, lij_bti_eo_fun_baseline, lij_iaep_ebbp_fun_baseline, title="Baseline panel regression on Federal-Unitary variables for Lijphart’s 32 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Lij_Ethnic_fun_baseline.html') 
stargazer(lij_ciri_polpris_fun, lij_bti_ci_fun, lij_bti_eo_fun, lij_iaep_ebbp_fun, title="Fixed effects panel regression on Federal-Unitary variables for Lijphart’s 32 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Lij_Ethnic_fun.html')

#Economic Inequality regression tables - Extended 65 democracies
stargazer(gini_exp_baseline, ratio10_exp_baseline, ratio20_exp_baseline, title="Baseline panel regression on Executives-Parties variables for 65 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Economic_exp_baseline1.html') 
stargazer(gini_exp, ratio10_exp, ratio20_exp, title="Fixed effects panel regression on Executives-Parties variables for 65 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2",  "HDI", "Logged population"), out='Economic_exp1.html') 
stargazer(gini_fun_baseline, ratio10_fun_baseline, ratio20_fun_baseline, title="Baseline panel regression on Federal-Unitary variables for 65 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Economic_fun_baseline1.html') 
stargazer(gini_fun, ratio10_fun, ratio20_fun, title="Fixed effects panel regression on Federal-Unitary variables for 65 democracies: Economic Inequality", dep.var.labels = c("Gini coefficient", "10/10 ratio", "20/20 ratio"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Economic_fun1.html') 

#Gender Inequality regression tables - Extended 65 democracies
stargazer(bl_asyf_exp_baseline, vdem_gender_exp_baseline, ipu_l_sw_exp_baseline, ciri_wecon_exp_baseline, ciri_wopol_exp_baseline, gii_gii_exp_baseline, cai_cai1_exp_baseline, title="Baseline panel regression on Executives-Parties variables for 65 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Gender_exp_baseline1.html') 
stargazer(bl_asyf_exp, vdem_gender_exp, ipu_l_sw_exp, ciri_wecon_exp, ciri_wopol_exp, gii_gii_exp, cai_cai1_exp, title="Fixed effects panel regression on Executives-Parties variables for 65 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Gender_exp1.html') 
stargazer(bl_asyf_fun_baseline, vdem_gender_fun_baseline, ipu_l_sw_fun_baseline, ciri_wecon_fun_baseline, ciri_wopol_fun_baseline, gii_gii_fun_baseline, cai_cai1_fun_baseline, title="Baseline panel regression on Federal-Unitary variables for 65 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Gender_fun_baseline1.html') 
stargazer(bl_asyf_fun, vdem_gender_fun, ipu_l_sw_fun, ciri_wecon_fun, ciri_wopol_fun, gii_gii_fun, cai_cai1_fun, title="Fixed effects panel regression on Federal-Unitary variables for 65 democracies: Gender Inequality", dep.var.labels = c("Average Schooling Years, Female", "Women Political Empowerment Index", "Share of Women - Lower and Single Houses", "Women's Economic Rights", "Women's Political Rights", "Gender Inequality Index", "Comparative Abortion Index"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Gender_fun1.html') 

#Ethnic Inequality regression tables - Extended 65 democracies
stargazer(ciri_polpris_exp_baseline, bti_ci_exp_baseline, bti_eo_exp_baseline, iaep_ebbp_exp_baseline, title="Baseline panel regression on Executives-Parties variables for 65 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Ethnic_exp_baseline1.html') 
stargazer(ciri_polpris_exp, bti_ci_exp, bti_eo_exp, iaep_ebbp_exp, title="Fixed effects panel regression on Executives-Parties variables for 65 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='Ethnic_exp1.html') 
stargazer(ciri_polpris_fun_baseline, bti_ci_fun_baseline, bti_eo_fun_baseline, iaep_ebbp_fun_baseline, title="Baseline panel regression on Federal-Unitary variables for 65 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Ethnic_fun_baseline1.html') 
stargazer(ciri_polpris_fun, bti_ci_fun, bti_eo_fun, iaep_ebbp_fun, title="Fixed effects panel regression on Federal-Unitary variables for 65 democracies: Ethnic Inequality", dep.var.labels = c("Political imprisonment", "BTI Conflict intensity", "BTI Equal Opportunity", "Ethnicity Based Banning of Parties"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Ethnic_fun1.html') 


#In this part I run F Tests and Hausman Tests on my models to pick the models which best fit the data

# Testing for fixed effects
pFtest(gini_exp, gini_exp_baseline)
pFtest(gini_fun, gini_fun_baseline)
pFtest(ratio10_exp, ratio10_exp_baseline)
pFtest(ratio10_fun, ratio10_fun_baseline)
pFtest(ratio20_exp, ratio20_exp_baseline)
pFtest(ratio20_fun, ratio20_fun_baseline)
pFtest(bl_asyf_exp, bl_asyf_exp_baseline)
pFtest(bl_asyf_fun, bl_asyf_fun_baseline)
pFtest(vdem_gender_exp, vdem_gender_exp_baseline)
pFtest(vdem_gender_fun, vdem_gender_fun_baseline)
pFtest(ipu_l_sw_exp, ipu_l_sw_exp_baseline)
pFtest(ipu_l_sw_fun, ipu_l_sw_fun_baseline)
pFtest(ciri_wopol_exp, ciri_wopol_exp_baseline)
pFtest(ciri_wopol_fun, ciri_wopol_fun_baseline)
pFtest(ciri_wecon_exp, ciri_wecon_exp_baseline)
pFtest(ciri_wecon_fun, ciri_wecon_fun_baseline)
pFtest(gii_gii_exp, gii_gii_exp_baseline)
pFtest(gii_gii_fun, gii_gii_fun_baseline)
pFtest(cai_cai1_exp, cai_cai1_exp_baseline)
pFtest(cai_cai1_fun, cai_cai1_fun_baseline)
pFtest(ciri_polpris_exp, ciri_polpris_exp_baseline)
pFtest(ciri_polpris_fun, ciri_polpris_fun_baseline)
pFtest(bti_ci_exp, bti_ci_exp_baseline)
pFtest(bti_ci_fun, bti_ci_fun_baseline)
pFtest(bti_eo_exp, bti_eo_exp_baseline)
pFtest(bti_eo_fun, bti_eo_fun_baseline)
pFtest(iaep_ebbp_exp, iaep_ebbp_exp_baseline)
pFtest(iaep_ebbp_fun, iaep_ebbp_fun_baseline)

#Creating random effects models for Hausman tests
bl_asyf_exp_random <- plm(panel_data$bl_asyf ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
bl_asyf_fun_random  <- plm(panel_data$bl_asyf ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model= "random")
ipu_l_sw_exp_random <- plm(panel_data$ipu_l_sw ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
ipu_l_sw_fun_random  <- plm(panel_data$ipu_l_sw ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model= "random")
gii_gii_exp_random <- plm(panel_data$gii_gii ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
gii_gii_fun_random  <- plm(panel_data$gii_gii ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model= "random")
cai_cai1_exp_random <- plm(panel_data$cai_cai1 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
cai_cai1_fun_random  <- plm(panel_data$cai_cai1 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model= "random")
ratio10_exp_random <- plm(panel_data$ratio10 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
ratio10_fun_random  <- plm(panel_data$ratio10 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model= "random")
ratio20_exp_random <- plm(panel_data$ratio20 ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
ratio20_fun_random  <- plm(panel_data$ratio20 ~ panel_data$fun + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model= "random")
ciri_polpris_exp_random <- plm(panel_data$ciri_polpris ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
bti_ci_exp_random <- plm(panel_data$bti_ci ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")
gini_exp_random <- plm(panel_data$wdi_gini ~ panel_data$exp + panel_data$undp_hdi + panel_data$wdi_pop_log, panel_data, effect="twoways", model = "random")

#Running Hausman tests for variables where F tests indicated superiority of fixed effects models
phtest(bl_asyf_exp, bl_asyf_exp_random)
phtest(ipu_l_sw_exp, ipu_l_sw_exp_random)
phtest(gii_gii_exp, gii_gii_exp_random)
phtest(cai_cai1_exp, cai_cai1_exp_random)
phtest(ratio10_exp, ratio10_exp_random)
phtest(ratio20_exp, ratio20_exp_random)
phtest(ciri_polpris_exp, ciri_polpris_exp_random)
phtest(bti_ci_exp, bti_ci_exp_random)
phtest(gini_exp, gini_exp_random)

phtest(bl_asyf_fun, bl_asyf_fun_random)
phtest(ipu_l_sw_fun, ipu_l_sw_fun_random)
phtest(gii_gii_fun, gii_gii_fun_random)
phtest(ratio10_fun, ratio10_fun_random)
phtest(cai_cai1_fun, cai_cai1_fun_random)
phtest(ratio20_fun, ratio20_fun_random)

#Producing tables of random effects regressions for p-value of Hausman tests >0.05
stargazer(gii_gii_exp_random, cai_cai1_exp_random, ratio10_exp_random, gini_exp_random, bti_ci_exp_random, title="Random effects panel regression on Executive-Parties variables for 65 democracies: Various measures of Social Inequality", dep.var.labels = c("Gender Inequality Index", "Comparative Abortion Index", "Ratio 10/10", "Gini coefficient", "BTI Conflict Intensity"), covariate.labels = c("Executives-Parties variables: PC1", "Executives-Parties variables: PC2", "HDI", "Logged population"), out='various_exp_random.html') 
stargazer(gii_gii_fun_random, title="Random effects panel regression on Federal-Unitary variables for 65 democracies: Gender Inequality Index", dep.var.labels = c("Gender Inequality Index"), covariate.labels = c("Federal-Unitary variables", "HDI", "Logged population"), out='Random_gii_gii_fun.html') 

#In this part I create plots.

#Creating Executives-Parties and Federal-Unitary additive Indices for plots 
panel_data$gol_enpp3 <- panel_data$gol_enpp
panel_data$disp_gal3 <- panel_data$disp_gal
panel_data$cons1_maj03 <- panel_data$cons1_maj0
panel_data$gol_enpp3 <- panel_data$gol_enpp3 * 4/12.161
panel_data$disp_gal3 <- panel_data$disp_gal3 - 43.56
panel_data$disp_gal3 <- panel_data$disp_gal3 * 4/56.44
panel_data$cons1_maj03 <- panel_data$cons1_maj03 * 4
panel_data$executives_parties_index <- (panel_data$cons1_maj03 + panel_data$disp_gal3 + panel_data$gol_enpp3) 

panel_data$iaep_constlam3 <- panel_data$iaep_constlam
panel_data$gtm_unit3 <- panel_data$gtm_unit
panel_data$cbi_cbiw3 <- panel_data$cbi_cbiw
panel_data$iaep_constlam3 <- panel_data$iaep_constlam3 / 13
panel_data$gtm_unit3 <- panel_data$gtm_unit3 * 2
panel_data$cbi_cbiw3 <- panel_data$cbi_cbiw3 - 0.1380
panel_data$cbi_cbiw3 <- panel_data$cbi_cbiw3 * 4/0.7661
panel_data$federal_unitary_index <- panel_data$cbi_cbiw3 + panel_data$gtm_unit3 + panel_data$iaep_constlam3

panel_data$federal_unitary_index <- panel_data$federal_unitary_index - 0.416
panel_data$federal_unitary_index <- panel_data$federal_unitary_index * 10/9.605
summary(panel_data$federal_unitary_index)

panel_data$executives_parties_index <- panel_data$executives_parties_index - 0.1361
panel_data$executives_parties_index <- panel_data$executives_parties_index * 10/10.533
summary(panel_data$executives_parties_index)

#Creating the plots
ggplot(subset(panel_data, countryname == 'Belgium' | countryname == 'Israel' | countryname == 'Italy' | countryname == 'Nicaragua' | countryname == 'Slovenia' | countryname == 'Trinidad and Tobago'), aes(executives_parties_index, ipu_l_sw, col = countryname)) + geom_line(linetype="dashed")+ geom_point() + xlab("Executives-Parties Index") + ylab("Share of Women in Lower/Single Houses of Parliament")+ggsave("ch1.png", width=9, height = 7)
ggplot(subset(panel_data, countryname == 'Czech Republic' | countryname == 'Dominican Republic' | countryname == 'El Salvador' | countryname == 'Indonesia' | countryname == 'Israel' | countryname == 'Netherlands' | countryname == 'Nicaragua' | countryname == 'Norway'), aes(executives_parties_index, federal_unitary_index, col = countryname)) + geom_line(linetype="dashed") + geom_point() + xlab("Executives-Parties Index") + ylab("Federal-Unitary Index") + ggsave("ch2.png", width = 9, height = 7)