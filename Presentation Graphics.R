# ---- Setup ----
library(raster)
library(rnaturalearth)
library(tidyverse)
library(readxl)
library(sf)
library(kableExtra)
library(leaflet)
library(gridExtra)
library(grid)
library(lattice)
library(shiny)
library(vctrs)
library(here)

options(scipen = 999999999) #turns off scientific notation

plot_theme <- theme_set(theme_classic())
theme_update(panel.grid.major.y = element_line(colour = "grey"), panel.grid.minor.y = element_line(colour = "grey"))

data_total <- read_xlsx(here("CustomQuery.xlsx")) %>% 
  rename(Government = GovtGrantingContract,
         ProjectName = `Project name`) %>% 
  mutate(`Primary sector` = ifelse(`Primary sector` == "Information and communication technology (ICT)", "ICT", `Primary sector`),
         `Financial closure year` = as.numeric(`Financial closure year`),
         ContractPeriod = as.numeric(ifelse(ContractPeriod %in% c("Not Available", "Not Applicable"), NA,ContractPeriod)),
         InvestmentYear = as.numeric(InvestmentYear),
         `Total Equity` = as.numeric(`Total Equity`), #*10e5, #Equity was in was in millions USD
         PercentPrivate = as.numeric(PercentPrivate)/100, #percent was in range 1:100
         FeesToGovernment = as.numeric(FeesToGovernment), #*10e5,
         PhysicalAssets = as.numeric(PhysicalAssets), #*10e5, #Assets was in was in millions USD
         TotalInvestment = as.numeric(TotalInvestment), #*10e5, #investment was in millions USD
         Sponsors = str_replace_all(Sponsors, "_x000d_", " "),
         MultiLateralSupport = str_replace_all(MultiLateralSupport, "_x000d_", " "),
         TotalDebtFunding = ifelse(TotalDebtFunding == "Not Available",
                                   NA,
                                   ifelse(TotalDebtFunding == "Not Applicable", 
                                          0, 
                                          as.numeric(TotalDebtFunding))), #*10e5)),
         ProjectBanks = str_replace_all(ProjectBanks, "_x000d_", " "),
         UnsolicitedProposal = UnsolicitedProposal,
         PublicDisclosure = PublicDisclosure,
         PrivateInvestment = TotalInvestment*PercentPrivate,
         CommercialProjectBanks = factor(ifelse(str_detect(ProjectBanks, "\\(Commercial"), 1, 0)),
         leverage_pct = TotalDebtFunding/TotalInvestment,
         na_count = rowSums(is.na(.))) %>% 
  mutate(Country = ifelse(Country == "United States", "United States of America", Country),
         Country = ifelse(Country == "Russian Federation", "Russia", Country),
         Country = ifelse(Country == "Egypt, Arab Rep.", "Egypt", Country),
         Country = ifelse(Country == "Korea, Dem. Rep.", "South Korea", Country),
         Country = ifelse(Country == "Iran, Islamic Rep.", "Iran", Country),
         Country = ifelse(Country == "Yemen, Rep.", "Yemen", Country),
         Country = ifelse(Country == "Congo, Dem. Rep.", "Democratic Republic of the Congo", Country),
         Country = ifelse(Country == "Congo, Rep.", "Republic of Congo", Country),
         Country = ifelse(Country == "Kyrgyz Republic.", "Kyrgyzstan", Country),
         Country = ifelse(Country == "Lao PDR", "Laos", Country),
         Country = ifelse(Country == "Macedonia, FYR", "Macedonia", Country),
         Country = ifelse(Country == "Guyana, CR", "Guyana", Country),
         Country = ifelse(Country == "Côte d'Ivoire", "Ivory Coast", Country),
         Country = ifelse(Country == "Guinea-Bissau", "Guinea Bissau", Country),
         Country = ifelse(Country == "Kyrgyz Republic", "Kyrgyzstan", Country),
         Country = ifelse(Country == "São Tomé and Principe", "Sao Tome and Principe", Country),
         Country = ifelse(Country == "Serbia", "Republic of Serbia", Country),
         Country = ifelse(Country == "Gambia, The", "Gambia", Country),
         Country = ifelse(Country == "Syrian Arab Republic", "Syria", Country),
         Country = ifelse(Country == "Timor-Leste", "East Timor", Country),
         Country = ifelse(Country == "Tanzania", "United Republic of Tanzania", Country),
         Country = ifelse(Country == "Venezuela, RB", "Venezuela", Country),
         Country = ifelse(Country == "West Bank and Gaza", "Palestine", Country)) %>% 
  mutate_if(is_character, function(x){str_replace_all(x, "<br/>", "\n")}) %>% 
  mutate_if(is.character, function(x){ifelse(x %in% c("Not Available", "Not Applicable", "N/A"), NA, x)})


countries <- ne_countries(returnclass = "sf") %>%
  dplyr::select(admin, economy, income_grp, region_wb) %>%
  rbind(.,
        ne_countries(returnclass = "sf", type = "tiny_countries") %>%
          dplyr::select(admin, economy, income_grp, region_wb) %>% st_buffer(dist=.5)) %>%
  filter(duplicated(admin) == F,
         admin != "Antarctica") %>%
  st_cast(., "MULTIPOLYGON")

data <- data_total %>% 
  filter(is.na(PercentPrivate) == F,
         Government %in% c("Local/Municipal", "State/Provincial", "National"),
         `Project status` != "Cancelled",
         InvestmentYear >= 1992) %>% 
  mutate(Government = factor(Government, levels = c("Local/Municipal", "State/Provincial", "National")))

proj_types <- data %>% 
  group_by(`Type of PPI`, `Subtype of PPI`, Government) %>% 
  summarise(Total = n(),
            Energy = sum(ifelse(`Primary sector` == "Energy", 1, 0)),
            ICT = sum(ifelse(`Primary sector` == "ICT", 1, 0)),
            Transport = sum(ifelse(`Primary sector` == "Transport", 1, 0)),
            Waste = sum(ifelse(`Primary sector` == "Municipal Solid Waste", 1, 0)),
            Water = sum(ifelse(`Primary sector` == "Water and sewerage", 1, 0))) %>% 
  rename(Government = Government)

#proj_types <- proj_types[order(proj_types$Count, decreasing = T),]

proj_country <- data %>% 
  group_by(Country) %>% 
  summarise(Projects =n(),
            PrivateInvestment = sum(PrivateInvestment, na.rm = T),
            local = sum(ifelse(Government == "Local/Municipal", 1, 0)),
            provincial = sum(ifelse(Government == "State/Provincial", 1, 0)),
            colourscale = log(sum(PrivateInvestment, na.rm = T)+1)) %>% 
  full_join(countries, ., by=c("admin" = "Country"))

proj_country_sub <- data %>% 
  filter(Government != "National") %>% 
  group_by(Country) %>% 
  summarise(Projects =n(),
            PrivateInvestment = sum(PrivateInvestment, na.rm = T),
            local = sum(ifelse(Government == "Local/Municipal", 1, 0)),
            provincial = sum(ifelse(Government == "State/Provincial", 1, 0)),
            colourscale = log(sum(PrivateInvestment, na.rm = T)+1)) %>% 
  full_join(countries, ., by=c("admin" = "Country"))



# ---- Projects by government level ----
data %>%
  group_by(Government) %>% 
  summarise(Projects =n()) %>%
  ggplot(., mapping = aes(x=Government, y=Projects, fill=Government)) + 
  geom_col() +
  scale_fill_viridis_d( aesthetics = c("colour", "fill"), option = "C") +
  labs(title = "Projects by Government Type",
       caption = "Data: World Bank PPI") +
  scale_y_continuous(labels = scales::comma, limits = c(0,5000)) +
  guides(fill=FALSE)

  ggsave("number of projects by government type.jpeg", path = "/Users/johnmichaellasalle/Dropbox/Classes/C2IFI/PPI Presentation", width = 15, height= 10, units = "cm", dpi = 300)

# ---- Total investment by government level ----
data %>%
  group_by(Government) %>% 
  summarise(`Total Investment (Millions USD)` = sum(TotalInvestment, na.rm = T)) %>%
  ggplot(., mapping = aes(x=Government, y=`Total Investment (Millions USD)`, fill=Government)) + 
  geom_col() +
  scale_fill_viridis_d( aesthetics = c("colour", "fill"), option = "C") +
  labs(title = "Total Investment by Government Type",
       caption = "Data: World Bank PPI") +
  scale_y_continuous(labels = scales::comma, limits = c(0,1200000)) +
  guides(fill=FALSE)

ggsave("total investment by government type.jpeg", path = "/Users/johnmichaellasalle/Dropbox/Classes/C2IFI/PPI Presentation", width = 15, height= 10, units = "cm", dpi = 300)

# ---- Private investment by government level ----
data %>%
  group_by(Government) %>% 
  summarise(`Private Investment (Millions USD)` = sum(PrivateInvestment, na.rm = T)) %>%
  ggplot(., mapping = aes(x=Government, y=`Private Investment (Millions USD)`, fill=Government)) + 
  geom_col() +
  scale_fill_viridis_d( aesthetics = c("colour", "fill"), option = "C") +
  labs(title = "Projects by Government Type",
       caption = "Data: World Bank PPI") +
  scale_y_continuous(labels = scales::comma, limits = c(0,1200000)) +
  guides(fill=FALSE)

ggsave("private investment by government type.jpeg", path = "/Users/johnmichaellasalle/Dropbox/Classes/C2IFI/PPI Presentation", width = 15, height= 10, units = "cm", dpi = 300)


# ---- Local Projects by Sector ----
ggplot(data %>% filter(Government == "Local/Municipal"), mapping = aes(x = `Primary sector`, y=PrivateInvestment, fill=`Primary sector`)) + 
  geom_col() +
  scale_fill_viridis_d( aesthetics = c("colour", "fill"), option = "C") +
  labs(title = "Private Investment in Local Projects by Sector",
       y = "Private Investment (Millions USD)",
       x = "Sector",
       caption = "Data: World Bank PPI") +
  scale_y_continuous(labels = scales::comma, limits = c(0,50000)) +
  guides(fill=FALSE)

ggsave("private investment by sector local.jpeg", path = "/Users/johnmichaellasalle/Dropbox/Classes/C2IFI/PPI Presentation", width = 15, height= 10, units = "cm", dpi = 300)
  
# ---- Local Projects by Sector ----
data %>% 
  filter(Government == "Local/Municipal") %>% 
  group_by(`Primary sector`) %>% 
  summarize(`Percent Private` = mean(PercentPrivate)) %>% 
  ggplot(., mapping = aes(x = `Primary sector`, y=`Percent Private`, fill=`Primary sector`)) + 
  geom_col() +
  scale_fill_viridis_d( aesthetics = c("colour", "fill"), option = "C") +
  labs(title = "Private Investment in Local Projects by Sector",
       y = "Percent Private Investment",
       x = "Sector",
       caption = "Data: World Bank PPI") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  guides(fill=FALSE)

ggsave("percent private investment by sector local.jpeg", path = "/Users/johnmichaellasalle/Dropbox/Classes/C2IFI/PPI Presentation", width = 15, height= 10, units = "cm", dpi = 300)
