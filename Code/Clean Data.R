setwd("/Users/nancy/Library/CloudStorage/Dropbox-个人/CarbonOffsetsAdditionality/")
getwd()

CDM_pipeline <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "CDM Projects") #2690
CDM_pipeline <- CDM_pipeline %>% drop_na(`Unique project ID`)
summary(CDM_pipeline$`Ref.`)

CDM_pipeline <- CDM_pipeline %>% select_if(~ !(all(. == 0) || all(is.na(.))))
unique(CDM_pipeline$Type)
#[1] "Biomass Energy"        "Methane avoidance"     "EE Industry"           "EE own generation"     "Fossil fuel switch"   
#[6] "Hydro"                 "Wind"                  "Cement"                "HFCs"                  "Landfill gas"         
#[11] "Coal bed/mine methane" "Fugitive"              "EE supply side"        "Agriculture"           "Solar"                
#[16] "EE households"         "Energy distribution"   "Geothermal"            "Reforestation"         "Transport"            
#[21] "N2O"                   "Afforestation"         "EE service"            "PFCs and SF6"          "CO2 usage"            
#[26] "Tidal"                 "Biogas"                "Mixed renewables"      "Biomass energy"        "Hybrid renewables"    
#[31] "EE industry"           "Solar & wind"          "EE Households"         NA                      "Waste"   

# Make uppercase and lowercase consistent for the same project type
CDM_pipeline$Type <- gsub("Biomass Energy", "Biomass energy", CDM_pipeline$Type, ignore.case = TRUE)
CDM_pipeline$Type <- gsub("EE Households", "EE households", CDM_pipeline$Type, ignore.case = TRUE)
CDM_pipeline$Type <- gsub("EE Industry", "EE industry", CDM_pipeline$Type, ignore.case = TRUE)

unique(CDM_pipeline$Type) # solved!


names(CDM_pipeline)

dim(CDM_pipeline) #13153    80
CDM_pipeline$`Unique project ID`[duplicated(CDM_pipeline$`Unique project ID`)] #character(0)


unique(CDM_pipeline$Status) 
#[1] "Withdrawn"             "Validation Terminated" "Validation Replaced"   "Validation Public"     "Rejected"             
#[6] "Registered"            "Deregistered"          "Pending Publication"   "Provisional"

table(CDM_pipeline$`Host country`) #China 5045, India 3401, Brazil 764

CDM_pipeline$`Start of validation` <- as.Date(CDM_pipeline$`Start of validation`)
CDM_pipeline$YearMonth <- format(CDM_pipeline$`Start of validation`, "%Y-%m")
CDM_pipeline$YearQuarter <- paste(format(CDM_pipeline$`Start of validation`, "%Y"), "-Q", 
                                  floor((as.numeric(format(CDM_pipeline$`Start of validation`, "%m")) - 1) / 3) + 1, 
                                  sep = "")

CDM_pipeline$ChangeIRR <- CDM_pipeline$`IRR  % incl. CER` - CDM_pipeline$`IRR % excl. CER`
CDM_pipeline$ExtraBenchIRR <- CDM_pipeline$`IRR  % incl. CER` - CDM_pipeline$`IRR benchmark`
CDM_pipeline$ShortBenchIRR <- CDM_pipeline$`IRR benchmark` - CDM_pipeline$`IRR % excl. CER`

# generate summary statistics of all variables
st(CDM_pipeline) 

# Find outliers
CDM_pipeline %>%
  filter(`IRR  % incl. CER` > 70) %>%
  select(`IRR % excl. CER`,Title,ShortBenchIRR,`Ref.`,`IRR  % incl. CER`,`IRR benchmark`,Status,`Start of validation`,ChangeIRR)

CDM_pipeline %>%
  filter(`IRR benchmark` > 30) %>%
  select(`IRR % excl. CER`,Title,ShortBenchIRR,`Ref.`,`IRR  % incl. CER`,`IRR benchmark`,Status,`Start of validation`,ChangeIRR)

CDM_pipeline %>%
  filter(`IRR % excl. CER` > 40) %>%
  dplyr::select(`IRR % excl. CER`,Title,ShortBenchIRR,`Ref.`,`IRR  % incl. CER`,`IRR benchmark`,Status,`Start of validation`,ChangeIRR)

CDM_pipeline %>%
  filter(ShortBenchIRR < 0) %>%
  select(`IRR % excl. CER`,Title,ShortBenchIRR,`Ref.`,`IRR  % incl. CER`,`IRR benchmark`,Status,`Start of validation`,ChangeIRR)


# Compare the outliers with data from Project Design Documents (PDDs) and correct typos 
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`IRR benchmark` == 73, 13, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`IRR benchmark` == 80, 8, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 1314, NA, `IRR benchmark`))

CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 5086, 7.32, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 7785, 0.27, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 2370, 6.84, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 2424, -16.37, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 2817, 7.04, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 3272, 7.71, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 3272, 10, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 3272, 10.52, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 3520, NA, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 3520, 16.6, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 3597, 6.41, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 4548, 10, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 5804, 0.8, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 6786, 0.75, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 9190, 0.60, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 5120, 0.13, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 10051, 5, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 10051, 17.34, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 2496, NA, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 7576, NA, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 7576, NA, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 6733, 7.78, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 6733, 11.80, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 10080, 19.7, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 4816, 48.86, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 3316, -9.66, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 4337, 14.08, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 1308, 7.90, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 1308, 16, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 1308, 16.93, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 2306, 5.38, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 2306, 6.94, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 3945, 11.7, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 3945, 14.61, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 3996, 13.68, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 4051, 11.73, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 5549, 14.05, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 5549, 10.98, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 5549, 4.36, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 5802, 10.89, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 6625, 10.74, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 7092, 14.18, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 7357, 9.67, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 7357, 7.76, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 8009, 11.91, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 8009, 10.03, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 8273, 8.88, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 10022, 7.62, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 10112, 17.77, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 10112, NA, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 10112, 11.13, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 8071, 5.87, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 5369, 10, `IRR benchmark`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`Ref.` == 2216, 8.94, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`Ref.` == 2216, 5.94, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR benchmark` = ifelse(`Ref.` == 2216, 8, `IRR benchmark`))

CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`IRR % excl. CER` == 92, 9.2, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR % excl. CER` = ifelse(`IRR % excl. CER` == 98.9, 9.89, `IRR % excl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`IRR  % incl. CER` == 195, 19.5, `IRR  % incl. CER`))
CDM_pipeline <- CDM_pipeline %>%
  mutate(`IRR  % incl. CER` = ifelse(`IRR  % incl. CER` == 154, 15.4, `IRR  % incl. CER`))


# China applied standardized IRR benchmarks, 8%.
CDM_pipeline$`IRR benchmark` <- ifelse(
  CDM_pipeline$Type == "Wind" & CDM_pipeline$`Host country` == "China" &
    !is.na(CDM_pipeline$`IRR % excl. CER`) & !is.na(CDM_pipeline$`IRR  % incl. CER`) & is.na(CDM_pipeline$`IRR benchmark`),
  8,
  CDM_pipeline$`IRR benchmark`
)

# Create more variables
CDM_pipeline$`AnnualCredRev(MUS)` <- CDM_pipeline$`Reductions (ktCO2e/yr)`*CDM_pipeline$`CER price  US$/tCO2`/1000
CDM_pipeline$`TotalCredRev(MUS)` <- CDM_pipeline$`Total issuance (kCERs)`*CDM_pipeline$`CER price  US$/tCO2`/1000
CDM_pipeline$MWh <- CDM_pipeline$MW*CDM_pipeline$`Full time hours`
CDM_pipeline$CO2 <- CDM_pipeline$MWh*CDM_pipeline$`Grid emission factor tCO2e/MWh`/1000

CDM_pipeline$ChangeIRR <- CDM_pipeline$`IRR  % incl. CER` - CDM_pipeline$`IRR % excl. CER`
CDM_pipeline$ExtraBenchIRR <- CDM_pipeline$`IRR  % incl. CER` - CDM_pipeline$`IRR benchmark`
CDM_pipeline$ShortBenchIRR <- CDM_pipeline$`IRR benchmark` - CDM_pipeline$`IRR % excl. CER`


# generate summary statistics of all variables again
st(CDM_pipeline)
names(CDM_pipeline)

write_rds(CDM_pipeline, "./Data/cleaned/CDM_pipeline.rds")

##########################################################################################
# combine five offset programmes: (1) Verra, (2) Gold Standard, (3) CAR, (4) ACR, (5) CDM

#Verra <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "Verra Projects")
#Verra_credits <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "Verra VCUs")

#CAR <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "CAR Projects")
#CAR_credits <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "CAR Issuances")

#GS <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "Gold Projects")
#GS_credits <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "Gold Issuances")

#ACR <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "ACR Projects")
#ACR_credits <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "ACR Issuances")

CDM <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "CDM activities")
CDM_credits <- read_excel("./Data/raw/offset-registry-files-2024-08-31.xlsx", sheet = "CDM Issuances")
country_code <- select(world, iso_a2, admin)
country_code <- distinct(country_code)

CDM_credits <- left_join(CDM_credits, country_code, by = c("HostParty" = "iso_a2"))
names(CDM_credits)
names(CDM_pipeline)
CDM_credits$`Issuance date` <- as.Date(CDM_credits$`Issuance date`, format = "%Y-%m-%d")
CDM_credits$`Issuance Year` <- year(CDM_credits$`Issuance date`)
CDM_credits$ID <- CDM_credits$`CDM project reference number`
CDM_credits$Registry <- "CDM"
CDM_credits$Credits <- CDM_credits$`Units - Total`
CDM_credits <- select(CDM_credits, ID, Registry, admin, `Issuance Year`, Credits)
names(CDM_credits)


## Issuance Year
Voluntary_credits <- read_excel(
  path = "./Data/raw/Voluntary-Registry-Offsets-Database-v2024-08-31.xlsx", 
  sheet = "Sheet1", 
  range = cell_limits(c(4, 1), c(10000, 52))
)

Voluntary_credits$ID <- Voluntary_credits$`Project ID`
Voluntary_credits$Registry <- Voluntary_credits$`Voluntary Registry`
Voluntary_credits <- Voluntary_credits %>% 
  filter(`Project ID` != "")
write_rds(Voluntary_credits, "./Data/cleaned/Voluntary_credits.rds")

Voluntary_credits <- Voluntary_credits %>% 
  filter(ID != "") %>% 
  select(ID, Registry, Country, "1997":"2024")
names(Voluntary_credits)

Voluntary_long <- Voluntary_credits %>%
  pivot_longer(
    cols = `1997`:`2024`,            # 指定要转换的列范围
    names_to = "Issuance Year",      # 转换后列名
    values_to = "Credits"     # 转换后的值列名
  ) %>%
  filter(Credits > 0) %>%
  mutate(admin = Country) %>%
  select(ID, Registry, admin, `Issuance Year`, Credits)
names(Voluntary_long)
table(Voluntary_long$admin)

Voluntary_long$admin[Voluntary_long$admin == "Bahamas"] <- "The Bahamas"
Voluntary_long$admin[Voluntary_long$admin == "Cape Verde"] <- "Cabo Verde"
Voluntary_long$admin[Voluntary_long$admin == "Congo"] <- "Republic of the Congo"
Voluntary_long$admin[Voluntary_long$admin == "Congo (DRC)"] <- "Democratic Republic of the Congo"
Voluntary_long$admin[Voluntary_long$admin == "Côte d'Ivoire"] <- "Ivory Coast"
Voluntary_long$admin[Voluntary_long$admin == "DRC"] <- "Democratic Republic of the Congo"
Voluntary_long$admin[Voluntary_long$admin == "Eswatini"] <- "eSwatini"
Voluntary_long$admin[Voluntary_long$admin == "Hong Kong"] <- "Hong Kong S.A.R."
Voluntary_long$admin[Voluntary_long$admin == "Serbia"] <- "Republic of Serbia"
Voluntary_long$admin[Voluntary_long$admin == "Tanzania"] <- "United Republic of Tanzania"
Voluntary_long$admin[Voluntary_long$admin == "Timor-Leste"] <- "East Timor"
Voluntary_long$admin[Voluntary_long$admin == "Türkiye"] <- "Turkey"
Voluntary_long$admin[Voluntary_long$admin == "United States"] <- "United States of America"
Voluntary_long$admin[Voluntary_long$admin == "Viet Nam"] <- "Vietnam"

creditcounts_year_registry_country <- rbind(CDM_credits, Voluntary_long)
creditcounts_year_registry_country$`Issuance Year`<- as.numeric(creditcounts_year_registry_country$`Issuance Year`)

write_rds(creditcounts_year_registry_country, "./Data/cleaned/creditcounts_year_registry_country.rds")
write.xlsx(creditcounts_year_registry_country, "./Data/cleaned/creditcounts_year_registry_country.xlsx")


# arrange ID and Issuance year
country_projectcounts_year <- creditcounts_year_registry_country %>%
  arrange(ID, `Issuance Year`) %>%
  distinct(ID, .keep_all = TRUE)
country_projectcounts_year$Country <- country_projectcounts_year$admin
country_projectcounts_year$Country[country_projectcounts_year$Country != "China" 
                                   & country_projectcounts_year$Country != "Brazil" & country_projectcounts_year$Country != "India" 
                                   & country_projectcounts_year$Country != "United States of America"] <- "Others"
country_projectcounts_year$Country[country_projectcounts_year$Country == "United States of America"] <- "United States"
country_projectcounts_year_0424 <- country_projectcounts_year %>%
  group_by(Country, `Issuance Year`) %>%
  summarise(project_count = n(), .groups = 'drop') %>%
  mutate(Country = factor(Country, levels = c("Brazil", "India", "China", "United States", "Others"))) %>%
  filter(`Issuance Year` >= 2004, `Issuance Year` <= 2024)
write_rds(country_projectcounts_year_0424, "./Data/cleaned/country_projectcounts_year_0424.rds")

creditcounts_year_registry_0424 <- creditcounts_year_registry_country %>%
  group_by(Registry, `Issuance Year`) %>%
  summarise(credit_count = sum(`Credits`), .groups = 'drop') %>%
  filter(`Issuance Year` >= 2004, `Issuance Year` <= 2024)
write_rds(creditcounts_year_registry_0424, "./Data/cleaned/creditcounts_year_registry_0424.rds")

creditcounts_year_country_0424 <- creditcounts_year_registry_country %>%
  group_by(admin, `Issuance Year`) %>%
  summarise(credit_count = sum(`Credits`), .groups = 'drop')  %>%
  filter(`Issuance Year` >= 2004, `Issuance Year` <= 2024) %>%
  mutate(Country = admin) %>%
  select(Country, `Issuance Year`, credit_count)
write_rds(creditcounts_year_country_0424, "./Data/cleaned/creditcounts_year_country_0424.rds")



Voluntary <- read_excel(
  path = "./Data/raw/Voluntary-Registry-Offsets-Database-v2024-08-31.xlsx", 
  sheet = "PROJECTS", 
  range = cell_limits(c(4, 1), c(10000, 52))
)
Voluntary <- filter(Voluntary, `Project ID` != "")

#summary(CDM_pipeline$`Start 1st period`)
#summary(CDM_pipeline$`Start of validation`)
#summary(CDM_pipeline$`Date of registration`)

names(Voluntary)
required_columns_voluntary <- c("Project ID", "Project Name", "Voluntary Registry", "Voluntary Status", 
                      "Type", "Country", "State", "Total Credits \r\nIssued", "Total Credits \r\nRetired",
                      "First Year of Project (Vintage)")
Voluntary_filtered <- Voluntary[, required_columns_voluntary]
new_names <- c("ID", "Project Name", "Registry", "Status", 
                                "Type", "Country", "State", "Total Credits Issued", 
                                "Total Credits Retired", "First Vintage Year")
colnames(Voluntary_filtered) <- new_names
names(Voluntary_filtered)

names(CDM_filtered)
names(CDM_pipeline)

CDM_filtered <- CDM_pipeline %>% 
  select("Unique project ID", "Title", 
         "Status", 
         "Type", "Host country", "Province / State", "Total issuance (kCERs)", 
         "Voluntary cancellations (kCERs)", "Start of validation")
CDM_filtered$Registry <- "CDM"
CDM_filtered$`First Vintage Year` <- year(CDM_filtered$`Start of validation`)

CDM_filtered <- CDM_filtered %>%
  select("Unique project ID", "Title", 
         "Registry", "Status", 
         "Type", "Host country", "Province / State", "Total issuance (kCERs)", 
         "Voluntary cancellations (kCERs)", "First Vintage Year")
CDM_filtered$`Total issuance (kCERs)` <- CDM_filtered$`Total issuance (kCERs)`*1000
CDM_filtered$`Voluntary cancellations (kCERs)` <- CDM_filtered$`Voluntary cancellations (kCERs)`*1000
colnames(CDM_filtered) <- new_names

offsets_projects <- rbind(Voluntary_filtered, CDM_filtered)
table(offsets_projects$Country)
table(world$admin)

table1 <- as.data.frame(table(offsets_projects$Country))
table2 <- as.data.frame(table(world$admin))
table <- merge(table1, table2, by = "Var1", all = TRUE)

offsets_projects$Country[offsets_projects$Country == "Bahamas"] <- "The Bahamas"
offsets_projects$Country[offsets_projects$Country == "Cape Verde"] <- "Cabo Verde"
offsets_projects$Country[offsets_projects$Country == "Congo"] <- "Republic of the Congo"
offsets_projects$Country[offsets_projects$Country == "Congo (DRC)"] <- "Democratic Republic of the Congo"
offsets_projects$Country[offsets_projects$Country == "Côte d'Ivoire"] <- "Ivory Coast"
offsets_projects$Country[offsets_projects$Country == "DRC"] <- "Democratic Republic of the Congo"
offsets_projects$Country[offsets_projects$Country == "Eswatini"] <- "eSwatini"
offsets_projects$Country[offsets_projects$Country == "Hong Kong"] <- "Hong Kong S.A.R."
#offsets_projects$Country[offsets_projects$Country == "International"] <- ""
#offsets_projects$Country[offsets_projects$Country == "multiple"] <- ""
offsets_projects$Country[offsets_projects$Country == "Serbia"] <- "Republic of Serbia"
offsets_projects$Country[offsets_projects$Country == "Tanzania"] <- "United Republic of Tanzania"
offsets_projects$Country[offsets_projects$Country == "Timor-Leste"] <- "East Timor"
offsets_projects$Country[offsets_projects$Country == "Türkiye"] <- "Turkey"
offsets_projects$Country[offsets_projects$Country == "United States"] <- "United States of America"
offsets_projects$Country[offsets_projects$Country == "Viet Nam"] <- "Vietnam"

offsets_projects <- offsets_projects %>%
  mutate(ID_number = as.numeric(gsub("[^0-9]", "", ID))) %>%  # 提取数字部分
  arrange(Registry,ID_number) %>%  # 按提取的数字部分排序
  select(-ID_number)  # 删除辅助列

write_rds(offsets_projects, "./Data/cleaned/offsets_projects_five_registries.rds")
#write.xlsx(offsets_projects, file = "./Data/cleaned/offsets_projects_five_registries.xlsx", overwrite = TRUE)

write_rds(offsets_projects, "./Data/cleaned/offsets_projects_five_registries_2025.rds")

offsets_projectcounts <- offsets_projects
offsets_projectcounts$Country[offsets_projectcounts$Country != "China" 
                              & offsets_projectcounts$Country != "Brazil" & offsets_projectcounts$Country != "India" 
                              & offsets_projectcounts$Country != "United States of America"] <- "Others"

write.xlsx(offsets_projectcounts, file = "./Data/cleaned/offsets_projectcounts.xlsx", overwrite = TRUE)

country_projectcounts_year <- offsets_projectcounts %>%
  group_by(Country, `First Vintage Year`) %>%
  summarise(project_count = n(), .groups = 'drop') %>%
  filter(`First Vintage Year` >= 1999, `First Vintage Year` < 2024)
country_projectcounts_year$Country[country_projectcounts_year$Country == "United States of America"] <- "United States"



