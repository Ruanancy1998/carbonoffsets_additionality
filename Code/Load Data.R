
#Load Cleaned Data

CDM_pipeline <- read_rds("./Data/cleaned/CDM_pipeline.rds")

CDM_Wind_China <- read_rds("./Data/cleaned/CDM_Wind_China.rds")

Voluntary_credits <- read_rds("./Data/cleaned/Voluntary_credits.rds")

offsets_projects <- read_rds("./Data/cleaned/offsets_projects_five_registries.rds")

creditcounts_year_registry_country <- read_rds("./Data/cleaned/creditcounts_year_registry_country.rds")
country_projectcounts_year_0424 <- read_rds("./Data/cleaned/country_projectcounts_year_0424.rds")
creditcounts_year_registry_0424 <- read_rds("./Data/cleaned/creditcounts_year_registry_0424.rds")

googletrends <- read_csv("./Data/raw/googletrends.csv")
#names(googletrends)


# subset 1: all registered projects
registered_CDM <- CDM_pipeline %>%
  filter(`Ref.` >= 1) %>%
  filter(Status == "Registered")
#st(registered_CDM)

#subset 2: all renewable energy projects globally
CDM_RE <- filter(CDM_pipeline, Type %in% c("Solar", "Hydro", "Wind"))
#dim(CDM_RE) #6762   85
#st(CDM_RE)

#subset 3: all wind projects globally
CDM_Wind <- filter(CDM_RE, Type == "Wind")
#dim(CDM_Wind) #3087   85
#st(CDM_Wind)

#subset 4: all wind projects in India
CDM_Wind_India <- filter(CDM_RE, `Host country` == "India")
#dim(CDM_Wind_India) #1647   89
#st(CDM_Wind_India)
#table(CDM_Wind_India$Status) #registered 1043/1647

#subset 5: all wind projects in China
CDM_Wind_China <- filter(CDM_Wind, `Host country` == "China")
#dim(CDM_Wind_China) #1614   85
#table(CDM_Wind_China$Status)
#st(CDM_Wind_China)
#summary(CDM_Wind_China$`IRR % excl. CER`)
