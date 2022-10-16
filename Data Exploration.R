#######################################################################################
# LOADING DATASETS --------------------------------------------------------------------

#Set Working Directory

    df_accounts <- read.csv("Account_USA.csv", header = T)
    View(df_accounts)
    
    df_profit2020 <- read.csv("Profitability_2020.csv", header = T)
    View(df_profit2020)
    
    df_profit2021 <- read.csv("Profitability_2021.csv", header = T)
    View(df_profit2021)


#######################################################################################
# LOADING LIBRARIES -------------------------------------------------------------------

    library(dplyr)
    library(tidyr)
    library(janitor)
    library(stringr)
    library(ggplot2)
    library(lubridate)
    library(caret)
    library(ggplot2)


#######################################################################################
# DATA STRUCTURE ----------------------------------------------------------------------

#df_profit2020
    names(df_profit2020)
    str(df_profit2020)
    length(unique(df_profit2020$Customer.Key)) #958
    length(unique(df_profit2020$Product.line)) #8; Grey, masonry, white, slag, raw materials
    #?additional services, others, not assigned
    length(unique(df_profit2020$Material)) #255
    length(unique(df_profit2020$Product.type)) #20; Type I/II =? Tipo I/II
    length(unique(df_profit2020$Source.plant)) #37


#df_profit2021
    names(df_profit2021)
    str(df_profit2021)
    length(unique(df_profit2021$Customer.Key)) #979
    length(unique(df_profit2021$Product.line)) #8; Grey, masonry, white, slag, raw materials
    #?additional services, others, not assigned
    length(unique(df_profit2021$Material)) #250
    sort(unique(df_profit2021$Material))
    length(unique(df_profit2021$Product.type)) #20; Type I/II =? Tipo I/II
    length(unique(df_profit2021$Source.plant)) #41


#######################################################################################
# DATA CLEANING: df_profit2020_KPIs ---------------------------------------------------
    

#Filter necessary KPIs for profit2020
#df_profit2020_KPIs  <- df_profit2020 %>%
  
#  filter(Totals %in% c("REVENUE", "EBITDA", "QUANTITY SOLD")) %>%
  
#  mutate(Revenue = ifelse(Totals == "REVENUE", Actual.accumulated.closing.month.Company.Cur, NA),
#         EBITDA = ifelse(Totals == "EBITDA", Actual.accumulated.closing.month.Company.Cur, NA),
#         Quantity_Sold = ifelse(Totals == "QUANTITY SOLD", Actual.accumulated.closing.month.Company.Cur, NA)) %>%
  
#  select(-Totals, -Actual.accumulated.closing.month.Company.Cur) %>%
  
#  group_by(Customer.Key, Material.Key, Fiscal.year.period.Key) %>%
  
#  fill(Revenue, EBITDA, Quantity_Sold, .direction = "downup")

#Restructure
#df_profit2020_KPIs <- df_profit2020_KPIs %>%

#select(Customer.Key, 
         Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
         Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
         Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
         Product.line.Key, Product.line, Material.Key, Material,
         Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
         Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
         Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
         Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
         Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
         Incoterms.Key, Incoterms, Revenue, EBITDA, Quantity_Sold) %>%

#mutate(Customer.Key = as.factor(Customer.Key), 
         Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
         Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
         Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
         Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
         Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
         Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
         Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
         Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
         Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
         Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
         Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
         City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
         Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
         Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
         Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
         Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
         Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
         Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
         Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))

#Remove duplicated rows after merging 3 KPIs for a customer
#df_profit2020_KPIs <- df_profit2020_KPIs[!duplicated(df_profit2020_KPIs), ]


#######################################################################################
# DATA CLEANING: df_profit2021_KPIs ---------------------------------------------------


#Filter necessary KPIs for profit2021
df_profit2021_KPIs  <- df_profit2021 %>%
  
  filter(Totals %in% c("REVENUE", "EBITDA", "QUANTITY SOLD")) %>%
  
  mutate(Revenue = ifelse(Totals == "REVENUE", Actual.accumulated.closing.month.Company.Cur, NA),
         EBITDA = ifelse(Totals == "EBITDA", Actual.accumulated.closing.month.Company.Cur, NA),
         Quantity_Sold = ifelse(Totals == "QUANTITY SOLD", Actual.accumulated.closing.month.Company.Cur, NA)) %>%
  
  select(-Totals, -Actual.accumulated.closing.month.Company.Cur) %>%
  
  group_by(Customer.Key, Material.Key, Fiscal.year.period.Key) %>%
  
  fill(Revenue, EBITDA, Quantity_Sold, .direction = "downup")

#Restructure
df_profit2021_KPIs <- df_profit2021_KPIs %>%
  
  select(Customer.Key, 
         Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
         Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
         Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
         Product.line.Key, Product.line, Material.Key, Material,
         Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
         Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
         Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
         Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
         Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
         Incoterms.Key, Incoterms, Revenue, EBITDA, Quantity_Sold) %>%
  
  mutate(Customer.Key = as.factor(Customer.Key), 
         Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
         Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
         Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
         Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
         Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
         Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
         Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
         Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
         Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
         Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
         Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
         City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
         Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
         Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
         Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
         Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
         Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
         Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
         Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))




#######################################################################################
# VARIATION 2 FOR DATA CLEANING accounts

df_accounts_key <- df_accounts %>%
  mutate(Customer.Key = as.numeric(as.factor(AccountNumber))) %>%
  mutate(Segment__c = case_when(
    str_detect(Segment__c, "Buyer") ~ "Basic Buyer",
    str_detect(Segment__c, "Relation") ~ "Relationally-Driven",
    str_detect(Segment__c, "Performance") ~ "Performance-Driven",
    str_detect(Segment__c, "External") ~ "External Strength",
    str_detect(Segment__c, "Internal") ~ "Internal Strength",
    TRUE ~ ""
  )) %>%
  select(-AccountNumber) %>%
  remove_empty("cols") %>%
  select(where(~n_distinct(.) > 1))


#######################################################################################
# VARIATION 2 FOR DATA CLEANING profits2020


#Revenue 2020----
df_profit2020_revenue  <- df_profit2020 %>%
  
  filter(Totals == "REVENUE") %>%
  
  mutate(Revenue = Actual.accumulated.closing.month.Company.Cur) %>%
  
  select(Customer.Key, 
         Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
         Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
         Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
         Product.line.Key, Product.line, Material.Key, Material,
         Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
         Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
         Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
         Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
         Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
         Incoterms.Key, Incoterms, Revenue) %>%
  
  mutate(Customer.Key = as.numeric(as.factor(Customer.Key)), 
         Source.plant.Key = as.numeric(as.factor(Source.plant.Key)), Source.plant = as.factor(Source.plant), 
         Product.Packaging.Key = as.numeric(as.factor(Product.Packaging.Key)), Product.Packaging = as.factor(Product.Packaging),
         Plant.Key = as.numeric(as.factor(Plant.Key)), Plant = as.factor(Plant), 
         Transaction.Type.Key = as.numeric(as.factor(Transaction.Type.Key)), Transaction.Type = as.factor(Transaction.Type),
         Sales.office.customer.Key = as.numeric(as.factor(Sales.office.customer.Key)), Sales.office.customer = as.factor(Sales.office.customer), 
         Deparment.State.customer.Key = as.numeric(as.factor(Deparment.State.customer.Key)), Deparment.State.customer = as.factor(Deparment.State.customer),
         Product.line.Key = as.numeric(as.factor(Product.line.Key)), Product.line = as.factor(Product.line), 
         Material.Key = as.numeric(as.factor(Material.Key)), Material = as.factor(Material), 
         Profit.Center.Key = as.numeric(as.factor(Profit.Center.Key)), Profit.Center = as.factor(Profit.Center), 
         Profit.Center.Zone.Key = as.numeric(as.factor(Profit.Center.Zone.Key)), Profit.Center.Zone = as.factor(Profit.Center.Zone),
         Ship.to.Key = as.numeric(as.factor(Ship.to.Key)), Ship.to = as.factor(Ship.to), 
         City.ship.to.Key = as.numeric(as.factor(City.ship.to.Key)), City.ship.to = as.factor(City.ship.to),
         Shipping.Conditions.Key = as.numeric(as.factor(Shipping.Conditions.Key)), Shipping.Conditions = as.factor(Shipping.Conditions),
         Sales.office.ship.to.Key = as.numeric(as.factor(Sales.office.ship.to.Key)), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
         Fiscal.year.period.Key = as.numeric(as.factor(Fiscal.year.period.Key)), Fiscal.year.period = as.factor(Fiscal.year.period), 
         Deparment.State.ship.to.Key = as.numeric(as.factor(Deparment.State.ship.to.Key)), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
         Presentation.pack.Key = as.numeric(as.factor(Presentation.pack.Key)), Presentation.pack = as.factor(Presentation.pack),
         Product.type.Key = as.numeric(as.factor(Product.type.Key)), Product.type = as.factor(Product.type),
         Incoterms.Key = as.numeric(as.factor(Incoterms.Key)), Incoterms = as.factor(Incoterms))


df_profit2020_revenue_accounts <- left_join(df_profit2020_revenue, df_accounts_key, by = "Customer.Key")
str(df_profit2020_revenue_accounts)

df_profit2020_revenue_accounts <- df_profit2020_revenue_accounts %>%
  mutate(Segment__c_NUM = as.numeric(as.factor(Segment__c)))



#EBITDA 2020----
df_profit2020_KPIs_ebitda  <- df_profit2020 %>%
  
  filter(Totals == "EBITDA") %>%
  
  select(Customer.Key, 
         Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
         Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
         Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
         Product.line.Key, Product.line, Material.Key, Material,
         Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
         Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
         Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
         Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
         Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
         Incoterms.Key, Incoterms, Totals, Actual.accumulated.closing.month.Company.Cur) %>%
  
  mutate(Customer.Key = as.factor(Customer.Key), 
         Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
         Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
         Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
         Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
         Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
         Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
         Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
         Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
         Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
         Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
         Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
         City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
         Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
         Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
         Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
         Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
         Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
         Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
         Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))


#Quantity Sold 2020----
df_profit2020_KPIs_sales  <- df_profit2020 %>%
  
  filter(Totals == "QUANTITY SOLD") %>%
  
  select(Customer.Key, 
         Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
         Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
         Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
         Product.line.Key, Product.line, Material.Key, Material,
         Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
         Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
         Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
         Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
         Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
         Incoterms.Key, Incoterms, Totals, Actual.accumulated.closing.month.Company.Cur) %>%
  
  mutate(Customer.Key = as.factor(Customer.Key), 
         Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
         Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
         Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
         Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
         Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
         Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
         Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
         Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
         Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
         Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
         Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
         City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
         Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
         Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
         Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
         Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
         Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
         Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
         Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))



#######################################################################################
# VARIATION 2 FOR DATA CLEANING profits2021


#Revenue 2021----
df_profit2021_revenue  <- df_profit2021 %>%
  
  filter(Totals == "REVENUE") %>%

  mutate(Revenue = Actual.accumulated.closing.month.Company.Cur) %>%
  
  select(Customer.Key, 
         Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
         Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
         Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
         Product.line.Key, Product.line, Material.Key, Material,
         Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
         Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
         Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
         Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
         Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
         Incoterms.Key, Incoterms, Revenue) %>%
  
  mutate(Customer.Key = as.numeric(as.factor(Customer.Key)), 
         Source.plant.Key = as.numeric(as.factor(Source.plant.Key)), Source.plant = as.factor(Source.plant), 
         Product.Packaging.Key = as.numeric(as.factor(Product.Packaging.Key)), Product.Packaging = as.factor(Product.Packaging),
         Plant.Key = as.numeric(as.factor(Plant.Key)), Plant = as.factor(Plant), 
         Transaction.Type.Key = as.numeric(as.factor(Transaction.Type.Key)), Transaction.Type = as.factor(Transaction.Type),
         Sales.office.customer.Key = as.numeric(as.factor(Sales.office.customer.Key)), Sales.office.customer = as.factor(Sales.office.customer), 
         Deparment.State.customer.Key = as.numeric(as.factor(Deparment.State.customer.Key)), Deparment.State.customer = as.factor(Deparment.State.customer),
         Product.line.Key = as.numeric(as.factor(Product.line.Key)), Product.line = as.factor(Product.line), 
         Material.Key = as.numeric(as.factor(Material.Key)), Material = as.factor(Material), 
         Profit.Center.Key = as.numeric(as.factor(Profit.Center.Key)), Profit.Center = as.factor(Profit.Center), 
         Profit.Center.Zone.Key = as.numeric(as.factor(Profit.Center.Zone.Key)), Profit.Center.Zone = as.factor(Profit.Center.Zone),
         Ship.to.Key = as.numeric(as.factor(Ship.to.Key)), Ship.to = as.factor(Ship.to), 
         City.ship.to.Key = as.numeric(as.factor(City.ship.to.Key)), City.ship.to = as.factor(City.ship.to),
         Shipping.Conditions.Key = as.numeric(as.factor(Shipping.Conditions.Key)), Shipping.Conditions = as.factor(Shipping.Conditions),
         Sales.office.ship.to.Key = as.numeric(as.factor(Sales.office.ship.to.Key)), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
         Fiscal.year.period.Key = as.numeric(as.factor(Fiscal.year.period.Key)), Fiscal.year.period = as.factor(Fiscal.year.period), 
         Deparment.State.ship.to.Key = as.numeric(as.factor(Deparment.State.ship.to.Key)), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
         Presentation.pack.Key = as.numeric(as.factor(Presentation.pack.Key)), Presentation.pack = as.factor(Presentation.pack),
         Product.type.Key = as.numeric(as.factor(Product.type.Key)), Product.type = as.factor(Product.type),
         Incoterms.Key = as.numeric(as.factor(Incoterms.Key)), Incoterms = as.factor(Incoterms),
         )
 

df_profit2021_revenue_accounts <- left_join(df_profit2021_revenue, df_accounts_key, by = "Customer.Key")
str(df_profit2021_revenue_accounts)

df_profit2021_revenue_accounts <- df_profit2021_revenue_accounts %>%
  mutate(Segment__c_NUM = as.numeric(as.factor(Segment__c)))
    


#EBITDA 2021----
    df_profit2021_KPIs_ebitda  <- df_profit2021 %>%
      
      filter(Totals == "EBITDA") %>%
      
      select(Customer.Key, 
             Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
             Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
             Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
             Product.line.Key, Product.line, Material.Key, Material,
             Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
             Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
             Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
             Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
             Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
             Incoterms.Key, Incoterms, Totals, Actual.accumulated.closing.month.Company.Cur) %>%
      
      mutate(Customer.Key = as.factor(Customer.Key), 
             Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
             Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
             Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
             Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
             Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
             Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
             Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
             Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
             Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
             Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
             Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
             City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
             Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
             Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
             Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
             Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
             Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
             Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
             Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))
    

#Quantity Sold 2021----
    df_profit2021_KPIs_sales  <- df_profit2021 %>%
      
      filter(Totals == "QUANTITY SOLD") %>%
      
      select(Customer.Key, 
             Source.plant.Key, Source.plant, Product.Packaging.Key, Product.Packaging,
             Plant.Key, Plant, Transaction.Type.Key, Transaction.Type,
             Sales.office.customer.Key, Sales.office.customer, Deparment.State.customer.Key, Deparment.State.customer,
             Product.line.Key, Product.line, Material.Key, Material,
             Profit.Center.Key, Profit.Center, Profit.Center.Zone.Key, Profit.Center.Zone,
             Ship.to.Key, Ship.to, City.ship.to.Key, City.ship.to,
             Shipping.Conditions.Key, Shipping.Conditions, Sales.office.ship.to.Key, Sales.office.ship.to,
             Fiscal.year.period.Key, Fiscal.year.period, Deparment.State.ship.to.Key, Deparment.State.ship.to,
             Presentation.pack.Key, Presentation.pack, Product.type.Key, Product.type,
             Incoterms.Key, Incoterms, Totals, Actual.accumulated.closing.month.Company.Cur) %>%
      
      mutate(Customer.Key = as.factor(Customer.Key), 
             Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
             Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
             Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
             Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
             Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
             Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
             Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
             Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
             Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
             Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
             Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
             City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
             Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
             Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
             Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
             Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
             Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
             Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
             Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))
    


#######################################################################################
# MERGE 2020, 2021 PROFITABILITY ------------------------------------------------------


#Combined 2020 and 2021
#    names(df_profit2020) == names(df_profit2021)
    
#    df_all_profits <- rbind(df_profit2020, df_profit2021)
#    names(df_all_profits)
#    nrow(df_all_profits)

#df_all_profits <- df_all_profits %>%
#  mutate(year = year(as.Date(Fiscal.year.period)),
#         month = month(as.Date(Fiscal.year.period))) %>%
  
#  mutate(Customer.Key = as.factor(Customer.Key), 
#         Source.plant.Key = as.factor(Source.plant.Key), Source.plant = as.factor(Source.plant), 
#         Product.Packaging.Key = as.factor(Product.Packaging.Key), Product.Packaging = as.factor(Product.Packaging),
#         Plant.Key = as.factor(Plant.Key), Plant = as.factor(Plant), 
#         Transaction.Type.Key = as.factor(Transaction.Type.Key), Transaction.Type = as.factor(Transaction.Type),
#         Sales.office.customer.Key = as.factor(Sales.office.customer.Key), Sales.office.customer = as.factor(Sales.office.customer), 
#         Deparment.State.customer.Key = as.factor(Deparment.State.customer.Key), Deparment.State.customer = as.factor(Deparment.State.customer),
#         Product.line.Key = as.factor(Product.line.Key), Product.line = as.factor(Product.line), 
#         Material.Key = as.factor(Material.Key), Material = as.factor(Material), 
#         Profit.Center.Key = as.factor(Profit.Center.Key), Profit.Center = as.factor(Profit.Center), 
#         Profit.Center.Zone.Key = as.factor(Profit.Center.Zone.Key), Profit.Center.Zone = as.factor(Profit.Center.Zone),
#         Ship.to.Key = as.factor(Ship.to.Key), Ship.to = as.factor(Ship.to), 
#         City.ship.to.Key = as.factor(City.ship.to.Key), City.ship.to = as.factor(City.ship.to),
#         Shipping.Conditions.Key = as.factor(Shipping.Conditions.Key), Shipping.Conditions = as.factor(Shipping.Conditions),
#         Sales.office.ship.to.Key = as.factor(Sales.office.ship.to.Key), Sales.office.ship.to = as.factor(Sales.office.ship.to), 
#         Fiscal.year.period.Key = as.factor(Fiscal.year.period.Key), Fiscal.year.period = as.factor(Fiscal.year.period), 
#         Deparment.State.ship.to.Key = as.factor(Deparment.State.ship.to.Key), Deparment.State.ship.to = as.factor(Deparment.State.ship.to),
#         Presentation.pack.Key = as.factor(Presentation.pack.Key), Presentation.pack = as.factor(Presentation.pack),
#         Product.type.Key = as.factor(Product.type.Key), Product.type = as.factor(Product.type),
#         Incoterms.Key = as.factor(Incoterms.Key), Incoterms = as.factor(Incoterms))


#######################################################################################
# EDA ---------------------------------------------------------------------------------


############################# TOP CUSTOMERS: 7909, 2851
#FOR PROJECT 4:
#df_Accounts----
df_profit2021_revenue_accounts %>%
  group_by(Name) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(District_Division__c) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(Channel__c) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(Segment__c) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)


#df_profit2021----
df_profit2021_revenue_accounts %>%
  group_by(Deparment.State.ship.to) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(Plant) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(Product.line) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)


df_profit2021_revenue_accounts %>%
  group_by(Material) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(Product.Packaging) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2021_revenue_accounts %>%
  group_by(Presentation.pack) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)


#df_profit2020----
df_profit2020_revenue_accounts %>%
  group_by(Deparment.State.ship.to) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2020_revenue_accounts %>%
  group_by(Plant) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)

df_profit2020_revenue_accounts %>%
  group_by(Product.line) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 3)


############################# TOP CUSTOMERS: 7909, 2851 ----
df_profit2021_revenue_accounts %>%
  group_by(Customer.Key) %>%
  summarise(TOTAL_REVENUE = sum(Revenue)) %>%
  arrange(desc(TOTAL_REVENUE)) %>%
  print(n = 25)
#Customer Key: 7909, 64547, 2851, 2831, 1371, 2853, 1299, 63004, 63666, 6109, 56, 2854


df_profit2021_ebitda %>%
  filter(Totals == "EBITDA") %>%
  group_by(Customer.Key) %>%
  summarise(TOTAL_EBITDA = sum(Actual.accumulated.closing.month.Company.Cur)) %>%
  arrange(desc(TOTAL_EBITDA)) %>%
  print(n = 25)
#Customer Key: 1232, 2854, 1372, 7909, 1258, 2851, 585, 63077, 63221, 1421, 63128, 1337


df_profit2021_quantity_sold %>%
  filter(Totals == "QUANTITY SOLD") %>%
  group_by(Customer.Key) %>%
  summarise(TOTAL_SALES = sum(Actual.accumulated.closing.month.Company.Cur)) %>%
  arrange(desc(TOTAL_SALES)) %>%
  print(n = 25)
#Customer Key: 3390, 97058, 7909, 2831, 1371, 2851, 64547, 63004, 6109, 1299, 63666, 2853

############################ MONTHLY SALES----


df_profit2020 %>%
  filter(Totals == "REVENUE") %>%
  group_by(Fiscal.year.period) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = 25)


df_profit2021 %>%
  filter(Totals == "REVENUE") %>%
  group_by(Fiscal.year.period) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = 25)


df_all_profits %>%
  filter(Totals == "REVENUE") %>%
  group_by(year, month) %>%
  summarise(TOTAL_REVENUE = sum(Actual.accumulated.closing.month.Company.Cur)) %>%
  arrange(month) %>%
  print(n = 25)


df_all_profits %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  arrange(month) %>%
  print(n = 25)


ggplot(df_all_profits %>%
         filter(Totals == "REVENUE"), 
       mapping = aes(x = month, fill = year)) +
  geom_bar(position = "stack")

###############################################################################
# TRAINING MODELS
#################

library(caret)

sapply(df_profit2021_revenue_accounts, function(x) sum(is.na(x)))
any(is.na(df_profit2021_revenue_accounts))


split_data_cleaning <- createDataPartition(y = df_profit2021_revenue_accounts$Revenue,
                                           p = 0.8, list = F, groups = 1000)
train_data_cleaning <- df_profit2021_revenue_accounts[split_data_cleaning, ]
test_data_cleaning <- df_profit2021_revenue_accounts[-split_data_cleaning, ]


################################################################################
#Identify best predictors by subsetting features--------------------------------
library(leaps)
best_subsets <- regsubsets(
  Revenue ~ Customer.Key + Source.plant.Key + Product.Packaging.Key +
    Plant.Key + Transaction.Type.Key + Sales.office.customer.Key +
    Deparment.State.customer.Key + Product.line.Key + Material.Key + 
    Profit.Center.Key + Profit.Center.Zone.Key + Ship.to.Key + 
    City.ship.to.Key + Shipping.Conditions.Key + Sales.office.ship.to.Key +
    Fiscal.year.period.Key + Deparment.State.ship.to.Key + 
    Presentation.pack.Key + Product.type.Key + Segment__c_NUM,
  data = train_data_cleaning
)
summary(best_subsets)
#Results:
#NOT THE BEST APPROACH AS IT VARIES SIGNIFICANTLY
#Revenue ~ Product.Packaging.Key + Plant.Key + Source.plant.Key + 
#   Transaction.Type.Key + Sales.office.customer.Key + Shipping.Conditions.Key + 
#   Ship.to.Key + Deparment.State.ship.to.Key


################################################################################
#MODEL 1 ###########################
#FEATURE SELECTION WITH ALL FEATURES
mod1 <- lm(Revenue ~ Customer.Key + Source.plant.Key + Product.Packaging.Key + 
             Plant.Key + Transaction.Type.Key + Deparment.State.customer.Key + 
             Sales.office.customer.Key + Profit.Center.Key + 
             Profit.Center.Zone.Key + Sales.office.ship.to.Key + 
             Fiscal.year.period.Key + Deparment.State.ship.to.Key + 
             Product.line.Key + Material.Key + Ship.to.Key + 
             City.ship.to.Key + Shipping.Conditions.Key + 
             Presentation.pack.Key + Product.type.Key + Incoterms.Key + 
             Segment__c_NUM, 
           data = train_data_cleaning)

summary(mod1)
#Results:
#SIGNIFICANT VARIABLES
#Revenue ~ Customer.Key + Source.plant.Key + Product.Packaging.Key + 
#   Transaction.Type.Key + Profit.Center.Key + Profit.Center.Zone.Key + 
#   Sales.office.ship.to.Key + Material.Key + Ship.to.Key +
#   Shipping.Conditions.Key + Product.type.Key + Incoterms.Key + Segment__c_NUM


#FORWARD STEPWISE----
start_mod1 <- lm(Revenue~1, data = train_data_cleaning)
empty_mod1 <- lm(Revenue~1, data = train_data_cleaning)
full_model1 <- mod1
forwardStepwise <- step(start_mod1,
                        scope=list(upper = full_model1, lower = empty_mod1),
                        direction = 'forward')
#Results: AIC=732684.7
#Revenue ~ Product.Packaging.Key + Plant.Key + Transaction.Type.Key + 
#   Shipping.Conditions.Key + Incoterms.Key + Profit.Center.Zone.Key + 
#   Source.plant.Key + Deparment.State.ship.to.Key + Sales.office.ship.to.Key + 
#   Ship.to.Key + Material.Key + Profit.Center.Key + Customer.Key + 
#   Product.type.Key + Segment__c_NUM


#BACKWARD STEPWISE----
start_mod2 <- mod1
empty_mod2 <- lm(Revenue~1, data = train_data_cleaning)
full_mod2 <- mod1
backwardStepwise <- step(start_mod2,
                         scope=list(upper=full_mod2,lower=empty_mod2),
                         direction='backward')
#Results: AIC=732684.7
#Revenue ~ Customer.Key + Source.plant.Key + Product.Packaging.Key + 
#   Transaction.Type.Key + Profit.Center.Key + Profit.Center.Zone.Key + 
#   Sales.office.ship.to.Key + Deparment.State.ship.to.Key + 
#   Material.Key + Ship.to.Key + Shipping.Conditions.Key + Product.type.Key + 
#   Incoterms.Key + Segment__c_NUM


#HYBRID STEPWISE----
start_mod3 <- lm(Revenue~1, data = train_data_cleaning)
empty_mod3 <- lm(Revenue~1, data = train_data_cleaning)
full_model3 <- mod1
hybridStepwise3 <- step(start_mod3,
                        scope=list(upper=full_model3,lower=empty_mod3),
                        direction='both')
#Results: AIC=732684.7
#Revenue ~ Product.Packaging.Key + Transaction.Type.Key + Shipping.Conditions.Key + 
#   Incoterms.Key + Profit.Center.Zone.Key + Source.plant.Key + 
#   Deparment.State.ship.to.Key + Sales.office.ship.to.Key + Ship.to.Key + 
#   Material.Key + Product.type.Key + Profit.Center.Key + Customer.Key +
#   Product.type.Key + Segment__c_NUM



################################################################################
#MODEL 2 ###########################
#Selected features with at least a correlation of 0.21
#-customer.key,
#-plant.key, -transaction.type.key, -sales.office.customer.key, -state.key, 
#-profit.center.key, -profit.center.zone.key, -sales.office.ship.to, 
#-fiscal.year.period.key, -department/state.ship.to, -incoterms.key
mod2 <- lm(Revenue ~ Source.plant.Key + Product.Packaging.Key + 
             Deparment.State.customer.Key + 
             Sales.office.ship.to.Key + 
             Deparment.State.ship.to.Key + 
             Product.line.Key + Material.Key + Ship.to.Key + 
             City.ship.to.Key + Shipping.Conditions.Key + 
             Presentation.pack.Key + Product.type.Key + 
             Segment__c, 
           data = train_data_cleaning)

summary(mod2)


#FORWARD STEPWISE----
start_mod1 <- lm(Revenue~1, data = train_data_cleaning)
empty_mod1 <- lm(Revenue~1, data = train_data_cleaning)
full_model2 <- mod2
forwardStepwise <- step(start_mod1,
                        scope=list(upper = full_model1, lower = empty_mod1),
                        direction = 'forward')
#Results: AIC=733014.9
#Revenue ~ Product.Packaging.Key + Plant.Key + Transaction.Type.Key + 
#   Shipping.Conditions.Key + Incoterms.Key + Profit.Center.Zone.Key + 
#   Source.plant.Key + Ship.to.Key + Deparment.State.ship.to.Key + 
#   Sales.office.ship.to.Key + Material.Key + Product.type.Key + 
#   Customer.Key + Profit.Center.Key + Presentation.pack.Key


#BACKWARD STEPWISE----
start_mod2 <- mod2
empty_mod2 <- lm(Revenue~1, data = train_data_cleaning)
full_mod2 <- mod2
backwardStepwise <- step(start_mod2,
                         scope=list(upper=full_mod2,lower=empty_mod2),
                         direction='backward')
#Results: AIC=734431.2
#Revenue ~ Source.plant.Key + Product.Packaging.Key + Deparment.State.customer.Key + 
#   Sales.office.ship.to.Key + Deparment.State.ship.to.Key + 
#   Product.line.Key + Material.Key + Ship.to.Key + Shipping.Conditions.Key + 
#   Presentation.pack.Key + Product.type.Key


#HYBRID STEPWISE----
start_mod3 <- lm(Revenue~1, data = train_data_cleaning)
empty_mod3 <- lm(Revenue~1, data = train_data_cleaning)
full_model3 <- mod2
hybridStepwise3 <- step(start_mod3,
                        scope=list(upper=full_model3,lower=empty_mod3),
                        direction='both')
#Results: AIC=734431.2
#Revenue ~ Product.Packaging.Key + Source.plant.Key + Ship.to.Key + 
#   Shipping.Conditions.Key + Sales.office.ship.to.Key + Deparment.State.ship.to.Key + 
#   Material.Key + Product.type.Key + Deparment.State.customer.Key + 
#   Presentation.pack.Key + Product.line.Key

























