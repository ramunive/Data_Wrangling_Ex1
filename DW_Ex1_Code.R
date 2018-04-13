# load packages
library("dplyr")
library("stringr")
library("tidyr")

# load the original data
refine_original <- read.csv("refine_original.csv")

# create a variable to save the cleaned data
refine <- refine_original

# explore data

# examine structure of dataset
str(refine)
# A dataframe as expected
# 25 observations of 6 variables
# company has 19 levels, but there should be only four brands.  Need to clean up company column

head(refine)

summary(refine)

# 1. Clean up brand names

# Find all variants of the company names
unique(refine$company)

# Save the original company names vector

original_company <- refine_original$company

# Find all variants of the Philips company name and standardize them to philips
# All variants of Philips company name end in either ps or pS

# Find Philips variants ending in "ps" or "pS"
new_name <- grepl(".ps$", original_company, ignore.case = TRUE)

# Replace with standardized company name and check results
refine$company[new_name] = "philips"
refine$company

# All variants of Akzo company name contain a "Z" or "z"

# Find Akzo variants containing "z" or "Z"
new_name <- grepl(".z.", original_company, ignore.case = TRUE)

# Replace with standardized company name and check results
refine$company[new_name] = "akzo"
refine$company

# Replace with standardized company name and check results
refine$company[new_name] = "akzo"
refine$company

# All variants of Van Houten company name end in "n"

# Find Van Houten variants containing "n"
new_name <- grepl(".n$", original_company, ignore.case = TRUE)

# Replace with standardized company name and check results
refine$company[new_name] = "van houten"
refine$company

# All variants of Unilever company name start with "u" or "U"

# Find Unilever variants containing "u" or "U"
new_name <- grepl("^u.", original_company, ignore.case = TRUE)

# Replace with standardized company name and check results
refine$company[new_name] = "unilever"
refine$company

# Replace with standardized company name and check results
refine$company[new_name] = "unilever"
refine$company

# Compare the number of unique names in the original company column to the clean company column
unique(original_company)
unique(refine$company)

# 2. Separate product code and number

# Examine product code and product number format
head(refine)

# product code and product number are separated by a hyphen.  For example, p-5
# Separate product code and product number using the hyphen
# Split the Product.code...number column into two columns
# Name the new columns product_code and product_number
# Remove the Product.code...number column
# Save the changes into refine
refine <- separate(refine, Product.code...number, c("product_code", "product_number"), sep = "-", remove = TRUE)


# 3. Add product categories
# p = Smartphone, v = TV, x = Laptop, q = Tablet

# Create a vector of product codes
product_category <- refine$product_code
product_category


# Convert the product codes to product categories
# Store product categories in product_category vector
product_function <- function (x){

     if (x == "p"){
       x = "Smartphone"
     } else if (x == "v"){
       x = "TV"
     } else if (x == "x"){
       x = "Laptop"
     } else if (x == "q"){
       x = "Tablet"
     }
}

product_category <- sapply(product_category, product_function, simplify = TRUE, USE.NAMES = FALSE)
product_category

# Add product category column to refine dataframe
refine <- cbind(refine, product_category)

# Verify change
head(refine)

# 4. Add full address for geocoding
# Create a new column full_address that concatenates the 3 address fields
# The 3 address fields are address, city, country
# Separate the address components by commas
# Save changes to refine dataframe

refine <- unite(refine, "full_address", address, city, country, sep = ",")

# Verify change
head(refine)

# 5. Create dummy variables for company and product category
# Both the company name and product category are categorical variables i.e, they take only a fixed set of values.
# In order to use them in further analysis, create dummy variables
# Create dummy binary variables for each of them

# 5.1 Add 4 binary (1 or 0) columns for company: company_philips,
# company_akzo, company_van_houten and company_unilever


# binary_function will return a vector of binary values.  
# the vector element will have a 1 if company_or_product is present
# the vector element will have a 0 if company_or_product is not present

binary_function <- function(x, company_or_product){
  
  out <- vector("logical", length(x))
  
  for (i in 1:length(x)) {
    if (x[i] == company_or_product){
      out[i] = 1
    } else {
      out[i] = 0
    }
  }
  out
}

# Create company_philips column and make it binary

company_philips <- binary_function(refine$company, "philips")

# Check output
company_philips

# Add company_philips column to refine

refine <- cbind(refine, company_philips)

# Create company_akzo column and make it binary

company_akzo <- binary_function(refine$company, "akzo")

# Check output
company_akzo

# Add company_akzo column to refine

refine <- cbind(refine, company_akzo)

# Create company_van_houten column and make it binary

company_van_houton <- binary_function(refine$company, "van houten")

# Check output
company_van_houton

# Add company_van_houton column to refine

refine <- cbind(refine, company_van_houton)

# Create company_unilever column and make it binary

company_unilever <- binary_function(refine$company, "unilever")

# Check output
company_unilever

# Add company_unilever column to refine

refine <- cbind(refine, company_unilever)

# 5.2 Add 4 binary (1 or 0) columns for product category:
# product_smartphone, product_tv, product_laptop, and product_tablet

# Create product_smartphone column and make it binary

product_smartphone <- binary_function(refine$product_category, "Smartphone")

# Check output
product_smartphone

# Add product_smartphone column to refine

refine <- cbind(refine, product_smartphone)

# Create product_tv column and make it binary

product_tv <- binary_function(refine$product_category, "TV")

# Check output
product_tv

# Add product_tv column to refine

refine <- cbind(refine, product_tv)

# Create product_laptop column and make it binary

product_laptop <- binary_function(refine$product_category, "Laptop")

# Check output
product_laptop

# Add product_laptop column to refine

refine <- cbind(refine, product_laptop)

# Create product_tablet column and make it binary

product_tablet <- binary_function(refine$product_category, "Tablet")

# Check output
product_tablet

# Add product_tablet column to refine

refine <- cbind(refine, product_tablet)

# This is refine
str(refine)


# Company column still shows 19 levels.  Correct the number of levels
refine$company <- factor(x = refine$company, levels = unique(refine$company))

# Product code should be Factor, not chr
refine$product_code <- factor(x = refine$product_code, levels = unique(refine$product_code))

# Product number should be Factor, not chr
refine$product_number <- factor(x = refine$product_number, levels = unique(refine$product_number))

# full_address should be Factor not chr
refine$full_address <- factor(x = refine$full_address, levels = unique(refine$full_address))

# company_philips needs to be restricted to 2 levels:  0 and 1
refine$company_philips <- factor(x = refine$company_philips, levels = c(0, 1))

# company_akzo needs to be restricted to 2 levels:  0 and 1
refine$company_akzo <- factor(x = refine$company_akzo, levels = c(0, 1))

# company_van_houton needs to be restricted to 2 levels:  0 and 1
refine$company_van_houton <- factor(x = refine$company_van_houton, levels = c(0, 1))

# company_unilever needs to be restricted to 2 levels:  0 and 1
refine$company_unilever <- factor(x = refine$company_unilever, levels = c(0, 1))

# product_smartphone needs to be restricted to 2 levels:  0 and 1
refine$product_smartphone <- factor(x = refine$product_smartphone, levels = c(0, 1))

# product_tv needs to be restricted to 2 levels:  0 and 1
refine$product_tv <- factor(x = refine$product_tv, levels = c(0, 1))

# product_laptop needs to be restricted to 2 levels:  0 and 1
refine$product_laptop <- factor(x = refine$product_laptop, levels = c(0, 1))

# product_tablet needs to be restricted to 2 levels:  0 and 1
refine$product_tablet <- factor(x = refine$product_tablet, levels = c(0, 1))

# Verify there are no changes to refine_original
str(refine_original)

# Verify changes to refine
str(refine)


# Store cleaned up data set to the file refine_clean.csv
write.csv(refine, "refine_clean.csv", row.names = FALSE)

