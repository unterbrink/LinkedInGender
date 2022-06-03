# LinkedIn Network Gender Balance Estimation
Estimate gender balance of your LinkedIn network. Output is an estimate of the gender balance of your network by year added and cumulatively. US Social Security data is used to assign gender based on first name.

## Limitations
There are many limitations to this approach. To name a few:
1. The example below estimates gender based on US data from 1930 - 2012, from the Social Security Administration. It is less effective if you have many connections without common American names.
2. Many first names are not gender specific (especially nicknames) so there is a risk for misclassification.
3. Some names cannot be classified, because (A) they have no match in the reference list or (B) The LinkedIn user uses an initial or other non-standard format. In my case ~11% of my contacts were not classified ("NA").
4. This approach does not properly account for non-binary individuals.

Inspired by: 
* https://www.zylstra.org/blog/2019/12/my-linkedin-network-and-its-gender-balance/d Manual review of LinkedIn followers
* https://www.pdf-archive.com/2017/12/12/analysis-of-gender-on-twitter-to-uk-politics-journalists/analysis-of-gender-on-twitter-to-uk-politics-journalists.pdf "Both male and female influencer groups analysed exhibited an overall tendency to follow
male journalists more frequently than female journalists. Even after adjusting for demographics the
ratio was 57:43"
* [The Authority Gap, by Mary Ann Sieghart](https://books.google.co.uk/books/about/The_Authority_Gap_Why_Women_Are_Still_Ta.html?id=EUM3EAAAQBAJ&source=kp_book_description&redir_esc=y)

# Step 1: 
* Download your data connections data from LinkedIn [(1)](https://www.linkedin.com/help/linkedin/answer/a566336/export-connections-from-linkedin?lang=en)

# Step 2: Install packages
* Install gender package to predict names from historical data [(2)](https://cran.r-project.org/web/packages/gender/gender.pdf)
* Install janitor package to clean up variable names
```R
install.packages("gender")
install.packages("janitor")
remotes::install_github("lmullen/genderdata")
library(gender) # gender
library(ggplot2) # graphs
library(lubridate) # manipluate dates
library(dplyr) # manipulate data
library(readr) # importing the LinkedIn data
library(janitor) # clean up variable names
library(stringr) # dealing with strings
library(tidyr) # data wrangling for graphs
```
# Step 3: Upload and clean data
* Upload your LinkedIn Data Export. Rename the file to *LIData.zip*.
```R
LIdata <- read_csv("LIData.zip", skip = 3) # The LinkedIn file has metadata 3 rows we will omit. Use "skip = 3" to ignore these rows
LIdata <- clean_names(LIdata) # Cleaning up variable names to remove spaces and capitalization
head(LIdata) # Check out the data
summary(LIdata)
colMeans(is.na(LIdata)) # Check for missing data
```
In my data, < 1% were missing first and last names, 99% were missing email, and 1% were missing company and position. Drop data without first name (on review of the raw data, it looks like these are people who have left LinkedIn -- only connection date is present for these rows).
```R
# Clean up data
LIdata <- LIdata %>% 
  filter(!is.na(first_name)) %>% # Filter out rows where first_name is missing
  mutate(connected_on = lubridate::dmy(connected_on), # Convert connected_on from string to date format
    year = year(connected_on), # Create a variable "year" to use later
    first_names = first_name,
    first_names = str_remove(first_names, "^Dr. "), # Some people have "Dr." in their first name, remove this
    first_names = sub("-.*", "", first_names), # Some people have hyphenated first names, remove this
    first_names = sub(" .*", "", first_names), # Some people have double first names, remove this
    first_names = str_trim(first_names)) # Trim any extra spaces
```
# Step 4: Predict gender from first names
* Run gender function on each row [(3)](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html)
* "ssa" is US data from 1930 - 2012, from the Social Security Administration [(4)](https://www.ssa.gov/oact/babynames/limits.html)
```R
LIdata_gender <- LIdata %>% 
  distinct(first_names) %>% 
  do(results = gender(.$first_names, method = "ssa")) %>% 
  do(bind_rows(.$results))  %>% 
  select(name, gender)
summary(LIdata_gender)
# Join results with original dataset
fullDF <- left_join(LIdata, LIdata_gender, by = c("first_names" = "name"))
```
# Step 5: Explore the results
```R
# Graph showing gender of connections by year added
g <- ggplot(data = fullDF, aes(x = year, fill = gender)) +
  geom_bar(position="fill") +
  labs(x = "Year", y = "Percentage", fill = "ID'd Gender",
    title = "New connections by year and ID'd Gender") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))
g
```
![percentage](https://user-images.githubusercontent.com/19696619/171907390-1ca2b144-96a5-4db8-a9b1-046e22f44db6.png)
##### Example interpretation: The percentage of my new connections each year who are classified as female has decreased about 15% betweet 2008 and 2022 (from 40% to 25%). The percentage classified as male has increased approximately 10% to 61% in 2022, and the proportion not classified has increased to 14%.
```R
# Graph cumulative percentage of male connections
#Manipulate the data -- note: this is defninitely not the most elegant approach
cummPercent <- fullDF  %>% 
  select(gender, year) %>% 
  group_by(year, gender) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  spread(gender, n) %>%
  mutate(total = female + male, # ommitting NA
    cummTot = cumsum(total), 
    cummFemale = cumsum(female),
    cummPercent = cummFemale / cummTot)
#Plot percentage
g2 <- ggplot() + 
  geom_line(data = cummPercent, aes(year, y = cummPercent)) + 
  labs(title = "Percentage of total LinkedIn connections ID'd Female", y = "Percentage", x = "Year")  +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"),
    breaks = c(0.2, 0.3, 0.4, 0.5), limits = c(0.25, 0.50))
g2
```
![cumulative](https://user-images.githubusercontent.com/19696619/171907358-2bb285f1-33c5-43f8-901b-9192c64edd30.png)
##### Example interpretation: Among all of my connections, the % classified as female was about 45% between 2008 and 2016, and then dropped to <35%. Note: this calculation omits "NA", so a more accurate title would be "Percentage of total classified LinkedIn connections that were classified as female"

# References
(1) https://www.linkedin.com/help/linkedin/answer/a566336/export-connections-from-linkedin?lang=en

(2) https://cran.r-project.org/web/packages/gender/gender.pdf

(3) https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html

(4) https://www.ssa.gov/oact/babynames/limits.html
