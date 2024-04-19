library(countrycode)

# HTTP request methods
request_methods <- c("GET", "POST")

# HTTP status codes
status_codes <- c(200, 304, 404, 500)

# User genders
genders <- c("Male", "Female", "Non-binary")

# List of paths from the FunOlympics broadcast platform
paths <- c("/index.html", "/about.html", "/athletes.php", "/medals.php", "/schedule.php", "/results.php",
           "/searchsports.php", "/basketball.php", "/table-tennis.php", "/tennis.php", "/athletics/track.php", 
           "/athletics/field.php", "/volleyball.php", "/cycling.php", "/diving.php", "/gymnastics.php", "/weightlifting.php",
           "/rowing.php", "/football.php", "/swimming.php", "/water-polo.php", "/wrestling.php", "/karate.php")

# List of countries
countries <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", 
               "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", 
               "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", 
               "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", 
               "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", 
               "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros", 
               "Congo (Congo-Brazzaville)", "Costa Rica", "Croatia", "Cuba", "Cyprus", 
               "Czechia (Czech Republic)", "Democratic Republic of Congo", "Denmark", 
               "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", 
               "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", 
               "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", 
               "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Holy See", 
               "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", 
               "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", 
               "Kiribati", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", 
               "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", 
               "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Micronesia", 
               "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", 
               "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", 
               "Nigeria", "North Korea", "North Macedonia", "Norway", "Oman", "Pakistan", 
               "Palau", "Palestine State", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", 
               "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
               "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", 
               "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", 
               "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Tajikistan", 
               "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
               "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Uzbekistan", 
               "Vanuatu", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")


# Function to generate simulated web server log entries
generate_log_data <- function() {
  start_time <- as.POSIXct("00:00:00", format="%T")
  end_time <- as.POSIXct("23:59:59", format="%T")
  time <- as.POSIXct(runif(1, start_time, end_time), origin="1970-01-01")
  timestamp <- format(time, "%H:%M:%S")
  ip_address <- paste(sample(0:255, 4, replace = TRUE), collapse = ".")
  http_method <- sample(request_methods, 1)
  path <- sample(paths, 1)
  status_code <- sample(status_codes, 1, prob=c(0.6, 0.2, 0.1, 0.1))
  country <- sample(countries, 1)
  continent <- countrycode::countrycode(country, "country.name", "continent")
  gender <- sample(genders, 1)
  age <- sample(16:80, 1)
  
  log_entry <- paste(timestamp, ip_address, http_method, path, status_code, 
                     country, continent, gender, age, sep = ", ")
  return(log_entry)
}

# Function to generate a log file 
generate_log_file <- function(num_entries) {
  con <- file("log_data.csv", "w")
  header <- "time, ip_address, request_method, path, status_code, country, continent, gender, age"
  cat(header, "\n", file = con)
  for (i in 1:num_entries) {
    log_entry <- generate_log_data()
    cat(log_entry, "\n", file = con)
    print(log_entry)
  }
  close(con)
}
  
# Generate a log file with 100000 entries
generate_log_file(100000)
  
    