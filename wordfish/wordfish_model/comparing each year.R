library(ggplot2)

load_and_rename <- function(file_path, new_var_name) {
  load(file_path)            
  assign(new_var_name, wf, envir = .GlobalEnv)  
  rm(wf)                      
}

file_paths <- c("C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2548.RData",
                "C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2551.RData",
                "C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2553.RData",
                "C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2555.RData",
                "C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2562.RData",
                "C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2563.RData",
                "C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/wordfish_model/wordfish_model_2564.RData")

new_names <- c("wf_2005", "wf_2008", "wf_2010", "wf_2012", "wf_2019", "wf_2021", "wf_2023")

for (i in seq_along(file_paths)) {
  load_and_rename(file_paths[i], new_names[i])
}

theta2005 <- wf_2005$theta
theta2008 <- wf_2008$theta
theta2010 <- wf_2010$theta
theta2012 <- wf_2012$theta
theta2019 <- wf_2019$theta
theta2021 <- wf_2021$theta
theta2023 <- wf_2023$theta


# List your Wordfish models
wf_models <- list(wf_2005, wf_2008, wf_2010, wf_2012, wf_2019, wf_2021, wf_2023)

# Names for the models to assign years
names(wf_models) <- c("2005", "2008", "2010", "2012", "2019", "2021", "2023")

# create dataframe
combined <- data.frame(
  score = c(theta2005, theta2008, theta2010, theta2012, theta2019, theta2021, theta2023),
  year = factor(rep(c("2005", "2008", "2010", "2012", "2019", "2021", "2023"), 
                    times = c(length(theta2005), length(theta2008), length(theta2010), 
                              length(theta2012), length(theta2019), length(theta2021), length(theta2023)))))

# Plot
ggplot(combined, aes(x = year, y = score)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, color = "blue") +
  labs(title = "Comparing Wordfish Scores", x = "Year", y = "Wordfish Score")





