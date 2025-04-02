

trait_data_to_impute <- read.csv("C:/Users/ah2255/Documents/part_2_project/traits_for_impute.csv")
#columns to impute - family = column 1, genus = column 2, wood density = column 5, average CN = column 6
#comp strength = column 7, fibre length = column 8 

#will be using predictive mean matching
#excluding columns 3 and 4 (species and subspecies) from the imputation 
#then imputing in the order of the spreadsheet 
#do 15 imputations, then a random number generator to select the main one for the analysis
library(mice)

trait_data_to_feed_impute <- read.csv("C:/Users/ah2255/Documents/part_2_project/traits_for_impute.csv")

# Excluding Species and Subsp columns from imputation
cleaned_data_to_impute <- trait_data_to_feed_impute[, !(names(trait_data_to_feed_impute) %in% c("Species", "Subsp"))]


imputed_data <- mice(cleaned_data_to_impute, method = "pmm", m = 15, seed = 11229)

#pmm still seems to be causing clustering albeit less than before - look into other methods which do not assume relationships between the 
#data

#checking if the imputation has converged:

plot(imputed_data)

longform_imputed_data <- complete(imputed_data, "long", include = TRUE)

write.csv(longform_imputed_data, "C:/Users/ah2255/Documents/part_2_project/outputs/imputed_data_longform.csv", row.names = FALSE)

print(imputed_data$loggedEvents)


random_numbers <- sample(1:15, 1, replace = FALSE)
print(random_numbers)
#number returned = 4
#main analysis to be conducted on imputation 4


#trying to increase the donor pool for the imputation (10 of the nearest neighbours) to see if this reduces clustering: 

imputed_data_donors <- mice(cleaned_data_to_impute, method = "pmm", donors = 10, m = 15, seed = 1122918924)


longform_imputed_data_with_more_donors <- complete(imputed_data_donors, "long", include = TRUE)

#still clusters 

#trying random forest imputation  

imputed_data_random_forest <- mice(cleaned_data_to_impute, method = "rf", m = 15, seed = 112294)
plot(imputed_data_random_forest)

longform_imputed_data_random_forest <- complete(imputed_data_random_forest, "long", include = TRUE)

write.csv(longform_imputed_data_random_forest, "C:/Users/ah2255/Documents/part_2_project/outputs/imputed_data_longform_random_forest.csv", row.names = FALSE)


#random forest is much better in terms of clustering, but there is still some clustering 
#we can try to overcome this by a) increasing the number of decision trees and b) increasing the depth of the trees 

#starting by reducing minibucket by 1 (the default is 5), and increasing the ntree value (to 500...this is going to take a long time)
#so 500 decision trees

#look over minibuckets

imputed_data_rf_minibucket_ntree <- mice(cleaned_data_to_impute, method = "rf", ntree = 500, minbucket = 4, m = 15, seed = 1234)

plot(imputed_data_rf_minibucket_ntree)

longform_imputed_data_rf_minibucket_ntree <- complete(imputed_data_rf_minibucket_ntree, "long", include = TRUE)

#this is the final imputed dataset using a random forest approach with fine-tuning

write.csv(longform_imputed_data_rf_minibucket_ntree, "C:/Users/ah2255/Documents/part_2_project/outputs/imputed_data_longform_rf_minibucket_ntree.csv", row.names = FALSE)






