# Upload original data
original_fish <- read.csv("fish.csv")

# Clean up data by removing incorrect measurement of a fish with 0 weight
fish <- original_fish[-41, ]

# Scatter plot matrix of all variables
# We do not plot the Species variable since it is non-numeric
plot(fish[, -1])

# lm using all variables
model <- lm(Weight ~ Length1 + Length2 + Length3 + Height + Width, data = fish)
summary(model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# Transformation
transformation <- powerTransform(cbind(fish$Weight, fish$Length1, fish$Length2, fish$Length3, fish$Height, fish$Width) ~ 1)
summary(transformation)

# New model
## New data containing transformed variables
fish_transformed <- data.frame("Species" = fish$Species,
                               "Transformed_Weight" = fish$Weight^.07,
                               "Transformed_Length1" = log(fish$Length1),
                               "Transformed_Length2" = fish$Length2^.17,
                               "Transformed_Length3" = log(fish$Length3),
                               "Transformed_Height" = log(fish$Height),
                               "Transformed_Width" = fish$Width^.33)
transformed_model <- lm(Transformed_Weight ~ Transformed_Length1 + Transformed_Length2 + Transformed_Length3 + Transformed_Height + Transformed_Width, data = fish_transformed)
summary(transformed_model)

# Model with insignificant variables removed
reduced_model <- lm(Transformed_Weight ~ Transformed_Length2 + Transformed_Height + Transformed_Width, data = fish_transformed)
summary(reduced_model)

