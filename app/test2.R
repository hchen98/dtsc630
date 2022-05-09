setwd("/home/hui/Projects/dtsc630_main/app")
data <- read.csv('Categories_KW_Normalized_test.csv')
# data <- Categories_KW_Normalized_test

df <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(df) <-
  c(
    'Cateogry',
    'Major',
    'Major_W',
    'Specialty',
    'Specialty_W',
    'Tool',
    'Tool_W',
    'Trait',
    'Trait_W',
    'Environment',
    'Environment_W'
  )

#New csv creation
for (i in 1:nrow(data)) {
  category <- data$Category[i]# for-loop over rows
  
  major <- as.list(strsplit(data$Major, "', '")[[i]])
  major <-
    c(lapply(major, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  #print(major)
  
  major_w <- as.list(strsplit(data$Major_Weights, "', '")[[i]])
  major_w <-
    c(lapply(major_w, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  specialty <- as.list(strsplit(data$Specialty, "', '")[[i]])
  specialty <-
    c(lapply(specialty, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  specialty_w <-
    as.list(strsplit(data$Specialty_Weights, "', '")[[i]])
  specialty_w <-
    c(lapply(specialty_w, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  tool <- as.list(strsplit(data$Tool, "', '")[[i]])
  tool <-
    c(lapply(tool, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  tool_w <- as.list(strsplit(data$Tool_Weights, "', '")[[i]])
  tool_w <-
    c(lapply(tool_w, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  trait <- as.list(strsplit(data$Trait, "', '")[[i]])
  trait <-
    c(lapply(trait, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  trait_w <- as.list(strsplit(data$Trait_Weights, "', '")[[i]])
  trait_w <-
    c(lapply(trait_w, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  env <- as.list(strsplit(data$Environment, "', '")[[i]])
  env <-
    c(lapply(env, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  env_w <- as.list(strsplit(data$Environment_Weights, "', '")[[i]])
  env_w <-
    c(lapply(env_w, function(x)
      gsub("\\[", "", gsub(
        "\\]", "", gsub("'", "", x)
      ))))
  
  df[i, ] <-
    list(
      category,
      list(major),
      list(as.numeric(major_w)),
      list(specialty),
      list(as.numeric(specialty_w)),
      list(tool),
      list(as.numeric(tool_w)),
      list(trait),
      list(as.numeric(trait_w)),
      list(env),
      list(as.numeric(env_w))
    )
}


WC_allSubCat <- function(cat, df) {
  "Generate wordcloud for all sub-category for a specific category of job"
  
  # note: the length of word, n and col_ must match
  word <- c()
  n <- c()
  col_ <- c()
  
  temp1 <- c(unlist(df$Major[df$Cateogry == cat]))
  
  # Sub-category: major
  new_w <- c(unlist(df$Major[df$Cateogry == cat]))
  word <- c(append(word, new_w))
  
  new_n <- c(unlist(df$Major_W[df$Cateogry == cat]))
  n <- c(append(n, new_n))
  
  for (x in 1:length(temp1)) {
    col_ <- c(append(col_, "#999999"))
  }
  
  temp2 <- c(unlist(df$Specialty[df$Cateogry == cat]))
  new_w <- c(unlist(df$Specialty[df$Cateogry == cat]))
  word <- c(append(word, new_w))
  
  new_n <- c(unlist(df$Specialty_W[df$Cateogry == cat]))
  n <- c(append(n, new_n))
  
  for (x in 1:length(temp2)) {
    col_ <- c(append(col_, "#777777"))
  }
  
  temp3 <- c(unlist(df$Tool[df$Cateogry == cat]))
  new_w <- c(unlist(df$Tool[df$Cateogry == cat]))
  word <- c(append(word, new_w))
  
  new_n <- c(unlist(df$Tool_W[df$Cateogry == cat]))
  n <- c(append(n, new_n))
  
  for (x in 1:length(temp3)) {
    col_ <- c(append(col_, "#555555"))
  }
  
  temp4 <- c(unlist(df$Trait[df$Cateogry == cat]))
  new_w <- c(unlist(df$Trait[df$Cateogry == cat]))
  word <- c(append(word, new_w))
  
  new_n <- c(unlist(df$Trait_W[df$Cateogry == cat]))
  n <- c(append(n, new_n))
  
  for (x in 1:length(temp4)) {
    col_ <- c(append(col_, "#333333"))
  }
  
  temp5 <- c(unlist(df$Environment[df$Cateogry == cat]))
  new_w <- c(unlist(df$Environment[df$Cateogry == cat]))
  word <- c(append(word, new_w))
  
  new_n <- c(unlist(df$Environment_W[df$Cateogry == cat]))
  n <- c(append(n, new_n))
  
  for (x in 1:length(temp5)) {
    col_ <- c(append(col_, "#111111"))
  }
  
  data <- data.frame((word), n, col_)
  # data2 <- data[order(data$freq, decreasing = TRUE), ]
  
  return(data$col_)
}

temp_df <- WC_allSubCat("program management", df)
print(length(temp_df))
print(temp_df)

# temp <- c(unlist(df$Environment_W[df$Cateogry == "program management"]))
# temp2 <- c(unlist(df$Tool_W[df$Cateogry == "program management"]))

temp3 <- c(append(temp, temp2))
print(temp3)
print(length(temp3))
# print(df)
