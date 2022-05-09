# set the working dir, make a new line of your working dir
setwd("/home/hui/Projects/dtsc630_main")      # Henry's wd

# read the dataset
data <- read.csv('./datasets/Categories_KW_Normalized_test.csv')

df <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(df)<-c('Cateogry', 'Major', 'Major_W', 'Specialty', 'Specialty_W', 
                'Tool', 'Tool_W', 'Trait', 'Trait_W', 'Environment', 'Environment_W')

#New csv creation
for(i in 1:nrow(data)) {
  
  category<- data$Category[i]# for-loop over rows
  
  major<- as.list(strsplit(data$Major, "', '")[[i]]) 
  major<- c(lapply(major, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  #print(major)
  
  major_w<- as.list(strsplit(data$Major_Weights, "', '")[[i]]) 
  major_w<- c(lapply(major_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  specialty<- as.list(strsplit(data$Specialty, "', '")[[i]]) 
  specialty<- c(lapply(specialty, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  specialty_w<- as.list(strsplit(data$Specialty_Weights, "', '")[[i]]) 
  specialty_w<- c(lapply(specialty_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  tool<- as.list(strsplit(data$Tool, "', '")[[i]]) 
  tool<- c(lapply(tool, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  tool_w<- as.list(strsplit(data$Tool_Weights, "', '")[[i]]) 
  tool_w<- c(lapply(tool_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  trait<- as.list(strsplit(data$Trait, "', '")[[i]]) 
  trait<- c(lapply(trait, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  trait_w<- as.list(strsplit(data$Trait_Weights, "', '")[[i]]) 
  trait_w<- c(lapply(trait_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  env<- as.list(strsplit(data$Environment, "', '")[[i]]) 
  env<- c(lapply(env, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  env_w<- as.list(strsplit(data$Environment_Weights, "', '")[[i]]) 
  env_w<- c(lapply(env_w, function(x) gsub("\\[", "", gsub("\\]", "", gsub("'", "", x)))))
  
  df[i, ]<- list(category, list(major),list(as.numeric(major_w)), list(specialty), 
                 list(as.numeric(specialty_w)), list(tool), list(as.numeric(tool_w)), 
                 list(trait), list(as.numeric(trait_w)), list(env), 
                 list(as.numeric(env_w)))
  
}

#Way to access a specific instance and it's column, ex: major for program management
words <- c(unlist(df$Major[df$Cateogry == 'program management']))
words

weights <- c(unlist(df$Major_W[df$Cateogry == 'program management']))
weights

##################################################

#Ignore This
key_words <- read.csv('/Users/miketrz/Downloads/DTSC_630_DAta/Categorized List_2.csv')

key_words$Key_Words[key_words$Grouping == 'major']

key_words$Key_Words[key_words$Grouping == 'environment']
