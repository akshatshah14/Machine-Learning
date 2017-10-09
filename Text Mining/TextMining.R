
#init
libs = c("tm","plyr","class","e1071");
lapply(libs,require,character.only = TRUE);

library(knitr)

#set options
options(stringsAsFactors = FALSE );



# reading data
data = read.csv("C:\\Users\\Akshat\\Dropbox\\Spring 2017- ML for Data Science\\Datasets\\TETRUS_New_Crime_Data.csv");




# cleaning and preprocessing
labels = unique(data$incident_type);
label_table = table(data$incident_type);
label_table = as.data.frame(label_table);
label_table =label_table[order(-label_table$Freq),];
data[which(data$incident_type == 'attempttoid'),]$incident_type = 'fraud';
data = data[-which(data$incident_type == 'cancelled'),];
data = data[-which(data$incident_type == 'general_info'),];
data = data[-which(data$incident_type == 'job_posting' | data$incident_type == 'job_postings'),];
data[which(data$incident_type == 'poss_of_weapon'),]$incident_type = 'poss_of_weapons';
data[which(data$incident_type == 'carjacking'),]$incident_type = 'car_jacking';
data[which(data$incident_type == 'drug_overdose'),]$incident_type = 'poss_of_drugs';
data[which(data$incident_type == 'no_headline'),]$incident_type = 'unknown';
data = data[-which(data$incident_type == 'test'),];
data = data[-which(data$incident_type == 'alerts'),];
data = data[-which(data$incident_type == 'charity'),];
data = data[-which(data$incident_type == 'events'),];
data = data[-which(data$incident_type == 'apprehended'),];
data = data[-which(data$incident_type == 'arrest_done'),];
data = data[-which(data$incident_type == 'meetings'),];
data = data[-which(data$incident_type == 'training'),];
data = data[-which(data$incident_type == 'obituary'),];


outliers = label_table[which(label_table$ Freq <5),]$Var1;
data = data[-(which(data$incident_type %in% outliers)),];



# clean text
cleancorpus = function(corpus){

  corpus.tmp = tm_map(corpus,removePunctuation);
  corpus.tmp = tm_map(corpus.tmp,stripWhitespace);
  corpus.tmp = tm_map(corpus.tmp,removeNumbers);
  corpus.tmp<- tm_map(corpus.tmp, content_transformer(tolower))
  myStopWords = c(stopwords("english"), "why","how","when","who","which","via","one","two","monday","tuesday","wednesday","thursday","friday","saturday","sunday","morning","afternoon","evening","night","white","will","within","using","used","use","well");
  corpus.tmp = tm_map(corpus.tmp,removeWords,myStopWords);
  corpus.tmp <- tm_map(corpus.tmp, stemDocument);

  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument);
  return(corpus.tmp);
}


# create DTM
generateDTM = function(description)
{

  s.cor = Corpus(DataframeSource(description));
  s.cor.cl = cleancorpus(s.cor);
    s.dtm = DocumentTermMatrix(s.cor.cl, control=list(weighing=weightTfIdf, minWordLength=2, minDocFreq=20));
  s.dtm = removeSparseTerms(s.dtm,0.99);

  return= s.dtm;
}


description<- data.frame()
for(i in 1:nrow(data)){
  description[i,1]<- data[i,4];
}


dtm = generateDTM(description);


# Latent Semantic Analysis: The basic idea of latent semantic analysis (LSA) is,
# that text do have a higher order (=latent semantic) structure which, is obscured
# by word usage (e.g. through the use of synonyms or polysemy). By using conceptual
# indices that are derived statistically via a truncated SVD over a given document-term
# matrix.This can overcome the variability problem.


library(lsa)
LSA_space = lsa( dtm, dims=dimcalc_share() );
Text_LSA = as.textmatrix(LSA_space);


# spliting into training and testing dataset

train.idx<- sample(nrow(Text_LSA ), ceiling(nrow(Text_LSA )*0.80));
test.idx<- (1: nrow(Text_LSA))[-train.idx];

category<- data$incident_type;

# feature selection
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')
library(rJava)
library(FSelector)

data.chi = data.frame(cbind(Text_LSA[train.idx,],as.factor(category[train.idx])))
nams = rownames(data.chi) 
rownames(data.chi) = make.names(nams, unique=TRUE)
data.chi = data.chi[!duplicated(data.chi), ]
names(data.chi)[663]<-paste("category")

# feature selection using chi square
weights <- chi.squared(category~.,data = data.chi)
print(weights)
subset <- cutoff.k.percent(weights, 0.75)
subset <- cutoff.biggest.diff(weights)
f <- as.simple.formula(subset, "category")
print(f)

# feature selection using random forests
weights.RF <- random.forest.importance(category~.,data = data.chi, importance.type = 1)
print(weights.RF)
subset <- cutoff.k.percent(weights.RF, 0.75)
subset <- cutoff.biggest.diff(weights.RF)
f <- as.simple.formula(subset, "Class")
print(f)

# build the model
model_new <- svm(formula = f,data = data.chi,type="C-classification",kernel ="radial");
test = Text_LSA[test.idx,]
colnames(test) = colnames(data.chi[,-663])
pred = predict(model_new,test);


# accuracy
conf.mat = table("Predictions" = pred, Actual= category[test.idx]);
accuracy = (sum(diag(conf.mat))/length(test.idx))*100;

accuracy