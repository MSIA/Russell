#LDA Topic Modeling on Simulation Abstracts
#Kevin Zhai 
#based off example in https://rstudio-pubs-static.s3.amazonaws.com/79360_850b2a69980c4488b1db95987a24867a.html#what-is-lda

#import necessary packages
from nltk.tokenize import RegexpTokenizer
from stop_words import get_stop_words
from nltk.stem.porter import PorterStemmer
from gensim import corpora, models
import gensim
from openpyxl import load_workbook

wb = load_workbook('1997-2015_FINAL.xlsx')
sheet = wb['Sheet1']
#list that will contain all of the paper abstracts 
doc_set =[]

for i in range(2,6952):
	if type(sheet['G' + str(i)]) is not type(None): #some of the abstracts will have null values, so we dont want to keep these 
		doc_set.append(sheet['G' + str(i)].value)

#tokenize the document into words
tokenizer = RegexpTokenizer(r'\w+')

# create English stop words list
en_stop = get_stop_words('en')

# Create p_stemmer of class PorterStemmer - algorithm that stems words 
p_stemmer = PorterStemmer()

print('yes')

#list for tokenized documents in loop
texts = []
counter = 0

for i in doc_set:
	# clean and tokenize document string
	raw = i.lower()
	tokens = tokenizer.tokenize(raw)

	# remove stop words from tokens
	stopped_tokens = [i for i in tokens if not i in en_stop]

	# stem tokens
	stemmed_tokens = [p_stemmer.stem(i) for i in stopped_tokens]

	# add tokens to list
	texts.append(stemmed_tokens)

# turn our tokenized documents into a id <-> term dictionary
dictionary = corpora.Dictionary(texts)

# convert tokenized documents into a document-term matrix
corpus = [dictionary.doc2bow(text) for text in texts]

# generate LDA model
ldamodel = gensim.models.ldamodel.LdaModel(corpus, num_topics=2, id2word = dictionary, passes=20)

#RESULTS
#>>> print(ldamodel.print_topics(num_topics=3, num_words=3))
#[(0, '0.022*"simul" + 0.018*"model" + 0.013*"use"'), (1, '0.032*"simul" + 0.030*"model" + 0.015*"use"')]
#>>> print(ldamodel.print_topics(num_topics=5, num_words=3))
#[(0, '0.022*"simul" + 0.018*"model" + 0.013*"use"'), (1, '0.032*"simul" + 0.030*"model" + 0.015*"use"')]
#>>> print(ldamodel.print_topics(num_topics=5, num_words=5))
#[(0, '0.022*"simul" + 0.018*"model" + 0.013*"use" + 0.012*"system" + 0.010*"time"'), (1, '0.032*"simul" + 0.030*"model" + 0.015*"use" + 0.010*"system" + 0.009*"base"')]
#>>> print(ldamodel.print_topics(num_topics=5, num_words=10))
#[(0, '0.022*"simul" + 0.018*"model" + 0.013*"use" + 0.012*"system" + 0.010*"time" + 0.008*"process" + 0.008*"product" + 0.008*"oper" + 0.007*"paper" + 0.006*"develop"'), (1, '0.032*"simul" + 0.030*"model" + 0.015*"use" + 0.010*"system" + 0.009*"base" + 0.008*"method" + 0.007*"paper" + 0.007*"can" + 0.007*"approach" + 0.007*"problem"')]