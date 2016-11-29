#LDA Topic Modeling on Simulation Titles
#Kevin Zhai 
#based off example in https://rstudio-pubs-static.s3.amazonaws.com/79360_850b2a69980c4488b1db95987a24867a.html#what-is-lda

#import necessary packages
from nltk.tokenize import RegexpTokenizer
from stop_words import get_stop_words
from nltk.stem.porter import PorterStemmer
from gensim import corpora, models
import gensim
from openpyxl import load_workbook

wb = load_workbook('MASTER_1968-2015.xlsx')
sheet = wb['Sheet1']

#list that will contain all of the documents. Each document is one year worth of titles
doc_set =[]

#starting year
year = 1968
titles = ''

#loop through csv to collect documents of yearly titles
for i in range(2,23889):
	if str(sheet['A' + str(i)].value) == str(year):
		#check if title is repeating -- we dont want repeating titles to scew the document term matrix
		if str(sheet['E' + str(i)].value) != str(sheet['E' + str(i+1)].value): 
			titles += str(sheet['E' + str(i)].value) + ' '
	#check if a new year starts in the next row. If so, complete the document and add to doc_set	
	if (str(sheet['A' + str(i)].value) != str(sheet['A' + str(i+1)].value)) | (type(sheet['A' + str(i+1)].value) is type(None)):
		doc_set.append(titles)
		titles = ''
		year += 1


#tokenize the document into words
tokenizer = RegexpTokenizer(r'\w+')

# create English stop words list
en_stop = get_stop_words('en')

#personal list of stop words
en_stop.append('simulation')
en_stop.append('simulating')
en_stop.append('simulated')
en_stop.append('simulate')
en_stop.append('model')
en_stop.append('modeling')
en_stop.append('modeled')


# Create p_stemmer of class PorterStemmer - algorithm that stems words 
p_stemmer = PorterStemmer()

#list for tokenized documents in loop
texts = []

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
ldamodel = gensim.models.ldamodel.LdaModel(corpus, num_topics=7, id2word = dictionary, passes=10)

