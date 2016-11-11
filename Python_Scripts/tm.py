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
	if sheet['G' + str(i)] is not None: #some of the abstracts will have null values, so we dont want to keep these 
		doc_set.append(sheet['G' + str(i)].value)

#tokenize the document into words
tokenizer = RegexpTokenizer(r'\w+')

# create English stop words list
en_stop = get_stop_words('en')

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
