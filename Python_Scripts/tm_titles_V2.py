#LDA Topic Modeling on Simulation Titles
#Kevin Zhai 
#based off example in http://scikit-learn.org/stable/auto_examples/applications/topics_extraction_with_nmf_lda.html#sphx-glr-auto-examples-applications-topics-extraction-with-nmf-lda-py

#import necessary packages
from openpyxl import load_workbook
from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation

wb = load_workbook('MASTER_1968-2015.xlsx')
sheet = wb['Sheet1']

#list that will contain all of the documents. Each document is one year worth of titles
data_samples =[]

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
		data_samples.append(titles)
		titles = ''
		year += 1

n_samples = 2000
n_features = 1000
n_topics = 10
n_top_words = 20

def print_top_words(model, feature_names, n_top_words):
    for topic_idx, topic in enumerate(model.components_):
        print("Topic #%d:" % topic_idx)
        print(" ".join([feature_names[i]
                        for i in topic.argsort()[:-n_top_words - 1:-1]]))
    print()

tf_vectorizer = CountVectorizer(max_df=0.60s, min_df=5,
                                max_features=n_features,
                                stop_words='english')
tf = tf_vectorizer.fit_transform(data_samples)

lda = LatentDirichletAllocation(n_topics=n_topics, max_iter=5,
                                learning_method='online',
                                learning_offset=50.,
                                random_state=0)
lda.fit(tf)


print("\nTopics in LDA model:")
tf_feature_names = tf_vectorizer.get_feature_names()
print_top_words(lda, tf_feature_names, n_top_words)
