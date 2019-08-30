
# coding: utf-8

# In[ ]:


import librosa
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')
import os
import csv


# In[1]:


# Preprocessing
from sklearn.model_selection import train_test_split


# In[2]:


from sklearn.preprocessing import LabelEncoder, StandardScaler


# In[2]:


import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import mglearn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.svm import SVC
from sklearn.neural_network import MLPClassifier


# In[3]:


mydata = pd.read_csv("E:/Audio_feature_extraction/voicegender/voice.csv")


# In[4]:


#Preview voice dataset
mydata.head()
print(mydata.shape)


# In[5]:


#Plot the histograms
male = mydata.loc[mydata['label']=='male']
female = mydata.loc[mydata['label']=='female']
fig, axes = plt.subplots(10, 2, figsize=(10,20))
ax = axes.ravel()


# In[6]:


for i in range(20):
    ax[i].hist(male.ix[:,i], bins=20, color=mglearn.cm3(0), alpha=.5)
    ax[i].hist(female.ix[:, i], bins=20, color=mglearn.cm3(2), alpha=.5)
    ax[i].set_title(list(male)[i])
    ax[i].set_yticks(())
    
ax[0].set_xlabel("Feature magnitude")
ax[0].set_ylabel("Frequency")
ax[0].legend(["male", "female"], loc="best")
fig.tight_layout()


# In[7]:


#Prepare data for modeling
mydata.loc[:,'label'][mydata['label']=="male"] = 0
mydata.loc[:,'label'][mydata['label']=="female"] = 1
mydata_train, mydata_test = train_test_split(mydata, random_state=0, test_size=.2)
scaler = StandardScaler()
scaler.fit(mydata_train.ix[:,0:20])
X_train = scaler.transform(mydata_train.ix[:,0:20])
X_test = scaler.transform(mydata_test.ix[:,0:20])
y_train = list(mydata_train['label'].values)
y_test = list(mydata_test['label'].values)


# In[8]:


#Train decision tree model
tree = DecisionTreeClassifier(random_state=0).fit(X_train, y_train)
print("Decision Tree")
print("Accuracy on training set: {:.3f}".format(tree.score(X_train, y_train)))
print("Accuracy on test set: {:.3f}".format(tree.score(X_test, y_test)))


# In[9]:


#Train random forest model
forest = RandomForestClassifier(n_estimators=5, random_state=0).fit(X_train, y_train)
print("Random Forests")
print("Accuracy on training set: {:.3f}".format(forest.score(X_train, y_train)))
print("Accuracy on test set: {:.3f}".format(forest.score(X_test, y_test)))


# In[10]:


#Train gradient boosting model
gbrt = GradientBoostingClassifier(random_state=0).fit(X_train, y_train)
print("Gradient Boosting")
print("Accuracy on training set: {:.3f}".format(gbrt.score(X_train, y_train)))
print("Accuracy on test set: {:.3f}".format(gbrt.score(X_test, y_test)))


# In[11]:


#Train support vector machine model
svm = SVC().fit(X_train, y_train)
print("Support Vector Machine")
print("Accuracy on training set: {:.3f}".format(svm.score(X_train, y_train)))
print("Accuracy on test set: {:.3f}".format(svm.score(X_test, y_test)))


# In[12]:


#Train neural network model
mlp = MLPClassifier(random_state=0).fit(X_train, y_train)
print("Multilayer Perceptron")
print("Accuracy on training set: {:.3f}".format(mlp.score(X_train, y_train)))
print("Accuracy on test set: {:.3f}".format(mlp.score(X_test, y_test)))


# In[13]:


def plot_feature_importances_mydata(model):
    n_features = X_train.shape[1]
    plt.barh(range(n_features), model.feature_importances_, align='center')
    plt.yticks(np.arange(n_features), list(mydata))
    plt.xlabel("Variable importance")
    plt.ylabel("Independent Variable")


# In[14]:


plot_feature_importances_mydata(tree)
plot_feature_importances_mydata(forest)
plot_feature_importances_mydata(gbrt)


# In[15]:


#Plot the heatmap on first layer weights for neural network
plt.figure(figsize=(100, 20))
plt.imshow(mlp.coefs_[0], interpolation='none', cmap='viridis')
plt.yticks(range(20), list(mydata),fontsize = 50)
plt.xlabel("Columns in weight matrix", fontsize = 50)
plt.ylabel("Input feature", fontsize = 50)
plt.colorbar().set_label('Importance',size=50)
    
plt.show()
