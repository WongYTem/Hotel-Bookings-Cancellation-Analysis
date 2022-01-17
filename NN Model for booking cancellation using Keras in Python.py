# -*- coding: utf-8 -*-
"""
Created on Sun Jan 16 03:48:10 2022

@author: user
"""
import pandas as pd

# Load dataset.
df = pd.read_csv('hotel_bookings.csv')

df.dtypes
df[['agent', 'company']] = df[['agent', 'company']].astype('str')

df.isnull().sum()
df.loc[df['children'].isnull(),'children'] = df['babies']
df[['agent','company','country']]=df[['agent','company','country']].fillna(" ")


df=df.drop(columns=['reservation_status_date'])
# remove special character
df.columns = df.columns.str.replace(' ', '_')

#move to last column
cols = list(df.columns.values) 
cols
target_col = df.pop('is_canceled')
df.insert(30, 'is_canceled', target_col)

X = df.iloc[:, :-1]
y = df.iloc[:, -1].values

#split df to num and object
X.dtypes
X_num = df.select_dtypes(exclude='object')
X_cat = df.select_dtypes(include='object')

from sklearn.preprocessing import LabelEncoder, OneHotEncoder
# instantiate labelencoder object
le = LabelEncoder()
# apply le on categorical feature columns
X_cat = X_cat.apply(lambda col: le.fit_transform(col))  

#One-hot-encode the categorical columns, which outputs an array instead of dataframe.  
ohe = OneHotEncoder()
X_cat = ohe.fit_transform(X_cat).toarray()
#Convert it to df
X_cat = pd.DataFrame(X_cat)

#Concatenate the two dataframes and turn to array : 
X = pd.concat([X_cat, X_num], axis=1)

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2)

# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

n_cols = X_train.shape[1] 
# Importing the Keras libraries and packages
import keras
from keras.models import Sequential
from keras.layers import Dense

keras.backend.clear_session()
#Initializing Neural Network
model = Sequential()

# Adding the input layer and the first hidden layer
model.add(Dense(200,  activation = 'relu', input_shape = (n_cols,)))
# Adding the second hidden layer
model.add(Dense(100, activation = 'relu'))
# Adding second third layer
model.add(Dense(50, activation = 'relu'))
# Adding second forth layer
model.add(Dense(50, activation = 'relu'))
# Adding final  layer
model.add(Dense(1, activation = 'sigmoid'))

model.summary()

# Compiling Neural Network
model.compile(optimizer='adam',loss='binary_crossentropy', metrics = ['accuracy'])
#modelcheckpoint
from keras.callbacks import ModelCheckpoint
checkpoint = ModelCheckpoint("best_model.hdf5", monitor='loss',
    save_best_only=True, mode='auto', save_freq=100)
# Fitting our model
EPOCHS=10
BATCHSIZE=32
history=model.fit(X_train, y_train, batch_size = BATCHSIZE, epochs = EPOCHS, callbacks=[checkpoint],verbose=1)
# saving the model in tensorflow format
model.save('./Model_tf',save_format='tf')

# Predicting the Test set results
y_pred = model.predict(X_test)
y_pred = (y_pred > 0.5)

# Creating the Confusion Matrix
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

