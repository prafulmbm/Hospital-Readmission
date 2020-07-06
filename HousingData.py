
# coding: utf-8

# In[116]:


import os
import pandas as pd

os.chdir(r'C:\Users\PS00647571\Downloads\Instruction+Document\Instruction Document')

HousingData = pd.read_csv("HousingData.csv",header=0)

HousingData.describe()
HousingData.ocean_proximity.value_counts()

Y = HousingData["median_house_value"]
X = HousingData.drop(columns = ["median_house_value"],axis =1)

X.dtypes


# In[117]:


print(X_test)


# In[118]:


ocean_proxiity_dummies = pd.get_dummies(X.select_dtypes("object"))


# In[119]:


X = X.drop(columns=["ocean_proximity"],axis=1)


# In[120]:


X = pd.concat([X,ocean_proxiity_dummies],axis=1)


# In[121]:


X.isnull().sum(axis=0)


# In[122]:


X.total_bedrooms = X.total_bedrooms.fillna(X.total_bedrooms.median(axis=0))


# In[123]:


from sklearn.preprocessing import StandardScaler


# In[124]:


Scaling = StandardScaler(with_mean=True,with_std=True)


# In[125]:


X = Scaling.fit_transform(X)


# In[126]:


from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X,Y,test_size=0.3,random_state = 202)


# In[127]:


from sklearn.linear_model import LinearRegression


# In[151]:


ln = LinearRegression()


# In[152]:


from yellowbrick.regressor import residuals_plot
residuals_plot(ln,X_train,y_train)


# In[153]:


pred = ln.predict(X_test)


# In[154]:


from sklearn.metrics import mean_absolute_error,mean_squared_error


# In[155]:


ln.score(X_train,y_train)


# In[132]:


mean_absolute_error(y_pred=pred,y_true=y_test)


# In[134]:


def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())


# In[135]:


def MAPE(predictions, targets):
    return np.average(np.abs(predictions - targets)/targets) 


# In[136]:


rmse(pred,y_test)


# In[137]:


MAPE(pred,y_test)


# In[138]:


from sklearn.ensemble import RandomForestRegressor


# In[139]:


RF = RandomForestRegressor(n_estimators=100,max_features="auto")


# In[140]:


RF.fit(X_train,y_train)


# In[141]:


pred = RF.predict(X_train)


# In[142]:


len(pred)


# In[143]:


rmse(pred,y_train)


# In[144]:


pred = RF.predict(X_test)


# In[145]:


MAPE(pred,y_test)

