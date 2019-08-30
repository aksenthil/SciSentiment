
# coding: utf-8

# In[3]:


import pyaudio
import librosa


# In[4]:


audio_path='C:/Users/s2759/Downloads/OSR_us_000_0010_8k.wav'


# In[5]:


x, sr =librosa.load(audio_path)


# In[6]:


print(type(x), type(sr))


# In[7]:


librosa.load(audio_path, sr=44100)


# In[9]:


librosa.load(audio_path, sr=None)


# In[11]:


import IPython.display as ipd


# In[12]:


ipd.Audio(audio_path)


# In[15]:


# Display waveform
get_ipython().run_line_magic('matplotlib', 'inline')
import matplotlib.pyplot as plt
import librosa.display
plt.figure(figsize=(14, 5))
librosa.display.waveplot(x, sr=sr)


# In[18]:


# Display Spectrogram

X= librosa.stft(x)
Xdb= librosa.amplitude_to_db(abs(X))
plt.figure(figsize=(14,5))


# In[21]:


librosa.display.specshow(Xdb, sr=sr, x_axis='time', y_axis='hz')
# If to pring log of frequencies
plt.colorbar()


# In[22]:


x, sr= librosa.load(audio_path)
# plot the signal:
plt.figure(figsize=(14,5))
librosa.display.waveplot(x, sr=sr)


# In[24]:


# Zooming in

n0=9000
n1=9100
plt.figure(figsize=(14,5))
plt.plot(x[n0:n1])
plt.grid()


# In[27]:


zero_crossings= librosa.zero_crossings(x[n0:n1], pad= False)
print(sum(zero_crossings))


# In[28]:


import sklearn


# In[29]:


spectral_centroids= librosa.feature.spectral_centroid(x, sr=sr)[0]


# In[30]:


spectral_centroids.shape


# In[31]:


# Computing the time variable for visualization
frames=range(len(spectral_centroids))


# In[32]:


t= librosa.frames_to_time(frames)


# In[38]:


def normalize(x, axis=0):
    return sklearn.preprocessing.minmax_scale(x, axis=axis)


# In[39]:


librosa.display.waveplot(x, sr=sr, alpha=0.4)
plt.plot(t, normalize(spectral_centroids), color='r')


# In[40]:


spectral_rolloff= librosa.feature.spectral_rolloff(x, sr=sr) [0]


# In[41]:


librosa.display.waveplot(x, sr=sr, alpha=0.4)
plt.plot(t, normalize(spectral_rolloff), color='r')


# In[42]:


mfccs= librosa.feature.mfcc(x, sr=sr)
print(mfccs.shape)


# In[43]:


# displaying the MFCCs;
librosa.display.specshow(mfccs, sr=sr, x_axis='time')

