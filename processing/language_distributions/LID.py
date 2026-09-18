import pandas as pd
import fasttext
import os
import regex as re
import emoji
import numpy as np
from huggingface_hub import hf_hub_download


# load model for LI
model_path_glot = hf_hub_download(repo_id="cis-lmu/glotlid", filename="model.bin")
model_glot = fasttext.load_model(model_path_glot)
#model_glot.predict("Wos soi denn des sei?", k = 1)

model_path_ft = hf_hub_download(repo_id="facebook/fasttext-language-identification", filename="model.bin")
model_ft = fasttext.load_model(model_path_ft)
#model_ft.predict("Hej på dig", k = 3)

def get_language_glot(text : str):
    #This function takes a string and returns the most probable label from glotLID
    # make prediction
    res = model_glot.predict(text, k = 1)
    
    # extract label from tuple
    lab = res[0][0]
    lab = lab.split("__")[2].split("_")[0]
    
    # extract confidence score
    conf = res[1][0]
    return(lab, conf)

def get_language_ft(text:str):
    #This function takes a string and returns the most probable label from fasttext
    # make prediction
    res = model_ft.predict(text, k = 1)
    
    # extract label from tuple
    lab = res[0][0]
    lab = lab.split("__")[2].split("_")[0]
    
    # extract confidence score
    conf = res[1][0]
    return(lab, conf)

#get_language_glot("detta er et test")
#get_language_ft("Wos soi denn des sei?")
#get_language_glot("Wos soi des denn sei?")



