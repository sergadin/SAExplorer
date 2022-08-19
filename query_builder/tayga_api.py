import requests
import numpy as np
import logging

def most_similar(positive, negative=[], topn=10):
    """Функция удаленно вызывает most_similar из модели Word2Vec"""
    logging.info("Api. Начало работы most similar.")
    tayga_rq = {
        "pos_words": list(positive),
        "neg_words": list(negative),
        "topn": int(topn)
    }
    # print(tayga_rq)
    r = requests.post('http://localhost:5100/most_similar/', json=tayga_rq)
    logging.info("Api. Конец работы most similar.")
    return r.json() 


vectors_cache = {}
def get_vector(word):
    """Функция удаленно вызывает model[word] из модели Word2Vec"""
    logging.info("Api. Начало работы get_vector.")
    if word in vectors_cache:
        return vectors_cache[word]
    
    tayga_rq = {
        "word": word
    }
    r = requests.post('http://localhost:5100/get_vector/', json=tayga_rq)
    vector = r.json().get("vector")
    vectors_cache[word] = np.array(vector) if vector else None
    logging.info("Api. Конец работы get_vector.")
    return vectors_cache[word]
