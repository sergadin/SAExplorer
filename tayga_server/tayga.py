from random import randrange
import traceback, sys
import json
from msilib.schema import Class
from flask import Flask
from flask import render_template, redirect, url_for
from flask import request, jsonify

from gensim.models import Word2Vec
from pymystem3 import Mystem
from gensim.models import KeyedVectors
import gensim
import pymorphy2


app = Flask(__name__)
app.config['JSON_AS_ASCII'] = False

model = gensim.models.KeyedVectors.load_word2vec_format(
    "local/tayga_1_2.vec", binary=False)
morph = pymorphy2.MorphAnalyzer()


@app.route('/get_vector/', methods=['POST'])
def get_vector():
    query = json.loads(request.data)
    word = query.get("word")
    if word is None:
        print("Пустой запрос в функции get_vector.")
        return jsonify({})
    try:
        vector = model[word].tolist()
        return jsonify({"vector": vector})
    except Exception as e:
        print(e)
        print(f'Ошибка при обработке слова "{word}".')
    return jsonify({})
# TODO Сделать возвращение статуса или как-то еще передавать сообщения об ошибке на сторону клиента. 


@app.route('/most_similar/', methods=['POST'])
def most_similar():
    query = json.loads(request.data)
    
    def key_for_tayga(word: str):
        if word.find('_') >= 0:
            return word
        p = morph.parse(word)[0]
        return "{normal_form}_{POS}".format(normal_form=p.normal_form, POS=p.tag.POS)

    def to_keys(words):
        keys = []
        for word in words:
            if isinstance(word, list):
                keys.append('{w}_{pos}'.format(w=word[0], pos=word[1]))
            elif isinstance(word, str):
                keys.append(key_for_tayga(word))
            else:
                raise ValueError(f"Unexpected word encoding: {word}")
        return keys
    

    try:
        positive = list(to_keys(query["pos_words"]))
        negative = list(to_keys(query.get("neg_words")))
        topn=query.get("topn", 15)
        # print("positive\n", positive)
        # print("negative\n", negative)
        # print("topn\n", topn)

        sim_list = model.most_similar(positive=positive, negative=negative, topn=topn)
    except Exception:
        traceback.print_exc(file=sys.stdout)
        sim_list = []
    return jsonify(sim_list)


if __name__ == "__main__":
    app.run(debug=True, port=5100)
