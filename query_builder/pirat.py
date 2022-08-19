from random import randrange
import logging
#import random
from msilib.schema import Class
from flask import Flask
from flask import render_template, redirect, url_for
from flask import request
from flask_sqlalchemy import SQLAlchemy
# from sqlalchemy.orm import relationship
# from sqlalchemy import Column, ForeignKey
from matplotlib.pyplot import title
from matplotlib.style import context
from suggest_refinement import suggest
app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:///local/dict.db'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
db = SQLAlchemy(app)


class Area_description(db.Model):
    __tablename__ = "area_description"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    title = db.Column(db.String(50), nullable=False)
    #suggestions = db.Column(db.String(200), nullable=False)
    versions = db.relationship(
        "Query_version", back_populates="area_description")

    def __repr__(self):
        return '<Area_description %r>' % self.id


class Query_version(db.Model):
    __tablename__ = "query_version"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    area_description_id = db.Column(
        db.Integer, db.ForeignKey("area_description.id"))
    # date =
    serial_number = db.Column(db.Integer)
    area_description = db.relationship(
        "Area_description", back_populates="versions")
    terms = db.relationship("Term", back_populates="qv", cascade="all, delete-orphan")
    rec = db.relationship("Rec", back_populates="qv", uselist=False, cascade="all, delete-orphan")

    def clone(self):
        version = Query_version(serial_number=self.serial_number+1,
                                area_description_id=self.area_description_id)
        db.session.add(version)
        db.session.commit()

        for term in self.terms:
           db.session.add(Term(qv_id=version.id, positivity=term.positivity, word=term.word, pos_tag=term.pos_tag))
        db.session.commit() 
        return version

    def __repr__(self):
        return '<Query_version %r>' % self.id


class Term(db.Model):
    __tablename__ = "term"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    qv_id = db.Column(db.Integer, db.ForeignKey("query_version.id", ondelete='CASCADE'))
    positivity = db.Column(db.Boolean)
    word = db.Column(db.String(200), nullable=False)
    pos_tag = db.Column(db.String(250), nullable=False)
    qv = db.relationship("Query_version", back_populates="terms")

    def __repr__(self):
        return '<Term %r>' % self.id


class Rec(db.Model):
    __tablename__ = "rec"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    qv_id = db.Column(db.Integer, db.ForeignKey("query_version.id", ondelete='CASCADE'))
    qv = db.relationship("Query_version", back_populates="rec")
    rec_words = db.relationship("Rec_word", back_populates="rec", cascade="all, delete-orphan")

    def __repr__(self):
        return '<Rec %r>' % self.id


class Rec_word(db.Model):
    __tablename__ = "rec_word"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    rec_id = db.Column(db.Integer, db.ForeignKey("rec.id", ondelete='CASCADE'))
    word = db.Column(db.String(250), nullable=False)
    pos_tag = db.Column(db.String(250), nullable=False)
    dist = db.Column(db.Float)
    rec = db.relationship("Rec", back_populates="rec_words")
    rec_words_cluster = db.relationship("Rec_word_cluster", back_populates="rec_word", cascade="all, delete-orphan")

    def __repr__(self):
        return '<Rec_word %r>' % self.id


class Rec_word_cluster(db.Model):
    __tablename__ = "rec_word_cluster"
    id = db.Column(db.Integer, primary_key=True, autoincrement=True)
    word = db.Column(db.String(250), nullable=False)
    pos_tag = db.Column(db.String(250), nullable=False)
    rec_word_id = db.Column(db.Integer, db.ForeignKey("rec_word.id", ondelete='CASCADE'))
    rec_word = db.relationship("Rec_word", back_populates="rec_words_cluster")

    def __repr__(self):
        return '<Rec_word_cluster %r>' % self.id

@app.route('/hello/')
def hello():
    return 'Hello, World!'


@app.route('/new_query', methods=['GET', 'POST'])
def new_query():
    title = request.form['title']
    word = request.form['word']

    area = Area_description(title=title)
    db.session.add(area)
    db.session.commit()

    version = Query_version(serial_number=1, area_description_id=area.id)
    db.session.add(version)
    db.session.commit()

    term = Term(positivity=True, word=word, qv_id=version.id)
    db.session.add(term)
    db.session.commit()

    return redirect(url_for('show_recomendations', area_id=version.area_description.id))


@app.route('/')
def index():
    context = {}
    # def randstr(N):
    #     return ''.join(random.choice(string.ascii_uppercase + string.digits) for _ in range(N))
    # db.session.add(Area_description(id=randrange(1000000), title=randstr(randrange(25))))
    # db.session.commit()
    old_descriptions = Area_description.query.all()
    context["old_descriptions"] = old_descriptions
    return render_template('index.html', context=context)




@app.route('/show_rec/<area_id>/')
def show_recomendations(area_id):
    area = Area_description.query.get(area_id)
    last_version = sorted(
        area.versions, key=lambda item: item.serial_number, reverse=True)[0]
    if last_version.rec is None:
        pos_words = []
        neg_words = []
        for term in last_version.terms:
            if term.positivity:
                pos_words.append(term)
            elif term.positivity is False:
                neg_words.append(term)
        
        suggestions = suggest(pos_words, neg_words)

        rec = Rec(qv_id=last_version.id)
        db.session.add(rec)
        db.session.commit()

        for n_w in suggestions.nearest_words:
            rec_word = Rec_word(word=n_w.word, pos_tag=n_w.pos_tag, dist=n_w.dist, rec_id=rec.id)
            db.session.add(rec_word)
            db.session.commit()
            for item in n_w.cluster:
                rec_word_cluster = Rec_word_cluster(word=item.word, pos_tag=item.pos_tag, rec_word_id=rec_word.id)
                db.session.add(rec_word_cluster)
        db.session.commit()

    context = {
        'title': area.title,
        'last_version': last_version
    }
    return render_template('describe.html',
                           context=context)

def save_feedback(version_id, rec_word_id, positivity):
    version = Query_version.query.get(version_id)
    new_version = version.clone()
    rec_word = Rec_word.query.get(rec_word_id)
    
    db.session.add(Term(qv_id=new_version.id, positivity=positivity, word=rec_word.word, pos_tag=rec_word.pos_tag))
    db.session.commit()
    return version

@app.route('/mark_positive/<version_id>/<rec_word_id>/')
def mark_positive(version_id, rec_word_id):
    version = save_feedback(version_id, rec_word_id, True)
    return redirect(url_for('show_recomendations', area_id=version.area_description.id))

@app.route('/mark_negative/<version_id>/<rec_word_id>/')
def mark_negative(version_id, rec_word_id):
    version = save_feedback(version_id, rec_word_id, False)
    return redirect(url_for('show_recomendations', area_id=version.area_description.id))

@app.route('/undo/<version_id>/')
def undo_version(version_id):
    version = Query_version.query.get(version_id)
    area_id=version.area_description.id
    print(version.serial_number)
    if version.serial_number > 1:
        db.session.delete(version)
        # Query_version.query.filter_by(id=version.id).delete()
        db.session.commit()
    return redirect(url_for('show_recomendations', area_id=area_id))


if __name__ == "__main__":
    FORMAT = '%(asctime)s %(message)s'
    logging.basicConfig(format=FORMAT,
                        filename="local/pirat.log", filemode="w", encoding='utf-8',
                        level=logging.INFO)
    app.run(debug=True)
