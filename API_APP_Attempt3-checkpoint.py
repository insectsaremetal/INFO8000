# Example of a web api using a local host. The 
# To utilize this you have use the following in python command prompt altering the location based on your computer: (base) C:\Users\student2\projects\api> python API_APP_Attempt3.py

import flask
from flask import request, jsonify
import sqlite3

app = flask.Flask(__name__)
app.config["DEBUG"] = True

def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d


@app.route('/', methods=['GET'])
def home():
    return '''<h1> Entertainment Industry </h1>
<p> API for information on the Entertainment Industry for Georgia and South Carolina. </p>'''


@app.route('/api/movies/all', methods=['GET'])

def api_all():
    conn = sqlite3.connect('entertainmentindustries.db')
    conn.row_factory = dict_factory
    cur = conn.cursor()
    all_movies = cur.execute('SELECT * FROM person;').fetchall()

    return jsonify(all_movies)



@app.errorhandler(404)
def page_not_found(e):
    return "<h1>404</h1><p>The resource could not be found. Try a different request</p>", 404


@app.route('/api/movies', methods=['GET'])
def api_filter():
    query_parameters = request.args

    age = query_parameters.get('age')
    movie = query_parameters.get('movie')
    theatername = query_parameters.get('theatername')

    query = "SELECT * FROM person WHERE"
    to_filter = []

    if age:
        query += ' age=? AND'
        to_filter.append(age)
    if movie:
        query += ' movie=? AND'
        to_filter.append(movie)
    if theatername:
        query += ' theatername=? AND'
        to_filter.append(theatername)
    if not (age or movie or theatername):
        return page_not_found(404)

    query = query[:-4] + ';'

    conn = sqlite3.connect('entertainmentindustries.db')
    conn.row_factory = dict_factory
    cur = conn.cursor()

    results = cur.execute(query, to_filter).fetchall()

    return jsonify(results)

app.run()


# To pull all movies: http://127.0.0.1:5000/api/movies/all

# To pull a specific theater: http://127.0.0.1:5000/api/movies?theatername=Rockstop

# To pull a specific age: http://127.0.0.1:5000/api/movies?age=22