# Example of a web api using a local host. 
# To utilize this you have use the following in python command prompt altering the location based on your computer: 
# (base) C:\Users\student2\projects\api> python API_APP_Attempt3.py

# I could not get my code to work using the method of Google Cloud. When I initially created an account it kept reverting back to
# an individual that I do not know and contained their information, so I don't know what is going on.
# My initial attempt using Google Cloud is in the INFO8000 git repo though. 

import flask
from flask import Flask, request, jsonify, session, redirect, url_for, escape, make_response
import sqlite3
from flask_httpauth import HTTPBasicAuth

app = flask.Flask(__name__)
app.config["DEBUG"] = True

auth = HTTPBasicAuth()

USER_DATA = {
    "owner": "moviefun"
}

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

#def login():
#    error = None
#    if request.method == 'POST':
#        if valid_login(request.form['username'],
#                       request.form['password']):
#            return log_the_user_in(request.form['username'])
#        else:
#            error = 'Invalid username/password'
#    return render_template('login.html', error=error)

@auth.verify_password
def verify(username, password):
    if not (username and password):
        return False
    return USER_DATA.get(username) == password

@auth.login_required
@app.route('/additions', methods=['PUT'])
def addinfo():
    additions = request.args.get("data")
    all_movies.db.execute(add)
    all_movies.db.commit(additions)
    return "<p>The additions have been included. </p>"

if __name__=="__main__":
    app.run()

# To pull all movies: http://127.0.0.1:5000/api/movies/all

# To pull a specific theater: http://127.0.0.1:5000/api/movies?theatername=Rockstop

# To pull a specific age: http://127.0.0.1:5000/api/movies?age=22

# To get to the homepage: http://127.0.0.1:5000/api/movies
