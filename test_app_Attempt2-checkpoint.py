# Example of a web api using a local host. 
# Must create the datebase using the file called Create_BooksDB_Attempt2
# To utilize this you have use the following in python command prompt altering the location based on your computer: (base) C:\Users\student2\projects\api> python test_app_final.py
# Based off of a tutorial: https://programminghistorian.org/en/lessons/creating-apis-with-python-and-flask 
# But with data I created


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
    return '''<h1> Books </h1>
<p> API for my book collection.</p>'''


@app.route('/api/books/all', methods=['GET'])

def api_all():
    conn = sqlite3.connect('books.db')
    conn.row_factory = dict_factory
    cur = conn.cursor()
    all_books = cur.execute('SELECT * FROM books;').fetchall()

    return jsonify(all_books)



@app.errorhandler(404)
def page_not_found(e):
    return "<h1>404</h1><p>The resource could not be found. Try a different request</p>", 404


@app.route('/api/books', methods=['GET'])
def api_filter():
    query_parameters = request.args

    id = query_parameters.get('id')
    published = query_parameters.get('published')
    author = query_parameters.get('author')

    query = "SELECT * FROM books WHERE"
    to_filter = []

    if id:
        query += ' id=? AND'
        to_filter.append(id)
    if published:
        query += ' published=? AND'
        to_filter.append(published)
    if author:
        query += ' author=? AND'
        to_filter.append(author)
    if not (id or published or author):
        return page_not_found(404)

    query = query[:-4] + ';'

    conn = sqlite3.connect('books.db')
    conn.row_factory = dict_factory
    cur = conn.cursor()

    results = cur.execute(query, to_filter).fetchall()

    return jsonify(results)

app.run()


# To pull all books: http://127.0.0.1:5000/api/books/all

# To pull books in a specific year: http://127.0.0.1:5000/api/books?published=2013

# To pull authors: http://127.0.0.1:5000/api/books?author=Mel+White
