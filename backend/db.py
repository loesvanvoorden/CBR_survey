import sqlite3

def create_db():
    conn = sqlite3.connect('database.db')
    cursor = conn.cursor()
    cursor.execute('''
    CREATE TABLE IF NOT EXISTS conversations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        question TEXT,
        answer TEXT,
        language TEXT,
        source_documents TEXT,
        token_usage INTEGER
    )
    ''')
    conn.commit()
    conn.close()

def save_conversation(question, answer, language, source_documents, token_usage):
    conn = sqlite3.connect('database.db')
    cursor = conn.cursor()
    cursor.execute('''
    INSERT INTO conversations (question, answer, language, source_documents, token_usage)
    VALUES (?, ?, ?, ?, ?)
    ''', (question, answer, language, str(source_documents), token_usage))
    conn.commit()
    conn.close()

create_db()