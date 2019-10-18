CREATE DATABASE TestDB;
USE TestDB;

CREATE TABLE Author(
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  name VARCHAR(255),
  address INTEGER,
  PRIMARY KEY (x, y)
);

CREATE TABLE Book(
  id INTEGER PRIMARY KEY,
  title VARCHAR(255),
  author_x INTEGER,
  author_y INTEGER,
  votes INTEGER
);

CREATE TABLE Category(
  id INTEGER PRIMARY KEY,
  name VARCHAR(255) DEFAULT 'foo'
);

CREATE TABLE Book_and_Category(
  book_id integer NOT NULL,
  category_id integer NOT NULL,
  PRIMARY KEY (book_id, category_id)
);

CREATE TABLE Address(
  id INTEGER PRIMARY KEY,
  street VARCHAR(255),
  city VARCHAR(255),
  author_x INTEGER,
  author_y INTEGER
);

CREATE TABLE Mother(
  id INTEGER PRIMARY KEY,
  foo VARCHAR(255) NOT NULL
);

CREATE TABLE Father(
  id INTEGER PRIMARY KEY,
  bar VARCHAR(255) NOT NULL
);

CREATE TABLE Person(
  id INTEGER PRIMARY KEY,
  mother_id INTEGER,
  father_id INTEGER,
  CHECK (((mother_id) IS NOT NULL) OR ((father_id) IS NOT NULL))
);

ALTER TABLE Author ADD FOREIGN KEY (address) REFERENCES Address(id);
ALTER TABLE Book ADD FOREIGN KEY (author_x, author_y) REFERENCES Author(x, y);
ALTER TABLE Address ADD FOREIGN KEY (author_x, author_y) REFERENCES Author(x, y);
ALTER TABLE Person ADD FOREIGN KEY (mother_id) REFERENCES Mother(id);
ALTER TABLE Person ADD FOREIGN KEY (father_id) REFERENCES Father(id);
