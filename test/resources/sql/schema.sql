CREATE DATABASE TestDB;
USE TestDB;

CREATE TABLE Author(
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  name VARCHAR,
  address INTEGER,
  PRIMARY KEY (x, y)
);

CREATE TABLE Book(
  id INTEGER NOT NULL,
  title VARCHAR(255),
  author_x INTEGER,
  author_y INTEGER,
  votes INTEGER,
  PRIMARY KEY (id)
);

CREATE TABLE Category(
  id INTEGER NOT NULL,
  name VARCHAR DEFAULT 'foo',
  PRIMARY KEY (id)
);

CREATE TABLE Book_and_Category(
  book_id integer NOT NULL,
  category_id integer NOT NULL,
  PRIMARY KEY (book_id, category_id)
);

CREATE TABLE Address(
  id INTEGER NOT NULL,
  street VARCHAR,
  city VARCHAR,
  author_x INTEGER,
  author_y INTEGER,
  PRIMARY KEY (id)
);

CREATE TABLE Mother(
  id INTEGER,
  foo VARCHAR NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE Father(
  id INTEGER NOT NULL,
  bar VARCHAR NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE Person(
  id INTEGER NOT NULL,
  mother_id INTEGER,
  father_id INTEGER,
  PRIMARY KEY (id),
  CHECK (((mother_id) IS NOT NULL) OR ((father_id) IS NOT NULL))
);

ALTER TABLE Author ADD FOREIGN KEY (address) REFERENCES Address(id);
ALTER TABLE Book ADD FOREIGN KEY (author_x, author_y) REFERENCES Author(x, y);
ALTER TABLE Address ADD FOREIGN KEY (author_x, author_y) REFERENCES Author(x, y);
ALTER TABLE Person ADD FOREIGN KEY (mother_id) REFERENCES Mother(id);
ALTER TABLE Person ADD FOREIGN KEY (father_id) REFERENCES Father(id);
