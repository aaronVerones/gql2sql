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
  book_id INTEGER NOT NULL,
  title VARCHAR,
  x INTEGER,
  y INTEGER,
  votes INTEGER,
  PRIMARY KEY (book_id)
);

CREATE TABLE Category(
  category_id INTEGER NOT NULL,
  name VARCHAR DEFAULT 'foo',
  PRIMARY KEY (category_id)
);

CREATE TABLE Address(
  id INTEGER NOT NULL,
  street VARCHAR,
  city VARCHAR,
  x INTEGER,
  y INTEGER,
  PRIMARY KEY (id)
);

CREATE TABLE Book_and_Category(
  book_id integer NOT NULL,
  category_id integer NOT NULL,
  PRIMARY KEY (book_id, category_id)
  FOREIGN KEY (book_id) REFERENCES Book(book_id)
  FOREIGN KEY (category_id) REFERENCES Category(category_id)
);

ALTER TABLE Author ADD FOREIGN KEY (address) REFERENCES Address(id);
ALTER TABLE Book ADD FOREIGN KEY (x, y) REFERENCES Author(x, y);
ALTER TABLE Address ADD FOREIGN KEY (x, y) REFERENCES Author(x, y);
