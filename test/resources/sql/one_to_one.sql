CREATE DATABASE IF NOT EXISTS TestDB;
USE TestDB;

CREATE TABLE FirstEntity(
  firstID INTEGER NOT NULL,
  firstsOtherEntity INTEGER,
  PRIMARY KEY (firstID)
);

CREATE TABLE SecondEntity(
  secondID INTEGER NOT NULL,
  secondsOtherEntity INTEGER,
  PRIMARY KEY (secondID)
);

ALTER TABLE FirstEntity ADD FOREIGN KEY (firstsOtherEntity) REFERENCES SecondEntity(secondID);
ALTER TABLE SecondEntity ADD FOREIGN KEY (secondsOtherEntity) REFERENCES FirstEntity(firstID);
