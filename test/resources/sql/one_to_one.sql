CREATE DATABASE IF NOT EXISTS TestDB;
USE TestDB;

CREATE TABLE FirstEntity(
  firstID INTEGER PRIMARY KEY,
  firstsOtherEntity INTEGER
);

CREATE TABLE SecondEntity(
  secondID INTEGER PRIMARY KEY,
  secondsOtherEntity INTEGER
);

ALTER TABLE FirstEntity ADD FOREIGN KEY (firstsOtherEntity) REFERENCES SecondEntity(secondID);
ALTER TABLE SecondEntity ADD FOREIGN KEY (secondsOtherEntity) REFERENCES FirstEntity(firstID);
