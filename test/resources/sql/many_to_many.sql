CREATE DATABASE IF NOT EXISTS TestDB;
USE TestDB;

CREATE TABLE Student(
  studentID INTEGER PRIMARY KEY
);

CREATE TABLE Course(
  deptName VARCHAR(255) NOT NULL,
  courseNumber INTEGER NOT NULL,
  PRIMARY KEY (deptName, courseNumber)
);

CREATE TABLE Student_and_Course(
  studentID INTEGER NOT NULL,
  deptName VARCHAR(255) NOT NULL,
  courseNumber INTEGER NOT NULL
);

ALTER TABLE Student_and_Course ADD FOREIGN KEY (studentID) REFERENCES Student(studentID);
ALTER TABLE Student_and_Course ADD FOREIGN KEY (deptName, courseNumber) REFERENCES Course(deptName, courseNumber);
