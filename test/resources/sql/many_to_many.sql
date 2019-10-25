CREATE DATABASE IF NOT EXISTS TestDB;
USE TestDB;

CREATE TABLE Student(
  studentID INTEGER NOT NULL,
  PRIMARY KEY (studentID)
);

CREATE TABLE Course(
  deptName VARCHAR NOT NULL,
  courseNumber INTEGER NOT NULL,
  PRIMARY KEY (deptName, courseNumber)
);

CREATE TABLE Student_and_Course(
  studentID INTEGER NOT NULL,
  deptName VARCHAR NOT NULL,
  courseNumber INTEGER NOT NULL,
  PRIMARY KEY (studentID, deptName, courseNumber),
  FOREIGN KEY (studentID) REFERENCES Student(studentID),
  FOREIGN KEY (deptName, courseNumber) REFERENCES Course(deptName, courseNumber)
);
