CREATE DATABASE IF NOT EXISTS `uniquant`;

USE `uniquant`;

DROP TABLE IF EXISTS `uniquant_holding_forex`;
DROP TABLE IF EXISTS `uniquant_holding`;
DROP TABLE IF EXISTS `uniquant_portfolio`;
DROP TABLE IF EXISTS `uniquant_users`;
DROP TABLE IF EXISTS `uniquant_history`;
DROP TABLE IF EXISTS `uniquant_trade`;

CREATE TABLE `uniquant_users` (
  `ID`          BIGINT(20)      NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `username`    VARCHAR(50)     NOT NULL UNIQUE,
  `firstname`   VARCHAR(255)    NOT NULL,
  `lastname`    VARCHAR(255)    NOT NULL,
  `email`       VARCHAR(320)    NOT NULL UNIQUE,
  `password`    VARCHAR(255)    NOT NULL,
  `dob`         DATETIME        NOT NULL,
  `gender`      TINYINT	        NOT NULL,
  `tor`         DATETIME        NOT NULL DEFAULT NOW() # timestamp of registration
);

CREATE TABLE `uniquant_portfolio` (
  `ID`          BIGINT(20)      NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `userID`      BIGINT(20)      NOT NULL,
  `name`        VARCHAR(255)    NOT NULL,
  FOREIGN KEY   (`userID`)      REFERENCES `uniquant_users`(`ID`),
  UNIQUE  KEY   `unique_key`(`userID`, `name`)
);

CREATE TABLE `uniquant_holding` (
  `ID`          BIGINT(20)      NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `portfolioID` BIGINT(20)      NOT NULL,
  `type`        VARCHAR(255)    NOT NULL,
  FOREIGN KEY   (`portfolioID`) REFERENCES `uniquant_portfolio`(`ID`),
  UNIQUE  KEY   `unique_key`(`portfolioID`, `type`)
);

CREATE TABLE `uniquant_holding_forex` (
  `ID`          BIGINT(20)     NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `holdingID`   BIGINT(20)     NOT NULL,
  `from`        VARCHAR(3)     NOT NULL,
  `to`          VARCHAR(3)     NOT NULL,
  `units`       INTEGER        NOT NULL,
  FOREIGN KEY   (`holdingID`)  REFERENCES `uniquant_holding`(`ID`)
);

CREATE TABLE `uniquant_history` (
  `ID`          BIGINT(20)     NOT NULL AUTO_INCREMENT PRIMARY KEY,
  `datetime`    DATETIME       NOT NULL,
  `symbol`      VARCHAR(6)     NOT NULL,
  `open`        DECIMAL(10, 6),
  `high`        DECIMAL(10, 6),
  `low`         DECIMAL(10, 6),
  `close`       DECIMAL(10, 6),
  `volume`      BIGINT(20),
  UNIQUE KEY    `unique_key`(`datetime`, `symbol`)
);
