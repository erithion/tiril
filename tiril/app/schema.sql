--
-- File generated with SQLiteStudio v3.1.1 on Ср Кві 18 21:25:38 2018
--
-- Text encoding used: UTF-8
--
PRAGMA foreign_keys = off;

-- Table: session
DROP TABLE IF EXISTS session;
CREATE TABLE session (word TEXT UNIQUE ON CONFLICT IGNORE NOT NULL);

PRAGMA foreign_keys = on;
