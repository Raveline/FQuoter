DROP TABLE IF EXISTS Author;
CREATE TABLE Author (
    id_author   INTEGER PRIMARY KEY AUTOINCREMENT,
    first_name  VARCHAR(128),
    last_name   VARCHAR(128),
    surname     VARCHAR(255)
);

DROP TABLE IF EXISTS MetadataInfo;
CREATE TABLE MetadataInfo (
    id_metadataInfo INTEGER PRIMARY KEY AUTOINCREMENT,
    name            VARCHAR(255)
);

DROP TABLE IF EXISTS Source;
CREATE TABLE Source (
    id_source       INTEGER PRIMARY KEY AUTOINCREMENT,
    title           VARCHAR(255)
);

DROP TABLE IF EXISTS Source_Authors;
CREATE TABLE Source_Authors (
    id_source_author    INTEGER PRIMARY KEY AUTOINCREMENT,
    related_source      INTEGER,
    related_author      INTEGER,
    FOREIGN KEY(related_source) REFERENCES Source(id_source),
    FOREIGN KEY(related_author) REFERENCES Author(id_author)
);

DROP TABLE IF EXISTS MetadataValue;
CREATE TABLE MetadataValue (
    id_metadataValue    INTEGER PRIMARY KEY AUTOINCREMENT,
    value               VARCHAR(255),
    related_metadata    INTEGER,
    related_source      INTEGER,
    FOREIGN KEY(related_metadata) REFERENCES MetadataInfo(id_metadataInfo),
    FOREIGN KEY(related_source) REFERENCES Source(id_source)
);

DROP TABLE IF EXISTS Quote;
CREATE TABLE Quote (
    id_quote            INTEGER PRIMARY KEY AUTOINCREMENT,
    related_source      INTEGER,
    localization        VARCHAR(255),
    content             TEXT,
    comment             TEXT,
    FOREIGN KEY(related_source) REFERENCES Source(id_source)
);

DROP TABLE IF EXISTS Quote_Authors;
CREATE TABLE Quote_Authors (
    id_quote_author     INTEGER PRIMARY KEY AUTOINCREMENT,
    related_quote       INTEGER,
    related_author      INTEGER,
    FOREIGN KEY(related_author) REFERENCES Author(id_author),
    FOREIGN KEY(related_quote) REFERENCES Quote(id_quote)
);

DROP TABLE IF EXISTS Tag;
CREATE TABLE Tag (
    id_tag          INTEGER PRIMARY KEY AUTOINCREMENT,
    name            VARCHAR(127)
);

DROP TABLE IF EXISTS Quote_Tags;
CREATE TABLE Quote_Tags(
    id_quote_tags       INTEGER PRIMARY KEY AUTOINCREMENT,
    related_quote       INTEGER,
    related_tag         INTEGER,
    FOREIGN KEY(related_tag) REFERENCES Tag(id_tag),
    FOREIGN KEY(related_quote) REFERENCES Quote(id_quote)
);
