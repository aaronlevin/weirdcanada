BEGIN;
  CREATE TABLE wc_volunteer (
    id SERIAL PRIMARY KEY,
    first_name TEXT NOT NULL,
    last_name TEXT NOT NULL,
    email TEXT NOT NULL,
    phone VARCHAR(12) NOT NULL,
    city TEXT NOT NULL,
    province varchar(2) NOT NULL,
    availability TEXT,
    why TEXT,
    gender VARCHAR(10),
    address TEXT,
    birthday TIMESTAMP,
    bio_english TEXT,
    bio_francais TEXT,
    byline_english TEXT,
    byline_francais TEXT,
    website TEXT,
    image TEXT,
    UNIQUE (first_name, last_name, email)
  );

  CREATE TABLE wc_volunteer_interest (
    id SERIAL PRIMARY KEY,
    interest TEXT NOT NULL,
    volunteer_id INTEGER REFERENCES wc_volunteer(id) ON DELETE CASCADE
  );

  CREATE INDEX wc_volunteer_interest_idx ON wc_volunteer_interest (lower(interest));
END;

    



  -- interests in different table
