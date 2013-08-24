BEGIN;
  CREATE TABLE wc_volunteer (
    id SERIAL PRIMARY KEY, -- 1
    first_name TEXT NOT NULL, -- 2
    last_name TEXT NOT NULL, -- 3
    email TEXT NOT NULL, -- 4
    phone VARCHAR(12) NOT NULL, -- 5
    city TEXT NOT NULL, -- 6
    province varchar(2) NOT NULL, -- 7
    availability TEXT, -- 8
    why TEXT, -- 9
    gender VARCHAR(10), -- 10
    address TEXT, -- 11
    birthday TIMESTAMP, -- 12
    bio_english TEXT, -- 13
    bio_francais TEXT, -- 14
    byline_english TEXT, -- 15
    byline_francais TEXT, -- 16
    website TEXT, -- 17
    image TEXT, -- 18
    UNIQUE (first_name, last_name, email)
  );

  CREATE INDEX wc_volunteer_first_name_idx ON wc_volunteer (lower(first_name));
  CREATE INDEX wc_volunteer_last_name_idx ON wc_volunteer (lower(last_name));
  CREATE INDEX wc_volunteer_email_idx ON wc_volunteer (lower(email));

  CREATE TABLE wc_volunteer_interest (
    id SERIAL PRIMARY KEY,
    interest TEXT NOT NULL,
    volunteer_id INTEGER REFERENCES wc_volunteer(id) ON DELETE CASCADE
  );

  CREATE INDEX wc_volunteer_interest_idx ON wc_volunteer_interest (lower(interest));
END;

    



  -- interests in different table
