BEGIN;
  CREATE OR REPLACE FUNCTION volunteer_upsert(
    var_first_name TEXT, 
    var_last_name TEXT, 
    var_email TEXT, 
    var_phone VARCHAR(12) DEFAULT '', 
    var_city TEXT DEFAULT '', 
    var_province VARCHAR(2) DEFAULT '', 
    var_availability TEXT DEFAULT '', 
    var_why TEXT DEFAULT '', 
    var_gender VARCHAR(10) DEFAULT '', 
    var_address TEXT DEFAULT '', 
    var_birthday TIMESTAMP DEFAULT 'now', 
    var_bio_english TEXT DEFAULT '', 
    var_bio_francais TEXT DEFAULT '', 
    var_byline_english TEXT DEFAULT '',
    var_byline_francais TEXT DEFAULT '',
    var_website TEXT DEFAULT '',
    var_image TEXT DEFAULT '', 
    var_interests TEXT[] DEFAULT '{}'
  ) RETURNS void AS $$
  DECLARE
    new_volunteer_id BIGINT;
    var_interest TEXT;
  BEGIN
    SELECT id 
      INTO new_volunteer_id
      FROM wc_volunteer 
      WHERE first_name = var_first_name AND last_name = var_last_name AND email = var_email;
    IF FOUND THEN
      -- update
      UPDATE wc_volunteer 
      SET
        first_name = var_first_name, 
        last_name = var_last_name,
        email = var_email,
        phone = var_phone,
        city = var_city,
        province = var_province,
        availability = var_availability,
        why = var_why,
        gender = var_gender,
        address = var_address, 
        birthday = var_birthday, 
        bio_english = var_bio_english,
        bio_francais = var_bio_francais, 
        byline_english = var_byline_english,
        byline_francais = var_byline_francais,
        website = var_website,
        image = var_image
      WHERE id = new_volunteer_id;

      FOREACH var_interest IN ARRAY var_interests
      LOOP
        UPDATE wc_volunteer_interest SET interest = var_interest, volunteer_id = new_volunteer_id;
      END LOOP;
    ELSE
      -- insert
      INSERT INTO wc_volunteer 
        (id, first_name, last_name, email, phone, city, province, availability, why, gender, address, birthday, bio_english, bio_francais, byline_english, byline_francais, website, image)
      VALUES (
        default,
        var_first_name, -- 2
        var_last_name, -- 3
        var_email, -- 4
        var_phone , -- 5
        var_city, -- 6
        var_province, -- 7
        var_availability, -- 8
        var_why, -- 9
        var_gender, -- 10
        var_address, -- 11
        var_birthday, -- 12
        var_bio_english, -- 13
        var_bio_francais, -- 14
        var_byline_english, -- 15
        var_byline_francais, -- 16
        var_website, -- 17
        var_image -- 18
      )
      RETURNING id INTO new_volunteer_id;
      FOREACH var_interest IN ARRAY var_interests
      LOOP
        -- insert
        INSERT INTO wc_volunteer_interest VALUES (default, var_interest, new_volunteer_id);
      END LOOP;
    END IF; 
  END;
  $$ LANGUAGE plpgsql;
END;


