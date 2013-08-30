#!/usr/bin/python
import sys, os, getopt
import gspread
import traceback
import psycopg2

DEBUG = True

alphabet = {}
for i,a in enumerate(xrange(ord('A'), ord('Z')+1)):
    alphabet[chr(a)] = i

# Note that gspread begins indexing at 1, 
# while the rest of computers start at 0
col_first_name = alphabet['B']
col_last_name = alphabet['C']
col_email = alphabet['D']
col_phone = alphabet['E']
col_city = alphabet['G']
col_interests = alphabet['H']
col_availability = alphabet['I']
col_word = alphabet['J']
col_why = alphabet['K']
col_gender = alphabet['N']
col_address = alphabet['F']
col_notes = alphabet['P']
col_inserted = alphabet['W']

flag_inserted = "yes"

insert_sql = """PREPARE prepared_upsert(text, text, text, text, text, varchar(2), text, text, varchar(10), text, timestamp, text, text, text, text, text, text, text[]) AS SELECT volunteer_upsert($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18)"""

interests_dict = { 
        u'Photography - Photographie':'photography',
        u'Video - Vidéo':'video',
        u'Design - Conception':'design',
        u'Write (Music) - Écrire (Musique)':'write-music',
        u'Write (Music)':'write-music',
        u'Write (Books) - Écrire (Livres)':'write-books',
        u'Write (Books)':'write-books',
        u'Write (Grants) - Écrire (les propositions de subventions)':'write-grants',
        u'Write (Grants)':'write-grants',
        u'Write (General Communications)':'write-general',
        u'Write (General Communications )':'write-general',
        u'Write (Art)':'write-art',
        u'Translation (French)':'translation-french',
        u'Translation (Syllabics)':'translation-syllabics',
        u'Editing & Proofreading':'editing-and-proofreading',
        u'Administration & Data Entry':'admin',
        u'Research':'research',
        u'High Level Strategizing':'strategy',
        u'App Development':'app-development',
        u'Coding':'coding',
        u'SEO':'seo',
        u'Bookkeeping':'bookkeeping',
        u'Outreach':'outreach',
        u'Show Staff (Bar, Door, Merch)':'show-staff',
        u'Postering':'postering',
        u'Printing/Screenprinting':'printing-screenprinting',
        u'Driving':'driving',
        u'Show Promoting':'show-promoting',
        u'Volunteer Sourcing':'volunteer-sourcing',
        u'Music Sourcing':'music-sourcing'
        }

def insert(connection, cursor, first_name, last_name, email, phone, city, interests, availability, word, why, gender, address, notes):
    try: 
        interests_string = "{" + ",".join(interests) + "}"
        why_string = word + "\n" + why + "\n" + notes
        cursor.execute("EXECUTE prepared_upsert(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", (first_name, last_name, email, phone, city, "", availability, why_string, gender, address, "now", "", "", "", "", "", "", interests_string))
        connection.commit()
        return True
    except psycopg2.DatabaseError, e:
        if connection: 
            print "FAILED :( %s" % e
            connection.rollback()
            return False

def main(argv=None):
    username, password, spreadsheet_key, database, database_password = parseArguments(argv)
    
    try:
        gc = gspread.login(username, password)
        worksheet = gc.open_by_key(spreadsheet_key).sheet1
    except gspread.exceptions.AuthenticationError:
        print("Error: Unable to log in - exiting")
        return -1
    except Exception, e:
        print("Error: exiting", e)
        print traceback.print_exc(15)
        return -1

    try:
        connection = psycopg2.connect(database=database, user=database, password=database_password)
        cursor = connection.cursor()
        cursor.execute(insert_sql)
    except Exception, e:
        print("Error connecting to database, exiting", e)
        print traceback.print_exc(15)
        return -1

    for row, volunteer_row in enumerate(worksheet.get_all_values()):
        if volunteer_row[0] != "" and row != 0 and volunteer_row[col_inserted] != flag_inserted:
            first_name = volunteer_row[col_first_name]
            last_name = volunteer_row[col_last_name]
            email = volunteer_row[col_email]
            phone = volunteer_row[col_phone]
            city = volunteer_row[col_city]
            interests_string = volunteer_row[col_interests]
            availability = volunteer_row[col_availability]
            word = volunteer_row[col_word]
            why = volunteer_row[col_why]
            gender = volunteer_row[col_gender]
            address = volunteer_row[col_address]
            notes = volunteer_row[col_notes]

            interests = [ interests_dict[x.strip()] for x in interests_string.split(",") if x.strip() in interests_dict ]

            insert_success = insert(connection, cursor, first_name, last_name, email, phone, city, interests, availability, word, why, gender, address, notes)
            if insert_success:
                worksheet.update_cell(row+1, col_inserted +1, flag_inserted)
       
def parseArguments(argv):
    username = None
    password = None
    spreadsheet = None
    database = None
    database_password = None
    
    try:
        opts, args = getopt.getopt(sys.argv[1:], "u:p:s:d:z:h", [
        "username=", "password=", "spreadsheet=", "database=", "dbpassword=", "help"])
        for opt, arg in opts:
            if opt in ("-h", "--help"):
                print getHelpText()
                sys.exit(2)
            elif opt in ("-u", "--username"):
                username = arg
            elif opt in ("-p", "--password"):
                password = arg
            elif opt in ("-s", "--spreadsheet"):
                spreadsheet = arg
            elif opt in ("-d", "--database"):
                database = arg
            elif opt in ("-z", "--dbpassword"):
                database_password = arg
            else:
                print "Incorrect arguments, -h for help."

    except getopt.GetoptError:
        sys.exit(2)
        
    if not username or not password or not spreadsheet or not database or not database_password:
        print "You need to pass the required arguments. -h for help"
        sys.exit(2)
        
    return username, password, spreadsheet, database, database_password
    
        
def getHelpText():
    print """
    -u      --username      the username of your Google Account
    -p      --password      the password of your Google Account
    -s      --spreadsheet   your Google Spreadsheet key (found in the url).
                            It might look something like this: 
                                0AhnGs-PFWpMhdHI3WlRtb1RHejZEbWN4YmNvQ0RMT2c
    -h      --help          display this message
    """
    
if __name__ == "__main__":
    main()
