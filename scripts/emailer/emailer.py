#!/usr/bin/python
import sys, os
import smtplib, email, email.encoders, email.mime.text, email.mime.base
import gspread

DEBUG = True

EMAIL_USERNAME = "email_address@gmail.com"
EMAIL_PASSWORD = "password"
SPREADSHEET_KEY = "0AhnGs-PFWpMhdHI3WlRtb1RHejZEbWN4YmNvQ0RMT2c"

# Note that gspread begins indexing at 1, 
# while the rest of computers start at 0
IS_CANADIAN_ARTIST_COL = 8 # I
IS_PHYSICAL_COL = 10 # K
HAS_BEEN_EMAILED_COL = 14 # O

HAS_BEEN_EMAILED = "Yes"
HAS_NOT_BEEN_EMAILED = "No"


def main(argv=None):
    try:
        gc = gspread.login(EMAIL_USERNAME, EMAIL_PASSWORD)
        worksheet = gc.open_by_key(SPREADSHEET_KEY).sheet1
    except Exception, e:
        print("Error: Couldn't open the spreadsheet", e)
        
    try:
        emailBands(worksheet)
    except Exception, e:
        print("Error emailing bands: ", e)
    
def emailBands(worksheet):
    unCanadianEmails = []
    digitalOnlyEmails = []
    successEmails = []
        
    for row, submissionRow in enumerate(worksheet.get_all_values()):
        if submissionRow[0] == "" or row == 0:
            continue
        
        emailAddress = submissionRow[5]
        emailInfo = {"cellRow": row,
                     "email": emailAddress}
        
        if submissionRow[HAS_BEEN_EMAILED_COL] != HAS_BEEN_EMAILED:            
            if not isCanadianAlbum(submissionRow):
                unCanadianEmails.append(emailInfo)
            elif isDigitalOnly(submissionRow):
                digitalOnlyEmails.append(emailInfo)
            else:
                successEmails.append(emailInfo)
        
    print("Sending physical-only emails")
    physicalEmail = PhysicalFailureEmail(worksheet, digitalOnlyEmails)
    physicalEmail.send()
    print("Sending Can-Con-only emails")
    canadaEmail = CanadianFailureEmail(worksheet, unCanadianEmails)
    canadaEmail.send()
    print("Sending Success Emails")
    successEmail = SuccessEmail(worksheet, successEmails)
    successEmail.send()
            
def isCanadianAlbum(submissionRow):
    return submissionRow[IS_CANADIAN_ARTIST_COL].strip() == "Yes"
    
def isDigitalOnly(submissionRow):
    return "Digitally Released Only" == submissionRow[IS_PHYSICAL_COL].strip()
    
    
class Email():
    
    def __init__(self, worksheet, emailInfoList):
        self.worksheet = worksheet
        self.emailInfoList = emailInfoList
        self.sendSuccesses = []
        self.sendFailures = []
        self.server = None
        self.setEmailServer()
        
    def setEmailServer(self):
        if DEBUG == True:
            emailServer = "localhost"
            emailPort = 25
        else:
            emailServer = "smtp.gmail.com"
            emailPort = 587
    
    def send(self):
        self.connectToEmailServer()
            
        for emailInfo in self.emailInfoList:
            try:
                self.sendEmail(emailInfo['email'])
                self.sendSuccesses.append(emailInfo)
            except Exception as e:
                self.sendFailures.append(emailInfo)
            
        self.server.close()
        self.markEmailSendOutcomes()
        print("Email sends failed for the following addresses: %s" % 
            self.sendFailures)
            
    def connectToEmailServer(self):
        self.server = smtplib.SMTP(self.emailServer, self.emailPort) # or 465 for SSL
        
        if DEBUG == False:
            self.server.ehlo()
            self.server.starttls()
            self.server.login(EMAIL_USERNAME, EMAIL_PASSWORD)
        
    def sendEmail(self, toAddress):
        emailMsg = email.MIMEMultipart.MIMEMultipart('alternative')
        emailMsg["Subject"] = self.subject
        emailMsg["From"] = EMAIL_USERNAME
        emailMsg["To"] = toAddress
        emailMsg.attach(email.mime.text.MIMEText(
            self.htmlEmail, 'html'))
        self.server.sendmail(EMAIL_USERNAME, [toAddress], emailMsg.as_string())
            
    def addRecipient(self, recipient):
        self.emailInfoList.append(recipient)
        
    def markEmailSendOutcomes(self):
        print("Marking emails as sent or failed on the spreadsheet")
        self.markEmailsAsSent()
        self.markEmailsAsFailed()
    
    def markEmailsAsSent(self):
        for emailInfo in self.sendSuccesses:
            self.markEmail(emailInfo, "Yes")
            
    def markEmailsAsFailed(self):
        for emailInfo in self.sendFailures:
            self.markEmail(emailInfo, "No")
    
    def markEmail(self, emailInfo, hasBeenEmailed):
        try:
            print(emailInfo)
            self.worksheet.update_cell(
                emailInfo['cellRow'] + 1, 
                HAS_BEEN_EMAILED_COL + 1, 
                hasBeenEmailed)
        except Exception:
            print("Unable to mark the email %s's sent value as %s" %
                (emailInfo['email'], hasBeenEmailed))
    
        
class SuccessEmail(Email):
    subject = None
    
    htmlEmail = """
    <!DOCTYPE html>
    <head>
        <meta charset="utf-8" />
    </head>
    <body>
        <p>Hello,</p>

        <p>I'm writing to let you know that your submission has met our pre-acceptance criteria, and as such has been sent to the magic pool where the writers drink. Right now, our writers are listening to your music. If it captures one of their hearts, we will contact you to obtain a physical copy of your release.</p>

        <p>Thanks for sending your art to Weird Canada. We always want to hear it, and we always listen.</p>

        <p>Love,</p>
        <p>Weird Canada</p>
        <p>
            <strong>Weird Canada ▽ Wyrd Arts Initiatives</strong>
            <br />
            <a href="http://www.weirdcanada.com/">
                www.weirdcanada.com
            </a>
            <br />
            <a href="https://twitter.com/weirdcanada">
                Twitter
            </a>
             + 
            <a href="http://www.facebook.com/weirdcanada">
                Facebook
            </a>
             + 
            <a href="http://eepurl.com/et2jg">
                Nyws
            </a>
        </p>
    </body>
    """
        
        
class PhysicalFailureEmail(Email):
    subject = None
    
    htmlEmail = """
    <!DOCTYPE html>
    <head>
        <meta charset="utf-8" />
    </head>
    <body>
        <p>Hello,</p>

        <p>Thank you for taking the time to submit your work to Weird Canada. We appreciate your effort in contributing to our documentation of creative expression across Canada.</p>

        <p>Unfortunately, one of our requirements for submissions is that they exist in a physical format. That said, you are always welcome to create a homemade, one-off copy (CDR, cassette, etc.) of your music and re-submit to Weird Canada. For more information on why we require physical submissions, 
            <a href="http://weirdcanada.com/why-we-require-physical-submissions/">
                see here.
            </a>
        </p>
        <p>Thanks for submitting your art to Weird Canada. We always want to hear it, and we always listen.</p>

        <p>Sincerely,</p>
        <p>The Weird Canada Team</p>
        <p>
            <strong>Weird Canada ▽ Wyrd Arts Initiatives</strong>
            <br />
            <a href="http://www.weirdcanada.com/">
                www.weirdcanada.com
            </a>
            <br />
            <a href="https://twitter.com/weirdcanada">
                Twitter
            </a>
             + 
            <a href="http://www.facebook.com/weirdcanada">
                Facebook
            </a>
             + 
            <a href="http://eepurl.com/et2jg">
                Nyws
            </a>
        </p>
    </body>
    """
    
class CanadianFailureEmail(Email):
    subject = None
    
    htmlEmail = """
    <!DOCTYPE html>
    <head>
        <meta charset="utf-8" />
    </head>
    <body>
        <p>Hello,</p>

        <p>Thank you for taking the time to submit your work to Weird Canada. We appreciate your effort in contributing to our documentation of creative expression across Canada. Unfortunately, one of our requirements for accepted submissions is that they come from Canada.</p>

        <p>We hope that our decision does not come as a discouragement. We are always interested in learning about what is going on in other countries, their experimental music culture and the efforts to promote it. So, please do keep in touch, and do not hesitate to fill us in on the exciting things that are happening in and around where you live.
        </p>
        <p>Thanks for sending your art to Weird Canada. We always want to hear it, and we always listen.</p>

        <p>Sincerely,</p>
        <p>The Weird Canada Team</p>
        <p>
            <strong>Weird Canada ▽ Wyrd Arts Initiatives</strong>
            <br />
            <a href="http://www.weirdcanada.com/">
                www.weirdcanada.com
            </a>
            <br />
            <a href="https://twitter.com/weirdcanada">
                Twitter
            </a>
             + 
            <a href="http://www.facebook.com/weirdcanada">
                Facebook
            </a>
             + 
            <a href="http://eepurl.com/et2jg">
                Nyws
            </a>
        </p>
    </body>
    """

if __name__ == "__main__":
    main()
