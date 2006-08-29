#
# METRo : Model of the Environment and Temperature of Roads
# METRo is Free and is proudly provided by the Government of Canada
# Copyright (C) 2006 Environment Canada

#  Questions or bugs report: metro@ec.gc.ca
#  METRo repository: https://gna.org/projects/metro/
#  Documentation: http://documentation.wikia.com/wiki/METRo
#
#
# Code contributed by:
#  Miguel Tremblay - Canadian meteorological center
#  Francois Fortin - Canadian meteorological center
#
#  $LastChangedDate$
#  $LastChangedRevision$
#
########################################################################
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#
#

####################################################
# Name:	 metro_date.py
# Description: Date and time functions for METRo
# Notes:
####################################################
import xml.utils.iso8601
import math
import time
import datetime
import os
import sys
import string

from toolbox import metro_util
from toolbox import metro_constant

_ = metro_util.init_translation('metro_date')

# sDate must conform to ISO 8601. ref. http://www.w3.org/TR/NOTE-datetime
# return float ctime
def parse_date_string( sDate ):

    if sDate != None:
        try:
            fDate = xml.utils.iso8601.parse(sDate)
        except ValueError, sError:
            sMessage = _("The following error occured when parsing the ") +\
                       _("ISO 8601 date:\n %s") % (sError)
            fDate = metro_constant.NaN
            raise "METRoDateError", sMessage        
    else:
        sMessage = _("The following error occured when parsing a ") +\
                   _("date:\nNo date string to convert")
        raise "METRoDateError", sMessage
    
    return fDate

def seconds2iso8601( fDate ):
    return xml.utils.iso8601.tostring(round(fDate))
    # Must replace the decimal of the seconds
    
##################################################
# Returns the current date in iso8601 format
##################################################
def get_current_date_iso8601():
    fCurrentTime  = time.time()
    sCurrentTime = seconds2iso8601(fCurrentTime)
    return sCurrentTime

def time_zone_convert(tm, source_zone, dest_zone):
    '''Convert a broken-down time (time.struct_time or tuple) from
    one named time zone to another.'''
    old_zone = os.environ['TZ']
    try:
        os.environ['TZ'] = source_zone
        time.tzset()
        stamp = time.mktime(tm)

        os.environ['TZ'] = dest_zone
        time.tzset()
        return time.mktime(time.localtime(stamp))
    finally:
        os.environ['TZ'] = old_zone
        time.tzset()

def is_daylight_saving_time(fTime, sTime_zone):
    bRslt = False
    tmp_time = get_struct_time(fTime, sTime_zone)
    if( tmp_time[8] == 1 ):
        bRslt = True
    return bRslt

def get_year( fTime, sTime_zone="UTC" ):
    tmp_time = get_struct_time(fTime, sTime_zone)
    return tmp_time[0]

def get_month( fTime, sTime_zone="UTC" ):
    tmp_time = get_struct_time(fTime, sTime_zone)
    return tmp_time[1]

def get_day( fTime, sTime_zone="UTC" ):
    tmp_time = get_struct_time(fTime, sTime_zone)
    return tmp_time[2]

def get_hour( fTime, sTime_zone="UTC" ):
    tmp_time = get_struct_time(fTime, sTime_zone)
    return tmp_time[3]

def get_minute( fTime, sTime_zone="UTC" ):
    tmp_time = get_struct_time(fTime, sTime_zone)
    return tmp_time[4]

def get_system_tz():
    return os.environ['TZ']

def get_struct_time( fTime, sTime_zone="UTC" ):
    sOld_zone = os.environ['TZ']
    rslt = (-1,-1,-1,-1,-1,-1,-1,-1,-1)

    try:
        os.environ['TZ'] = sTime_zone
        time.tzset()
        rslt = time.localtime(fTime)
    finally:
        os.environ['TZ'] = sOld_zone
        time.tzset()

    return rslt


def get_short_time_zone(tm, sTime_zone):

    # get first part of time zone spec ( before first comma ) 
    # see http://www.qnx.com/developers/docs/momentics621_docs/neutrino/ ...
    #     lib_ref/global.html#TheTZEnvironmentVariable
    #
    # ex: EST5EDT4,M4.1.0/02:00:00,M10.5.0/02:00:00 ->  EST5EDT4
    if ',' in sTime_zone:
        lTz = string.split(sTime_zone,',')
        sTime_zone = lTz[0]
        
    # extract time zone letter
    #
    # ex: EST5EDT4 -> ESTEDT
    sTz_letter = ""
    for letter in sTime_zone:
        if letter.isalpha():
            sTz_letter = sTz_letter + letter

    if is_daylight_saving_time(tm, sTime_zone):
        return sTz_letter[-3:]
    else:
        return sTz_letter[:3]

####################################################
# Name: getTimeCorrection
#
# Parameters: [[I] string sTimeZone : Represente les 3 lettres du fuseau horaire
#
# Returns: int dLCorr : retourne la difference par rapport au UTC
#
# Description: Fonction qui etait auparavant dans lib_gen.f
#
# Notes: <
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay       june 28th 2004   Journee d'election federale
#####################################################

def getTimeCorrection(sTimeZone):

    if( sTimeZone == 'PST' or sTimeZone == 'HNP' ):
        fLCorr = -8.0
    elif ( sTimeZone == 'MST' or sTimeZone == 'HNR' ):
        fLCorr = -7.0
    elif ( sTimeZone == 'CST' or sTimeZone == 'HNC' ):
        fLCorr = -6.0
    elif ( sTimeZone == 'EST' or sTimeZone == 'HNE' ):
        fLCorr = -5.0
    elif ( sTimeZone == 'AST' or sTimeZone == 'HNA' ):
        fLCorr = -4.0
    elif ( sTimeZone == 'NST' or sTimeZone == 'HNT' ):
        fLCorr = -3.5
    elif ( sTimeZone == 'PDT' or sTimeZone == 'HAP' ):
        fLCorr = -7.0
    elif ( sTimeZone == 'MDT' or sTimeZone == 'HAR' ):
        fLCorr = -6.0
    elif ( sTimeZone == 'CDT' or sTimeZone == 'HAC' ):
        fLCorr = -5.0
    elif ( sTimeZone == 'EDT' or sTimeZone == 'HAE' ):
        fLCorr = -4.0
    elif ( sTimeZone == 'ADT' or sTimeZone == 'HAA' ):
        fLCorr = -3.0
    elif ( sTimeZone == 'NDT' or sTimeZone == 'HAT' ):
        fLCorr = -2.5
    else:
        sTimeZoneError = _("Unknown time zone: %s") % (sTimeZone)
        raise "METRoDateError", sTimeZoneError

    return fLCorr

####################################################
# Name: get_elapsed_time
#
# Parameters: [[I] ftime1 float : ctime de la premiere date]
#             [[I] ftime2 float : ctime de la deuxieme date]
#
# Returns: The time difference between ftime1 and ftime2
#
# Description: Retourne la difference ftime1 - ftime2
#
# Notes: 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay       august 2nd 2004   
#####################################################
def get_elapsed_time(ftime1, ftime2, sTimeZone="UTC", sUnit="hours"):

    # Transform the ctime in the datetime.datetime class
    cTime1 = datetime.datetime(get_year(ftime1,sTimeZone), \
                               get_month(ftime1,sTimeZone), \
                               get_day(ftime1,sTimeZone), \
                               get_hour(ftime1,sTimeZone), \
                               get_minute(ftime1,sTimeZone))
    cTime2 = datetime.datetime(get_year(ftime2,sTimeZone),\
                               get_month(ftime2,sTimeZone), \
                               get_day(ftime2,sTimeZone), \
                               get_hour(ftime2,sTimeZone), \
                               get_minute(ftime2,sTimeZone))
        
    # Get the timedelta object from the substraction.
    cTimeDifference = cTime1-cTime2
    if(sUnit == "hours"):
        nNbrHours = cTimeDifference.days*24.0+cTimeDifference.seconds/3600.0
        return nNbrHours
    elif(sUnit == "days"):
        nNbrDays = cTimeDifference.days \
                   + cTimeDifference.seconds/3600.0*1/24.0
        return nNbrDays
    elif(sUnit == "seconds"):
        nNbrSeconds = ftime1-ftime2
        return nNbrSeconds
    else:
        sInvalidParameterError =_("Invalid criteria in get_elapsed_time: sUnit = %s")\
                                     % (sUnit)
        raise "METRoDateError", sInvalidParameterError

def list_to_date(lDate):

    old_zone = os.environ['TZ']

    try:
        iYear      = lDate[0]
        iMonth     = lDate[1]
        iDay       = lDate[2]
        iHour      = lDate[3][0]
        iMin       = lDate[3][1]
        sTime_zone = lDate[4]


        os.environ['TZ'] = sTime_zone
        time.tzset()

        #convertion de la date en format ctime
        tDate = (iYear, iMonth, iDay, iHour, iMin, 0, 0, 0, -1)        
        fRslt = time.mktime(tDate)  #convertion en ctime
    except:
        fRslt = None

    os.environ['TZ'] = old_zone
    time.tzset()

#    print "oldtime=",time.ctime(fRslt)
    return fRslt


####################################################
# Name: tranform_decimal_hour_in_minutes
#
# Parameters: [[I] float fTimeHour : Time in decimal form. Eg 1.90 for
#                       1h54:00
#
# Returns: [nHour, nMinute, nSecond]
#
# Description: Return an array containing the hour, the minutes and the secondes,
#   respectively.
#
# Notes: 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay       October 29th 2004   
#####################################################
def tranform_decimal_hour_in_minutes(fTimeHour):
    # Extract decimal from integer part
    tModHour = math.modf(fTimeHour)
    nHour = int(tModHour[1])
    fDecimalHour = tModHour[0]
    # Transform decimal in minutes
    fMinute = fDecimalHour*60
    # Again, extract the decimal and the integer part
    tModMinute = math.modf(fMinute)
    nMinute = int(tModMinute[1])
    fDecimalMinute = tModMinute[0]
    # Transform decimal in seconds
    fSecond = fDecimalMinute*60    
    # Again, extract the decimal and the integer part
    tModSecond = math.modf(fSecond)
    nSecond = int(tModSecond[1])

    return (nHour, nMinute, nSecond)

def get_iso_8601(nYear, nMonth, nDay, nHour=0, nMinute=0, nSecond=0):
    """
    Convert numbers of date in the iso 8601 format
    We assume UTC time zone.
    """

    sYear = str_to_at_least_two_digits(nYear)
    sMonth = str_to_at_least_two_digits(nMonth)
    sDay = str_to_at_least_two_digits(nDay)

    if (nHour == nMinute == nSecond == 0):
        sIso = sYear + '-' + sMonth + '-' + sDay
    else:
        sHour = str_to_at_least_two_digits(nHour)
        sMinute = str_to_at_least_two_digits(nMinute)
        sSecond = str_to_at_least_two_digits(nSecond)
        sIso = sYear + '-' + sMonth + '-' + sDay+ 'T' +\
           sHour + ':' + sMinute + ':' + sSecond + 'Z'

    return sIso

def str_to_at_least_two_digits(nNumber):
    """
    Take an int and return '01' instead of '1'.
    There must be a 'regular' way to do it in python but
    it takes longer to find it than to write it.
    """     
    sRes = str(nNumber)
    if len(sRes) < 2:
        sRes = '0'+sRes

    return sRes
