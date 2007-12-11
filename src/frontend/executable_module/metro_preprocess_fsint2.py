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

"""
Name:		Metro_preprocess_fsint2
Description: Subroutine correcting the incident solar flux
              to take the sunrise/sunset into account.

Notes: Take a special care with the difference between the interpolated
       values and the raw one. The search for the sunrise/sunset
       is performed on the raw values of SF. The number of time step
       is performed on the raw values of time. The number of time steps
       should normally correspond to the interpolated intervals.
"""

from metro_preprocess import Metro_preprocess

import time
import calendar
from math import pi
from math import sin
from math import cos
import numarray

import metro_logger
import Sun
from toolbox import metro_constant
from toolbox import metro_util
from toolbox import metro_date
from data_module import metro_data


_ = metro_util.init_translation('metro_preprocess_fsint2')


class Metro_preprocess_fsint2(Metro_preprocess):

    ##
    # Class attribute
    ##
    fLat = 0 # Latitude of the station
    fLon = 0 # Longitude of the station

    fEot = None
    fR0r = None
    tDeclsc = ()

#####################################################
    def start(self):
        Metro_preprocess.start(self)
        # Get the attribute of the class
        
        pForecast = self.get_infdata_reference('FORECAST')
        forecast_data = pForecast.get_data_collection()
        pStation = self.get_infdata_reference('STATION')
        station_data = pStation.get_data()


        self.__set_attribute(forecast_data.get_controlled_data(), \
                             forecast_data.get_interpolated_data(), \
                             station_data)
        self.__set_theoretical_flux(forecast_data.get_controlled_data(), \
                                    forecast_data.get_interpolated_data())
        pForecast.set_data_collection(forecast_data)
        pStation.set_data(station_data)        


    def __set_attribute(self, wf_controlled_data,
                        wf_interpolated_data,
                        station_data):
        """
         Name: __set_attribute
        
         Parameters:[I] metro_data wf_controlled_data : controlled data.  Read-only
                    [I] metro_data wf_interpolated_data : container of the
                    interpolated data.
         Returns: None

         Functions Called:  self.forecast_data.get_matrix_col
                            station_data.get_latitude
                            station_data.get_longitude
                            __get_eot

         Description: Set the attributes needed by this class

         Notes: <other information, if any>

         Revision History:
         Author		Date		Reason
         Miguel Tremblay      July 2nd 2004
         """
        
        # Get the cTime of the beginning of data
        nTime = wf_controlled_data.get_matrix_col('FORECAST_TIME')[0]
        # Lat & Lon
        self.fLat = station_data.get_latitude()
        self.fLon = station_data.get_longitude()
        # Set the constant for the position of the earth around the sun
        (self.fEot, self.fR0r, self.tDeclsc) = self.__get_eot(nTime)


    def __set_theoretical_flux(self, wf_controlled_data, \
                               wf_interpolated_data):
        """
        Name: __set_theoretical_flux
        Parameters:[I] metro_data wf_controlled_data : controlled data.
                       Read-only
                   [I] metro_data wf_interpolated_data : container of the
                       interpolated data.
        Returns: None

        Functions Called: wf_controlled_data.get_matrix_col
                  numarray.cos, where, 
                  wf_controlled_data.append_matrix_col
                  metro_util.interpolate
                   

         Description: The flux value of the forecast are calculated from the
                      position of the earth around the sun.

         Notes: All times are in UTC. Since Sun.py gives times over 24h,
                special case is done with %24 (January 15th 2006 modifications).

         Revision History:
         Author		Date		Reason
         Miguel Tremblay      July 29th 2004
         Miguel Tremblay     January 15th 2006
         """
        
        ctimeFirstForecast = wf_controlled_data.get_matrix_col\
                             ('FORECAST_TIME')[0]
        # Get the sunrise and the sunset
        nStartYear =  metro_date.get_year(ctimeFirstForecast)
        nStartMonth =   metro_date.get_month(ctimeFirstForecast)
        nStartDay =  metro_date.get_day(ctimeFirstForecast)
        cSun = Sun.Sun()
        (fSunriseTimeUTC, fSunsetTimeUTC) = cSun.sunRiseSet(\
           nStartYear, nStartMonth, nStartDay,\
           self.fLon, self.fLat)
        if self.fLon < 0:
            sLon = 'W'
        else:
            sLon = 'E'
        if self.fLat > 0:
            sLat = 'N'
        else:
            sLat = 'S'

        tSunset = metro_date.tranform_decimal_hour_in_minutes(fSunsetTimeUTC)
        tSunrise = metro_date.tranform_decimal_hour_in_minutes(fSunriseTimeUTC)      
        sMessage = _("For the date %d-%d-%d,\n") % \
                   ((nStartDay,nStartMonth,nStartYear)) +\
                   _("at the latitude %0.2f ") %(abs(round(self.fLat,2))) + sLat +\
                   _(" and longitude %0.2f ") %(abs(round(self.fLon,2))) + sLon +\
                   _("\nsunrise is at %d:%d:%d UTC\n") \
                   % ((tSunrise[0], tSunrise[1], tSunrise[2])) +\
                   _("sunset  is at %d:%d:%d UTC") \
                   % ((tSunset[0], tSunset[1], tSunset[2])) 
        metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                   sMessage)

        naTimeHour =  wf_controlled_data.get_matrix_col('Hour')
        nTimeHourLength = len(naTimeHour)
        naCloudsOctal = wf_controlled_data.get_matrix_col('CC')
        
        naSft = numarray.zeros(nTimeHourLength, type=numarray.Float)
        naCoeff = numarray.zeros(nTimeHourLength, type=numarray.Float)
        
        ###### In the night, the solar flux is null ###############
        for i in range(0, nTimeHourLength):
            # Current hour is needed for the computation of
            # fDh in the theoritical solar flux.
            nCurrentHour = (naTimeHour[i])%24
            # atmospheric forecast is before the sunrise
            # or after the sunset
            if self.__in_the_dark(nCurrentHour, fSunriseTimeUTC, \
                                  fSunsetTimeUTC,):
                    naSft[i] = 0
            else:
                # Position of the sun around the earth, in radian
                fDh =  pi*(nCurrentHour/12.0 + self.fLon/180 - 1) + self.fEot
                fCosz = self.tDeclsc[0] + \
                        self.tDeclsc[1]*cos(fDh)
                naSft[i] = max(0.0, fCosz)*self.fR0r
        naCoeff =  -1.56e-12*naSft**4 + 5.972e-9*naSft**3 -\
                    8.364e-6*naSft**2  + 5.183e-3*naSft - 0.435

        naCoeff = numarray.where(naCoeff > 0, naCoeff, 0.0)
        # Set naCloudsPercent to be able to reference it in the
        #  numarray.where method.
        naCloudsPercentDay = naCloudsOctal
        naCloudsPercentNight1 = naCloudsOctal
        naCloudsPercentNight2 = naCloudsOctal
        # Correction based on the clouds
        for i in range(0,9):
            nPercentDay = metro_constant.lCloudsDay[i]
            nPercentNight1 = metro_constant.lCloudsNight1[i]
            nPercentNight2 = metro_constant.lCloudsNight2[i]
            naCloudsPercentDay = numarray.where(naCloudsOctal == i,\
                                      nPercentDay, naCloudsPercentDay)
            naCloudsPercentNight1 = numarray.where(naCloudsOctal == i,\
                                                   nPercentNight1, \
                                                   naCloudsPercentNight1)
            naCloudsPercentNight2 = numarray.where(naCloudsOctal == i,\
                                                   nPercentNight2, \
                                                   naCloudsPercentNight2)

        # TODO MT: Voir les implications de cette passe.
        # There is a mix up with the 0-based octal used in fortran
        #  See rofile2.f
        naCloudsPercentDay = numarray.where(naCloudsPercentDay == 0, 1.0 \
                                             , naCloudsPercentDay)
        naCloudsPercentNight1 = numarray.where(naCloudsPercentNight1 == 0, 3.79 \
                                             , naCloudsPercentNight1)
        naCloudsPercentNight2 = numarray.where(naCloudsPercentNight2 == 0,214.7 \
                                               , naCloudsPercentNight2)

        # Solar flux
        naSF3 = naSft * naCoeff * naCloudsPercentDay

        # Infra-red flux 
        naAT = wf_controlled_data.get_matrix_col('AT')
        naIR = naCloudsPercentNight1*naAT+naCloudsPercentNight2
        wf_controlled_data.append_matrix_col('IR', naIR)

        # Interpolate
        naTime = wf_controlled_data.get_matrix_col('Time')
        naIR = metro_util.interpolate(naTime, naIR, \
                                      metro_constant.fTimeStep)
        wf_controlled_data.append_matrix_col('SF', naSF3)
        naSF  = metro_util.interpolate(naTime, naSF3, \
                                      metro_constant.fTimeStep)
        wf_interpolated_data.append_matrix_col('SF',  naSF)

        wf_interpolated_data.append_matrix_col('IR', naIR)

    def __get_eot(self, nTime):
        """
         Name: __get_eot

         Parameters: [I] int nTime : cTime of the beginning of the forecast


         Returns: tuple (double fEot, double fR0r, tuple tDeclsc)
                  dEot: Correction for the equation of time 
                  dR0r: Corrected solar constant for the equation of time
                  tDeclsc: Declinaison

         Functions Called: __Solcons
                           time.gmtime
                           calendar.isleap
                           cos, sin
                   
                   

         Description: Subroutine computing the part of the equation of time
                      needed in the computing of the theoritical solar flux
                      Correction originating of the CMC GEM model.

         Revision History:
         Author		Date		Reason
         Miguel Tremblay       June 30th 2004
         """
        
        # Convert ctime to python tuple for time.
        # see http://www.python.org/doc/current/lib/module-time.html
        tDate = time.gmtime(nTime)
        # Julian date is the 7th argument
        fJulianDate = tDate[7] + tDate[3]/24.0
        # Check if it is a leap year
        if(calendar.isleap(tDate[0])):
            fDivide = 366.0
        else:
            fDivide = 365.0
        # Correction for "equation of time"
        fA = fJulianDate/fDivide*2*pi
        self.fR0r = self.__Solcons(fA)*metro_constant.fConsol
        fRdecl = 0.412*cos((fJulianDate+10.0)*2.0*pi/fDivide-pi)
        fDeclsc1 = sin(self.fLat*pi/180.0)*sin(fRdecl)
        fDeclsc2 = cos(self.fLat*pi/180.0)*cos(fRdecl)
        self.tDeclsc = (fDeclsc1, fDeclsc2)
        # in minutes
        self.fEot = 0.002733 -7.343*sin(fA)+ .5519*cos(fA) -9.47*sin(2.0*fA) \
               -3.02*cos(2.0*fA) -0.3289*sin(3.*fA) -0.07581*cos(3.0*fA) \
               -0.1935*sin(4.0*fA) -0.1245*cos(4.0*fA)
        # Express in fraction of hour
        self.fEot = self.fEot/60.0
        # Express in radians
        self.fEot = self.fEot*15*pi/180.0

        return (self.fEot, self.fR0r, self.tDeclsc)

    def __Solcons(self, dAlf):
        """
        Name: __Solcons
        
        Parameters: [I] double dAlf : Solar constant to correct the excentricity
        
        Returns: double dVar : Variation of the solar constant

        Functions Called: cos, sin
         
        Description:  Statement function that calculates the variation of the
          solar constant as a function of the julian day. (dAlf, in radians)
         
        Notes: <other information, if any>
         
        Revision History:
        Author		Date		Reason
        Miguel Tremblay      June 30th 2004
        """
        
        dVar = 1.0/(1.0-9.464e-4*sin(dAlf)-0.01671*cos(dAlf)- \
                    + 1.489e-4*cos(2.0*dAlf)-2.917e-5*sin(3.0*dAlf)- \
                    + 3.438e-4*cos(4.0*dAlf))**2
        return dVar

    def __in_the_dark(self, nCurrentTime, fSunrise, fSunset):
        """
        Name: __in_the_dark
        
        Parameters: [I] int nCurrentTime. Current time. In [0,24]
                    [I] float fSunrise. Sunrise time as returned by Sun.py
                      Value could be > 24.
                    [I] float fSunset. Sunset time as returned by Sun.py
                      Value could be > 24.
                      
        Returns: boul bDark

        Functions Called: cos, sin
         
        Description: Sometimes, value returned by Sun.py are over 24. Since
          the time of day is needed with modulo 24, a special check must be
          performed. See https://gna.org/bugs/?8277 for more details.
         
        Notes: <other information, if any>
         
        Revision History:
        Author		Date		Reason
        Miguel Tremblay      January 15th 2006
        """
        bDark=False
        
        if ((fSunset%24 > fSunrise%24) and \
            (nCurrentTime < fSunrise or \
             nCurrentTime > fSunset)) or \
             (not(fSunset%24 > fSunrise%24) and \
              (nCurrentTime > fSunset%24 and \
               nCurrentTime < fSunrise%24)):
            bDark=True

        return bDark
        
