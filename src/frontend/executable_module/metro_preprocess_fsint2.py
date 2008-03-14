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
import numpy

import metro_logger
import Sun
from toolbox import metro_physics
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

    # Date
    nStartDay = None
    nStartMonth = None
    nStartYear = None
    fSunrise = None
    fSunset = None

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
        self.__print_info()
        self.__set_theoretical_flux(forecast_data.get_controlled_data(), \
                                    forecast_data.get_interpolated_data())
        pForecast.set_data_collection(forecast_data)
        pStation.set_data(station_data)        


    def __set_attribute(self, wf_controlled_data,
                        wf_interpolated_data,
                        station_data):
        """
        Set the attributes needed by this clas
        
        Parameters:
        wf_controlled_data (metro_data) : controlled data.  Read-only
        wf_interpolated_data (metro_data) : container of the
        interpolated data.

        Returns: None
        """
        # Lat & Lon
        self.fLat = station_data.get_latitude()
        self.fLon = station_data.get_longitude()
        self.__set_sunrise_sunset(wf_controlled_data)

    def __set_theoretical_flux(self, wf_controlled_data, \
                               wf_interpolated_data):
        """
        The flux value of the forecast are calculated from the
        position of the earth around the sun.

        Notes: All times are in UTC. Since Sun.py gives times over 24h,
        special case is done with %24 (January 15th 2006 modifications).

        Parameters:
        wf_controlled_data (metro_data) : controlled data. Read-only
        wf_interpolated_data (metro_data) : container of the interpolated data.
        """
        # SF
        self.__set_sf(wf_controlled_data, wf_interpolated_data)
        # IR
        self.__set_ir(wf_controlled_data, wf_interpolated_data)
        
    def __print_info(self):
        """
        Print the information about the sunrise/sunset computed for this
        day and emplacement.
        """
        if self.fLon < 0:
            sLon = 'W'
        else:
            sLon = 'E'
        if self.fLat > 0:
            sLat = 'N'
        else:
            sLat = 'S'

        tSunset = metro_date.tranform_decimal_hour_in_minutes(\
            self.fSunset)
        tSunrise = metro_date.tranform_decimal_hour_in_minutes(\
            self.fSunrise)      
        sMessage = _("For the date %d-%d-%d,\n") % \
                   ((self.nStartDay,self.nStartMonth,self.nStartYear)) +\
                   _("at the latitude %0.2f ") %(abs(round(self.fLat,2))) + sLat +\
                   _(" and longitude %0.2f ") %(abs(round(self.fLon,2))) + sLon +\
                   _("\nsunrise is at %d:%d:%d UTC\n") \
                   % ((tSunrise[0], tSunrise[1], tSunrise[2])) +\
                   _("sunset  is at %d:%d:%d UTC") \
                  % ((tSunset[0], tSunset[1], tSunset[2])) 
        metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                  sMessage)

 

    def __set_ir(self, wf_controlled_data, wf_interpolated_data):
        """
        Set the theoretical infrared flux.
           
        Parameters:
        wf_controlled_data (metro_data) : controlled data. Read-only
        """
        npTime = wf_controlled_data.get_matrix_col('Time')
        (npCoeff1, npCoeff2) = self.__get_cloud_coefficient(wf_controlled_data)
        npAT = wf_controlled_data.get_matrix_col('AT')
        npIR = npCoeff1*npAT+npCoeff2
        wf_controlled_data.set_matrix_col('IR', npIR)
        npIR = metro_util.interpolate(npTime, npIR)
        wf_interpolated_data.append_matrix_col('IR', npIR)
        

    def __set_sf(self, wf_controlled_data, wf_interpolated_data):
        """
        Set the theoretical solar flux.
           
        Parameters:
        wf_controlled_data (metro_data) : controlled data. Read-only
        """
        # Get data
        npTime = wf_controlled_data.get_matrix_col('Time')
        npCloudOctal = wf_controlled_data.get_matrix_col('CC')
        npTimeHour =  wf_controlled_data.get_matrix_col('Hour')
        fStartForecastTime = wf_controlled_data.\
                             get_matrix_col('FORECAST_TIME')[0]
        # Get solar fluxes for this cloud cover for this specific day
        npSF = metro_physics.get_sf(npCloudOctal, npTimeHour, \
                                    fStartForecastTime,\
                                    self.fSunrise, self.fSunset,\
                                    self.fLat, self.fLon)


        # Set value in matrix
        wf_controlled_data.set_matrix_col('SF', npSF)

        # Set value in interpolated matrix.
        npSF2  = metro_util.interpolate(npTime, npSF)
        wf_interpolated_data.append_matrix_col('SF',  npSF2)


    def __set_sunrise_sunset(self, wf_controlled_data):
        """
        Description: Get the value of sunrise/sunset for the first
        day of forecast.
   
        Parameters:
        wf_controlled_data (metro_data) : controlled data. Read-only
        
        Set the attribute for sunrise/sunset
        """
        ctimeFirstForecast = wf_controlled_data.get_matrix_col\
                             ('FORECAST_TIME')[0]
        # Get the sunrise and the sunset
        self.nStartYear =  metro_date.get_year(ctimeFirstForecast)
        self.nStartMonth =   metro_date.get_month(ctimeFirstForecast)
        self.nStartDay =  metro_date.get_day(ctimeFirstForecast)
        cSun = Sun.Sun()
        (fSunriseTimeUTC, fSunsetTimeUTC) = cSun.sunRiseSet(\
           self.nStartYear, self.nStartMonth, self.nStartDay,\
           self.fLon, self.fLat)

        self.fSunrise = fSunriseTimeUTC
        self.fSunset = fSunsetTimeUTC


    def __get_cloud_coefficient(self, wf_controlled_data):
        """
        Get the coefficient D1 and D2 as described in the metro article
        p.2030 corresponding to the octal values in npCloudsOctal.

        wf_controlled_data (metro_data) : controlled data. Read-only

        Note: Could be place in metro_physic if npCloudsOctal is given
        in arguement instead of wf_controlled_data.

        Return (npCoeff1, npCoeff2) with coefficients.
        """
        npCloudsOctal = wf_controlled_data.get_matrix_col('CC')
        
        npCoeff1 = npCloudsOctal
        npCoeff2 = npCloudsOctal
        for i in range(0,9):
            fCoeff1 = metro_constant.lCloudsNightCoeff1[i]
            fCoeff2 = metro_constant.lCloudsNightCoeff2[i]
            npCoeff1 = numpy.where(npCloudsOctal == i, fCoeff1, npCoeff1)
            npCoeff2 = numpy.where(npCloudsOctal == i, fCoeff2, npCoeff2)
                 

        return (npCoeff1, npCoeff2) 


    
