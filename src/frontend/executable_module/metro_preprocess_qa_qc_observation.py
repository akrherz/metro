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
Name:	       Metro_preprocess_qa_qc_observation
Description: QA and QC for the RWIS observation are performed here.
Notes: 
Auteur: Miguel Tremblay
Date: 4 aout 2004
"""

from metro_preprocess import Metro_preprocess

import numarray

import metro_config
import metro_logger
from toolbox import metro_constant
from toolbox import metro_date
from toolbox import metro_util

_ = metro_util.init_translation('metro_preprocess_qa_qc_observation')


##
# Class attributes
##

ERROR_EMPTY_MATRIX = "sEmptyMatrixError"

class Metro_preprocess_qa_qc_observation(Metro_preprocess):

    def start(self):
        Metro_preprocess.start(self)

        try:
        
            pForecast = self.get_infdata_reference('FORECAST')
            forecast_data = pForecast.get_data_collection()
            pObservation = self.get_infdata_reference('OBSERVATION')
            observation_data = pObservation.get_data_collection()
        
            Metro_preprocess.start(self)


            self.__remove_bad_arg(observation_data.get_controlled_data())
            self.__set_time(observation_data.get_controlled_data())
            self.__check_time_order(observation_data.get_controlled_data(),\
                                    forecast_data.get_controlled_data())
            self.__check_TA_TD(observation_data.get_controlled_data(),
                            observation_data)
            self.__validate(observation_data.get_controlled_data(),
                            observation_data)
            self.__set_time_difference(observation_data.get_controlled_data(),\
                                       forecast_data.get_controlled_data(),
                                       observation_data)
            
            self.__set_coupling_instruction(observation_data.\
                                            get_controlled_data(),\
                                            observation_data.\
                                            get_interpolated_data(),\
                                            observation_data)

            pForecast.set_data_collection(forecast_data)
            pObservation.set_data_collection(observation_data)

        except ERROR_EMPTY_MATRIX:
            sMessage = _("No valid observations. Halting METRo")
            metro_logger.print_message(metro_logger.LOGGER_MSG_STOP,\
                                       sMessage)



    def __set_time(self, ro_controlled_data):
        """
        Name: __set_time
        Parameters: metro_data controlled_data : controlled observation data

        Returns: None
        
        Functions Called: metro_data.get_matrix_col
                          numarray.zeros
                          metro_date.get_hour, get_minute
                          metro_date.get_elapsed_time
                          metro_data.append_matrix_col

        Description: Put the value of the time in seconds.
                      Set it in the matrix.

        Notes: 

        Revision History:
        Author		    Date		Reason
        Miguel Tremblay      August 4th 2004
        """
        
        naOT = ro_controlled_data.get_matrix_col('OBSERVATION_TIME')
        naTime = numarray.zeros(len(naOT))
        naTime[0] = metro_date.get_hour(naOT[0])*3600 + \
                    metro_date.get_minute(naOT[0])*60
        if len(naOT) == 0:
            sMessage = _("No valid observation")
            metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                       sMessage)
            return 
        for i in range(1,len(naOT)):
            fTimeElapsed =  metro_date.get_elapsed_time(naOT[i], \
                                                        naOT[i-1],\
                                                        "UTC", "seconds")
            naTime[i] = naTime[i-1]+fTimeElapsed
            
        # Registered.
        ro_controlled_data.append_matrix_col('Time', naTime)
    
    def __remove_bad_arg(self, ro_controlled_data):
        """
        Name: __remove_bad_arg
        Parameters: metro_data controlled_data : controlled observation data
        Returns: None

        Functions Called: metro_data.get_matrix_col
                          numarray.where, nonzero
                          metro_logger.print_message
                          metro_data.del_matrix_row
                          metro_date.get_hour
                          metro_config.get_value

        Description: Remove the wrong measure of surface temperature
                     in the controlled observations.

        Notes: 

        Revision History:
        Author		Date		Reason
        Miguel Tremblay      August 4th 2004
        """

        ################ Check road surface temperature ###############
        naST = ro_controlled_data.get_matrix_col('ST')
        # More than nRoadTemperatureHigh degrees
        naBad = numarray.where(naST > metro_constant\
                               .nRoadTemperatureHigh , 1, 0)
        if len(naBad) > 0:
            naBadIndices = (numarray.nonzero(naBad))[0]
            if len(naBadIndices) > 0:
                sMessage = _("Invalid road temperature")
                metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,
                                           sMessage)
                for i in range(0,len(naBadIndices)):
                    nIndice = naBadIndices[i]
                    sMessage = _("%d th  temperature is %f") %  \
                               ( nIndice, round(naST[nIndice],2)) 
                    metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,
                                               sMessage)
                
                ro_controlled_data.del_matrix_row(naBadIndices)
        # or less than nRoadTemperatureMin
        naST = ro_controlled_data.get_matrix_col('ST')
        naBad = numarray.where(naST < metro_constant.nRoadTemperatureMin , 1, 0)
        if len(naBad) > 0:
            naBadIndices = (numarray.nonzero(naBad))[0]
            ro_controlled_data.del_matrix_row(naBadIndices)

    def __check_time_order(self, ro_controlled_data, wf_controlled_data):
        """
        Name: __check_time_order

        Parameters: metro_data controlled_data : controlled observation data

        Returns: None
        
        Functions Called: metro_data.get_matrix_col
                          metro_util.get_difference_array
                          numarray.where, nonzero, arange
                          metro_date.get_day, get_hour, get_minute
                          metro_data.del_matrix_row
                          metro_logger.print_message

        Description: Check if the time of the observation are in order.  
                     Cut the information that are spaced by more than 240 minutes.
        Notes: 

        Revision History:
        Author		Date		Reason
        Miguel Tremblay      August 4th 2004
        """
        
        naTime = ro_controlled_data.get_matrix_col('Time')
        naCheck = metro_util.get_difference_array(naTime)
        # If a gap of more than nGapMinuteObservation
        #  minutes is identify, cut the value before.
        naCheck = metro_util.get_difference_array(naTime)        
        naBad = numarray.where( naCheck > metro_constant.\
                                nGapMinuteObservation*60, 1, 0)
        naBadIndice =  (numarray.nonzero(naBad))[0]
        if len(naBadIndice) > 0:
            sMessage =  _("More than %d minutes between 2 measures")\
                          % (metro_constant.nGapMinuteObservation)
            metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                       sMessage)
            naOT = ro_controlled_data.get_matrix_col('OBSERVATION_TIME')
            for i in range(0,len(naBadIndice)):
                nIndice = naBadIndice[i]
                sMessage = _("Indice: %d") % (nIndice)
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                           sMessage)
                sMessage =  _("Cutoff time: day:%d hour:%d minute:%d") %\
                      (metro_date.get_day(naOT[nIndice]),\
                       metro_date.get_hour(naOT[nIndice]),\
                       metro_date.get_minute(naOT[nIndice]))
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                           sMessage)

            
            toto = numarray.arange(0,naBadIndice[len(naBadIndice)-1]+1) 
            ro_controlled_data.del_matrix_row(toto)
        naTime = ro_controlled_data.get_matrix_col('Time')
        naBad = numarray.where( naCheck < 0, 1, 0)
        naBadIndice = (numarray.nonzero(naBad))[0]
        # Accept 1 value under zero because the last value of
        #  naBadIndice = naCheck[len(naCheck)-1] - naCheck[0]
        if len(naBadIndice) > 1:
            sMessage = _("Time of observation are not in order. ") + \
                       _("Check the %d th value") %(naBadIndice[1])
            metro_logger.print_message(metro_logger.LOGGER_MSG_STOP,\
                                      sMessage )
        # Remove the values that are equal.
        naBad = numarray.where( naCheck == 0, 1, 0)
        naBadIndice = (numarray.nonzero(naBad))[0]
        ro_controlled_data.del_matrix_row(naBadIndice)


########################################################

        naFT = wf_controlled_data.get_matrix_col('FORECAST_TIME')
        
        naOT = ro_controlled_data.get_matrix_col('Time')
        nHourStart = metro_date.get_hour(naFT[0])
        naDiff = - naOT + nHourStart*3600
        naBad = numarray.where(naDiff > metro_constant.\
                               nHourForExpirationOfObservation*3600, 1, 0)
        if len(naBad) > 0:
            naBadIndices = (numarray.nonzero(naBad))[0]
            if len(naBadIndices) > 0:
                naBadIndices = (numarray.nonzero(naBad))[0]
                sMessage = _("Observation is more than %d hours")  \
                           % ( metro_constant.nHourForExpirationOfObservation)\
                           + _("before the first roadcast")
                metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,
                                           sMessage)
                for i in range(0,len(naBadIndices)):
                    nIndice = naBadIndices[i]
                    sMessage = _("Indice: %d") % (nIndice)
                    metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                               sMessage)
                    ro_controlled_data.del_matrix_row(naBadIndices)
            
        # Get start time
        sStart_time = metro_config.get_value('INIT_ROADCAST_START_DATE')
        # If start time is not specified, default will be used
        if sStart_time == "":
            return

        # Check if the observation are not before the start of the roadcast if specified.
        fStart_time = metro_date.parse_date_string(sStart_time)
        naOT = ro_controlled_data.get_matrix_col('Time')\
               +nHourStart*3600
        naDiff = - naOT + int(metro_date.get_hour(fStart_time))*3600
        naBad = numarray.where(naDiff > metro_constant\
                               .nHourForExpirationOfObservation*3600, 1, 0)
        if len(naBad) > 0:
            naBadIndices = (numarray.nonzero(naBad))[0]
            if len(naBadIndices) > 0:
                sMessage = _("Observation after the first roadcast time of METRo")
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                           sMessage)
                
                sMessage = _("Threshold: %d") \
                           % (int(metro_date.get_hour(fStart_time))*3600 \
                              +  int(metro_date.get_month(fStart_time))*60)
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                           sMessage)
                for i in range(0,len(naBadIndices)):
                    nIndice = naBadIndices[i]
                    sMessage = _("Time difference: %f") \
                               % (naDiff[nIndice])
                    metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                               sMessage)
                    ro_controlled_data.del_matrix_row(naBadIndices)


####################################################
# Name: __validate
#
# Parameters: metro_data road_controlled_data : controlled observation data
#
# Returns: None
#
# Functions Called: metro_data.get_matrix_col
#                   numarray.where,
#                   metro_data.set_attribute
#
# Description: Set the attributes in road_data_collection to tell if the
#  values are in accordance of the criterium.
#
# Notes: I didn't put the check who verify if the dew point is always
#         greater than the air temperature. 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay      August 4th 2004
#####################################################
    def __validate(self, ro_controlled_data, observation_data):
        naSST = ro_controlled_data.get_matrix_col('SST')
        naAT = ro_controlled_data.get_matrix_col('AT')
        naTD = ro_controlled_data.get_matrix_col('TD')
        naWS = ro_controlled_data.get_matrix_col('WS')

        # Check SST #######################################
        naCheck = numarray.where(naSST > metro_constant.nSubSurRoadTmpHigh, 0, 1)
        naCheck = numarray.where(naSST < metro_constant.nSubSurRoadTmpMin, 0,\
                                 naCheck)

        if len(naCheck) > 0:
            # Special case, first element is not valid
            if naCheck[0] == 0:
                i = 1
                while (naCheck[i] == 0):
                    i = i+1
                    if i == len(naCheck): # No valid sub surface temperature
                        sMessage = _("No valid sub-surface temperature (element <sst>) in observation file %s")  %\
                                   (metro_config.\
                                    get_value('FILE_OBSERVATION_FILENAME'))
                        metro_logger.print_message(metro_logger.LOGGER_MSG_STOP,\
                                                   sMessage)
                         
                fCurrent = naSST[i]
            for i in range(0,len(naCheck)):
                if naCheck[i] == 1: # Good value
                    fCurrent = naSST[i]
                    continue
                else:
                    naSST[i] = fCurrent

        ro_controlled_data.set_matrix_col('SST', naSST)
        observation_data.set_attribute('SST_VALID', numarray.ones(len(naSST)))
            
        # Check AT ###########################################
        naCheck = numarray.where(naAT > metro_constant.nAirTempHigh , 0, 1)
        naCheck = numarray.where(naAT < metro_constant.nAirTempMin , 0, naCheck)
        if len(naCheck) > 0:
            observation_data.set_attribute('AT_VALID', naCheck)
            
        # Check TD ##########################################
        naCheck = numarray.where(naTD > metro_constant.nAirTempHigh, 0, 1)
        naCheck = numarray.where(naTD < metro_constant.nAirTempMin, 0, naCheck)
        naCheck = numarray.where(naTD > naAT , 0, naCheck)
        if len(naCheck) > 0:
            observation_data.set_attribute('TD_VALID', naCheck)
        
        # Check WS ###########################################
        naCheck = numarray.where(naWS > metro_constant.nMaxWindSpeed, 0, 1)
        naCheck = numarray.where(naWS < 0, 0, naCheck)

        if len(naCheck) > 0:
            observation_data.set_attribute('WS_VALID', naCheck)


####################################################
# Name: __set_road_condition
#
# Parameters: metro_data road_controlled_data : controlled observation data
#
# Returns: None
#
# Functions Called: metro_data.get_matrix_col
#                   numarray.where.
#
# Description: Transform the road condition (SC) into boolean field
#
# Notes: OBSOLETE Miguel March 3rd 2005
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay      August 5th 2004
#####################################################
    def __set_road_condition(self, ro_controlled_data):
        naSC = ro_controlled_data.get_matrix_col('SC')        
        naSC = numarray.where(naSC == 33, 0, 1)
        ro_controlled_data.set_matrix_col('SC', naSC) 

####################################################
# Name: __set_coupling_instruction
#
# Parameters: metro_data road_controlled_data : controlled observation data
#
# Returns: None
#
# Functions Called: metro_data.get_matrix_col
#                   collection_data.get_attribute
#                   collection_data.set_attribute
#
# Description: Set the boolean field that will indicate how to perform
#  the coupling stage, based on what observations are available.
#
# Notes: 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay      August 13th 2004
#####################################################
    def __set_coupling_instruction(self, ro_controlled_data, \
                                   ro_interpolated_data, \
                                   observation_data):
        # Take any of the column of the observation to check the dimensions.
        naAT = ro_controlled_data.get_matrix_col('AT') 
        naTime = ro_controlled_data.get_matrix_col('Time')
        nNbr30Seconds = (naTime[len(naTime)-1]-naTime[0]) \
                        /metro_constant.fTimeStep

        # Initialize the boolean field
        bNoObs = [0, 0, 0, 0]
        # Check if there is any data for initialisation
        fDeltaT =  observation_data.get_attribute('DELTA_T')

        if fDeltaT <= 0:
            bNoObs[0] = 1
        # Less than 3 hours of observation        
        if nNbr30Seconds-fDeltaT*3600/30. < metro_constant.nThreeHours*3600/30:
            bNoObs[1] = 1
        # No valid observation
        if len(naAT) == 0:
            bNoObs[2] = 1
            
        # One valid observation
        if len(naAT) == 1:
            bNoObs[3] = 1
            
        # Set the variable
        observation_data.set_attribute('NO_OBS',bNoObs)

        
####################################################
# Name: __set_time_difference
#
# Parameters: metro_data road_controlled_data : controlled observation data
#              metro_data forecast_controlled_data : controlled forecast data
#
# Returns: None
#
# Functions Called: metro_data.get_controlled_data
#                   collection_data.set_attribute
#                   metro_date.get_elapsed_time
#                   metro_date.get_hour, get_day
#
# Description: Set the time difference between the beginning of observation
#   and the beginnig of the roadcast.
#
# Notes: 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay      August 16th 2004
#####################################################
    def __set_time_difference(self, ro_controlled_data, wf_controlled_data, \
                              observation_data):
        # Compute the time difference between the first forecast time
        #  and the beginning of the observation        
        StartForecast = wf_controlled_data.get_matrix_col('FORECAST_TIME')[0]
        
        StartObservation = \
            ro_controlled_data.get_matrix_col('OBSERVATION_TIME')[0]
        fTimeElapsed = metro_date.get_elapsed_time(StartForecast, \
                                                   StartObservation, \
                                                   "UTC", "hours")

        observation_data.set_attribute('DELTA_T', fTimeElapsed)

        sMessage = _("First atmospheric forecast: %s")  %\
                   (metro_date.seconds2iso8601(StartForecast))
        metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                   sMessage)

        sMessage = _("First valid observation   : %s") % \
                   (metro_date.seconds2iso8601(StartObservation))
        metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                   sMessage)
        

####################################################
# Name: __check_TA_TD
#
# Parameters: 
#
# Returns: None
#
# Functions Called: metro_data.get_matrix_col
#                   numarray.where.
#
# Description: When the dew point is over the the air temperature, replace
#  it by the air temperature.
#
# Notes: 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay      March 1st 2005
#####################################################
    def __check_TA_TD(self, ro_controlled_data, observation_data):
        naTD = ro_controlled_data.get_matrix_col('TD')
        naAT = ro_controlled_data.get_matrix_col('AT')

        # First check, if TD > AT, replace TD by AT
        naTD = numarray.where(naTD > naAT, naAT, naTD)

        ro_controlled_data.set_matrix_col('TD', naTD) 


