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
Name:	       Metro_preprocess_validate_input
Description:  Validate input data to make sure they conform to certain rule. 
               1) Forecast and observation must overlap
               
Auteur: Francois Fortin
Date: 9 novembre 2004
"""

from metro_preprocess import Metro_preprocess

import metro_logger
import metro_config
from toolbox import metro_date
from toolbox import metro_util

_ = metro_util.init_translation('metro_config')

class Metro_preprocess_validate_input(Metro_preprocess):

    def start(self):
        Metro_preprocess.start(self)

        pForecast = self.get_infdata_reference('FORECAST')
        forecast_data = pForecast.get_data_collection()
        pObservation = self.get_infdata_reference('OBSERVATION')
        observation_data = pObservation.get_data_collection()

        self.__validate_roadcast_start_date(observation_data.\
                                            get_controlled_data())

        self.__validate_overlap(forecast_data.get_controlled_data(),
                                observation_data.get_controlled_data())


    def __validate_roadcast_start_date(self, ro_controlled_data):
        """
        Parameters: controlled observation data
        
        Returns: None
        
        Functions Called: 

        Description: If roadcast start date is not set, set it
                     to the date of the last observation.

        Notes: 

        Revision History:
        Author		Date		Reason
        Miguel Tremblay      August 4th 2004
        """
        
        # Get start time
        if metro_config.get_value('INIT_ROADCAST_START_DATE') == "":
            naOT = ro_controlled_data.get_matrix_col('OBSERVATION_TIME')
            fLast_observation_date = naOT[len(naOT)-1]
            sStart_date = metro_date.seconds2iso8601(fLast_observation_date)
            metro_config.set_value('INIT_ROADCAST_START_DATE', sStart_date)
            sMessage = _("Roadcast start date set to the date of\nthe last ") +\
                       _("observation: '%s'") % (sStart_date)
            metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,\
                                       sMessage)            


    def __validate_overlap( self, forecast_data, observation_data ):
        """        
        Parameters: forecast_data, observation_data

        Description: Make sure forecast and observation overlap, if it's
                     not the case abort METRo

        """

        fForecast_start = forecast_data.get_matrix_col('FORECAST_TIME')[0]
        naObservation = observation_data.get_matrix_col('OBSERVATION_TIME')
        fObservation_end = naObservation[len(naObservation)-1]

        if fForecast_start > fObservation_end:
            sForecast_start = metro_date.seconds2iso8601(fForecast_start)
            sObservation_end = metro_date.seconds2iso8601(fObservation_end)
            sError = _("Forecast and observation don't overlap. The date\n") +\
            _("of the first forecast must be before the last date of ") +\
            _("observation.\nFirst Forecast='%s'\nLast Observation='%s'") \
            %(sForecast_start,sObservation_end)

            metro_logger.print_message(metro_logger.LOGGER_MSG_STOP, sError)
