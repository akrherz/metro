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
Name       : Metro_postprocess_subsample        
Description: Subsampling roadcast matrix at 20 minutes interval
Work on    : roadcast_data.subsampled_data
Notes      :   
Author     : Francois Fortin
Date       : 10 September 2004
"""

from metro_postprocess import Metro_postprocess

import metro_config
import metro_logger
from toolbox import metro_util
from toolbox import metro_constant
from toolbox import metro_date

_ = metro_util.init_translation('metro_postprocess_subsample_roadcast')

class Metro_postprocess_subsample_roadcast(Metro_postprocess):

    def start(self):
        Metro_postprocess.start(self)

        pRoadcast = self.get_infdata_reference('ROADCAST')
        roadcast_data = pRoadcast.get_data_collection()

        self.__subsample_roadcast(roadcast_data)

        pRoadcast.set_data_collection(roadcast_data)

    def __subsample_roadcast( self, roadcast):

        # Get all roadcast items
        lStandard_roadcast = metro_config.get_value( \
            'XML_ROADCAST_PREDICTION_STANDARD_ITEMS')
        lExtended_roadcast = metro_config.get_value( \
            'XML_ROADCAST_PREDICTION_EXTENDED_ITEMS')        
        lRoadcast_items = lStandard_roadcast + lExtended_roadcast

        
        dElement_Array = {}

        rc_controlled = roadcast.get_controlled_data()

        # Put array in a dictionnary
        for dRoadcast_item in lRoadcast_items:
            sElement = dRoadcast_item['NAME']

            dElement_Array[sElement] = rc_controlled.get_matrix_col(sElement)

        # Get some values necessary for the creation of a complete roadcast
        iObservation_len = roadcast.get_attribute('OBSERVATION_LENGTH')
        fObservation_delta_t = roadcast.get_attribute('OBSERVATION_DELTAT_T')
        iNb_timesteps = roadcast.get_attribute('FORECAST_NB_TIMESTEPS')
        fEndCoupling = (iObservation_len*\
                        metro_constant.fTimeStep/3600.0-fObservation_delta_t)
        sStartDate = metro_config.get_value('INIT_ROADCAST_START_DATE')
        fStartDate = metro_date.parse_date_string(sStartDate)
        
        sMessage = _("Specified roadcast start date: '%s'") % (sStartDate)
        metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,
                                   sMessage)
        #
        # Create the data matrix for roadcast
        #
        rc_subsampled = roadcast.get_subsampled_data()

        nSecondsForOutput = metro_constant.nMinutesForOutput*60
        for i in range(0,iNb_timesteps):
            fCurrentTime = dElement_Array['HH'][i]*3600

            # Forecast at every 20 minutes, i.e. 1200 seconds  
            lRCvect = [0]*rc_subsampled.get_nb_matrix_col()
            lMatrix_line = [None]*rc_subsampled.get_nb_matrix_col()

            # if current time is a 20 minute interval
            # and roadcast time is >= roadcast start date
            if round(fCurrentTime)%nSecondsForOutput == 0 and \
                   dElement_Array['ROADCAST_TIME'][i] >= fStartDate :

                for sElement in dElement_Array.keys():
                    lMatrix_line[rc_subsampled.index_of_matrix_col(sElement)] = \
                      dElement_Array[sElement][i]
                # Ugly stuff.  Don't know. No clue about the 600.
                for j in range(int(max(1,i-600/metro_constant.fTimeStep)),\
                               int(min(iNb_timesteps,i+600/\
                                       metro_constant.fTimeStep))):
                    if dElement_Array['RC'][j] == 0:
                        continue

                    iRC = int(round(dElement_Array['RC'][j]))

                    lRCvect[iRC-1] = lRCvect[iRC-1] +1

                # Identify the highest value:
                nTop = max(lRCvect)
                nRCcode = lRCvect.index(nTop)+1

                lMatrix_line[rc_subsampled.index_of_matrix_col('RC')] = nRCcode

                # Adding the new line in the matrix
                rc_subsampled.append_matrix_row(lMatrix_line)

        fActual_start_date = dElement_Array['ROADCAST_TIME'][0]
        sMessage = _("Actual roadcast start date: '%s'") \
                   % (metro_date.seconds2iso8601(fActual_start_date))
        metro_logger.print_message(metro_logger.LOGGER_MSG_INFORMATIVE,
                                   sMessage)
        roadcast.set_subsampled_data(rc_subsampled)

