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
Name       : metro_postprocess_round_roadcast         
Description: Rounding of float value stocked in roadcast matrix
Work on    : roadcast_data.subsampled_data
Notes      :   
Author     : Francois Fortin
Date       : 1 September 2004
"""

from metro_postprocess import Metro_postprocess

import numpy

import metro_config
import metro_logger
from toolbox import metro_util

_ = metro_util.init_translation('metro_postprocess_round_roadcast')

class Metro_postprocess_round_roadcast(Metro_postprocess):

    def start(self):
        Metro_postprocess.start(self)

        pRoadcast = self.get_infdata_reference('ROADCAST')
        roadcast_data = pRoadcast.get_data_collection()

        iPrecision = \
            metro_config.get_value('DEFAULT_ROADCAST_PREDICTION_PRECISION')

        if roadcast_data != None:
            self.__round_roadcast(roadcast_data.get_subsampled_data())
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                                       _("No roadcast!"))

        pRoadcast.set_data_collection(roadcast_data)

    def __round_roadcast( self, roadcast_data):

        # Get the matrix columns definition
        lStandard_roadcast = metro_config.get_value( \
            'XML_ROADCAST_PREDICTION_STANDARD_ITEMS')
        lExtended_roadcast = metro_config.get_value( \
            'XML_ROADCAST_PREDICTION_EXTENDED_ITEMS')        
        lRoadcast_items = lStandard_roadcast + lExtended_roadcast

        # Get the default value for the accurary of "float" of the roadcast
        iDefault_precision = \
            metro_config.get_value('DEFAULT_ROADCAST_PREDICTION_PRECISION')

        # Process of each column of roadcast
        iItem_id = 0
        for dRoadcast_item in lRoadcast_items:

            # If this column contains float data
            if dRoadcast_item['DATA_TYPE'] == 'REAL':

                # If the accuracy have been specified: use it.
                # Otherwhise use the default value for the roadcast.
                if 'PRECISION' in dRoadcast_item:
                    iPrecision = dRoadcast_item['PRECISION']
                else:
                    iPrecision = iDefault_precision

                # Perform the round operation
                naCol = roadcast_data.get_matrix_col(dRoadcast_item['NAME'])
                naCol = numpy.around(naCol,iPrecision)
                roadcast_data.set_matrix_col(dRoadcast_item['NAME'], naCol)
                
            iItem_id = iItem_id + 1
        
