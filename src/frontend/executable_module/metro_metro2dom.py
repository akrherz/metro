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

from metro_module import Metro_module

import string

import metro_config
import metro_logger
from toolbox import metro_xml
from toolbox import metro_util
from data_module import metro_data_collection

_ = metro_util.init_translation('metro_metro2dom')

class Metro_metro2dom( Metro_module ):

    ##
    # attributs de la classe
    ##
    domForecast    = None
    domRoadcast    = None

    forecast_data    = None
    observation_data = None
    station_data     = None
    roadcast_data    = None
    
    ##
    # methodes redefinies
    ##
    def start( self ):
        Metro_module.start(self)

        pForecast = self.get_infdata_reference('FORECAST')
        forecast_data = pForecast.get_data_collection()
        pRoadcast = self.get_infdata_reference('ROADCAST')
        roadcast_data = pRoadcast.get_data_collection()
        
        # Create forecast
        if forecast_data != None:
            domForecast = \
                self.__create_forecast(forecast_data.get_original_data())
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                _("No forecast, can't create DOM forecast"))
            domForecast = None

        # Create roadcast
        if roadcast_data != None:
            domRoadcast = \
                self.__create_roadcast(roadcast_data.get_subsampled_data())
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                _("No roadcast, can't create DOM roadcast"))
            domRoadcast = None

        pForecast.set_output_information(domForecast)
        pRoadcast.set_output_information(domRoadcast)

    def stop( self ):
        Metro_module.stop(self)

    def get_receive_type( self ):
        return Metro_module.DATATYPE_DATA_OUT

    def get_send_type( self ):
        return Metro_module.DATATYPE_DOM_OUT

    def __create_forecast( self, data ):

        #
        # construction du DOM et du root de forecast
        #

        sRoot_xpath = metro_config.get_value('XML_FORECAST_XPATH_ROOT')
        domDoc = metro_xml.create_dom(sRoot_xpath)
        nodeRoot = metro_xml.get_dom_root(domDoc)

        #
        # construction du header de forecast
        #

        # concatenation de toutes les cles du header
        lSkeys = metro_config.get_value('XML_FORECAST_HEADER_STANDARD_ITEMS')
        lEkeys = metro_config.get_value('XML_FORECAST_HEADER_EXTENDED_ITEMS')
        lHeader_keys = lSkeys + lEkeys

        # construction du xpath
        sHeader_xpath = metro_config.get_value('XML_FORECAST_XPATH_HEADER')

        self.__create_header(domDoc, nodeRoot, sHeader_xpath,
                             lHeader_keys, data)


        #
        # construction de la matrix de forecast
        #

        # concatenation de toutes les types de predictions
        lSkeys = \
               metro_config.get_value('XML_FORECAST_PREDICTION_STANDARD_ITEMS')
        lEkeys = \
               metro_config.get_value('XML_FORECAST_PREDICTION_EXTENDED_ITEMS')
        lPrediction_keys = lSkeys + lEkeys

        # construction du xpath
        sPrediction_xpath = metro_config.get_value( \
            'XML_FORECAST_XPATH_PREDICTION')

        self.__create_matrix(domDoc, nodeRoot, sPrediction_xpath,
                             lPrediction_keys, data)

        return domDoc


    def __create_roadcast( self, data ):

        #
        # construction du DOM et du root de roadcast
        #

        sRoot_xpath = metro_config.get_value('XML_ROADCAST_XPATH_ROOT')
        domDoc = metro_xml.create_dom(sRoot_xpath)
        nodeRoot = metro_xml.get_dom_root(domDoc)

        #
        # construction du header de forecast
        #

        # concatenation de toutes les cles du header
        lHeader_keys = \
            metro_config.get_value('XML_ROADCAST_HEADER_STANDARD_ITEMS') + \
            metro_config.get_value('XML_ROADCAST_HEADER_EXTENDED_ITEMS')

        # construction du xpath
        sHeader_xpath = metro_config.get_value('XML_ROADCAST_XPATH_HEADER')

        self.__create_header(domDoc, nodeRoot, sHeader_xpath,
                             lHeader_keys, data)


        #
        # construction de la matrix de forecast
        #

        # concatenation de toutes les types de predictions
        lPrediction_keys = \
            metro_config.get_value('XML_ROADCAST_PREDICTION_STANDARD_ITEMS') + \
            metro_config.get_value('XML_ROADCAST_PREDICTION_EXTENDED_ITEMS')

        # construction du xpath
        sPrediction_xpath = \
            metro_config.get_value('XML_ROADCAST_XPATH_PREDICTION')

        self.__create_matrix(domDoc, nodeRoot, sPrediction_xpath,
                             lPrediction_keys, data)

        return domDoc


    def __create_header( self, domDoc, nodeRoot, sHeader_xpath,
                         lHeader_keys, metro_data ):

        lHeader_xpath = string.split(sHeader_xpath,"/")

        # supprime le root xml du xpath
        sHeader_xpath = string.join(lHeader_xpath[2:],'/')

        # si necessaire, creation d'une node pour contenir le header
        if sHeader_xpath != "":
            nodeHeader = metro_xml.mkdir_xpath(domDoc, nodeRoot, sHeader_xpath)
        else:
            nodeHeader = nodeRoot

        dHeader = metro_data.get_header()

        # creation du header
        metro_xml.create_node_tree_from_dict(domDoc, nodeHeader,
                                             lHeader_keys, dHeader)


    def __create_matrix( self, domDoc, nodeRoot, sPrediction_xpath,
                         lPrediction_keys, metro_data):


        lPrediction_xpath = string.split(sPrediction_xpath,"/")

        # creation de la branche qui contiendra les predictions
        iNb_element = len(lPrediction_xpath)
        if iNb_element > 3:
            # conserve le path entre le root et la feuille
            # ex: /niveau1/niveau2/niveau3/niveau4 -> niveau2/niveau3
            sPrediction_xpath = string.join(lPrediction_xpath[2:iNb_element-1],
                                            '/')
            # creation de la branche
            nodePrediction = metro_xml.mkdir_xpath(domDoc, nodeRoot,
                                                   sPrediction_xpath)
        else:
            nodePrediction = nodeRoot

        # extration du nom d'une node prediction
        sPrediction_node_name = string.join(lPrediction_xpath[iNb_element-1:],
                                            '/')

        #
        # creation des donnees
        naMatrix   = metro_data.get_matrix()
        metro_xml.create_node_tree_from_matrix(domDoc, nodePrediction,
                                               sPrediction_node_name,
                                               lPrediction_keys,
                                               metro_data, naMatrix)
