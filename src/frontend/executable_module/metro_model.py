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
import time
import numarray

import metro_logger
import metro_config
from toolbox import metro_util
from toolbox import metro_date
from toolbox import metro_constant
from data_module import metro_data_collection_output
from data_module import metro_data
from data_module import metro_infdata

_ = metro_util.init_translation('metro_model')

# To call the C function.
from model import macadam

class Metro_model(Metro_module):

    def get_receive_type( self ):
        return Metro_module.DATATYPE_DATA_IN

    def get_send_type( self ):
        return Metro_module.DATATYPE_DATA_OUT

    def start( self ):
        Metro_module.start(self)

        pForecast = self.get_infdata_reference('FORECAST')
        forecast_data = pForecast.get_data_collection()
        pObservation = self.get_infdata_reference('OBSERVATION')
        observation_data = pObservation.get_data_collection()
        pStation = self.get_infdata_reference('STATION')
        station_data = pStation.get_data()

        if metro_config.get_value('T_BYPASS_CORE') == False:

            self.__send_data_to_metro_core(forecast_data,
                                           observation_data,
                                           station_data )
            
            roadcast_data = self.__create_roadcast_collection(forecast_data,
                                                              observation_data,
                                                              station_data)
        else:
            roadcast_data = None
            metro_logger.print_message(
                metro_logger.LOGGER_MSG_INFORMATIVE,
                _("Bypassing METRo core, roadcast not created."))

        pForecast.set_data_collection(forecast_data)
        pObservation.set_data_collection(observation_data)
        pStation.set_data(station_data)

        # creer et ajouter infdata
        # Creation and adding infdata
        infdata_roadcast = metro_infdata.Metro_infdata(
            'ROADCAST', metro_infdata.DATATYPE_METRO_DATA_COLLECTION)
        infdata_roadcast.set_data_collection(roadcast_data)
        self.add_infdata(infdata_roadcast)
        
            
    def stop( self ):
        Metro_module.stop(self)

    def __get_nb_timesteps( self, forecast ):
        wf_data = forecast.get_interpolated_data()
        naFT = wf_data.get_matrix_col('Time')

        return len(naFT)


    def __get_observation_lenght( self, observation ):
        obs_data = observation.get_interpolated_data()
        lTime_obs = obs_data.get_matrix_col('Time').tolist()
        return len(lTime_obs)        

    def __get_observation_delta_t( self, observation ):
        return observation.get_attribute('DELTA_T')        
        
    def __send_data_to_metro_core( self, forecast, observation, station ):

        wf_data = forecast.get_interpolated_data()
        ro_data = observation.get_interpolated_data()
        cs_data = station

        # Start time from model is the last observation
        sStart_time = metro_config.get_value('DATA_ATTRIBUTE_LAST_OBSERVATION')
        print sStart_time
        fStart_time = metro_date.parse_date_string(sStart_time)


        iModel_start_y = metro_date.get_year(fStart_time)
        sMessage = _("year: [%s]") % (iModel_start_y)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        iModel_start_m = metro_date.get_month(fStart_time)
        sMessage = _("month: [%s]") % (iModel_start_m)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        iModel_start_d = metro_date.get_day(fStart_time)
        sMessage = _("day: [%s]") % (iModel_start_d)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        iModel_start_h = metro_date.get_hour(fStart_time)
        sMessage = _("hour: [%s]") % (iModel_start_h)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        dStation_header = cs_data.get_header()

        # test observation
        dObservation_header = ro_data.get_header()
        lObservation_data   = ro_data.get_matrix()
        sMessage = "Observation_header=" + str(dObservation_header)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                  sMessage)        
        sMessage = "Observation_data=" + str(lObservation_data)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)
        
        # test forecast
        dForecast_header = wf_data.get_header()
        lForecast_data   = wf_data.get_matrix()

        sMessage = "Forecast_header=" + str(dForecast_header)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)        
        sMessage = "Forecast_data=" +  str(lForecast_data)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        # start roadlayer MATRIX

        naLayerType  = cs_data.get_matrix_col('TYPE')
        lLayerType = naLayerType.astype(numarray.Int32).tolist()
        lLayerThick = cs_data.get_matrix_col('THICKNESS').tolist()
        nNbrOfLayer = len(lLayerType)

        sMessage = _("Number of layer=") +  str(nNbrOfLayer)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        # Append an empty box for the manuel mode
        lLayerType.append(0)
        lLayerThick.append(0.0)
        

        sMessage = _("roadlayer type=") + str(lLayerType)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)
        sMessage = _("roadlayer thick=") + str(lLayerThick)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)
        # end roadlayer MATRIX

        fTime =  dStation_header['PRODUCTION_DATE']
        fTimeForecast = dForecast_header['PRODUCTION_DATE']
        sTimeZone = dStation_header['TIME_ZONE']
        sMessage = _("timezone=") + sTimeZone
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)
        
        fIssue_time = fStart_time
        
        sMessage = _("issue time=") + time.ctime(fIssue_time)
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        
        wforiginal = forecast.get_original_data()

        ft = wforiginal.get_matrix_col('FORECAST_TIME')


        ##################################################################
        # Forecast
        #  Get the interpolated values.
        wf_interpolated_data =  forecast.get_interpolated_data()
        naAT =  wf_interpolated_data.get_matrix_col('AT')
        lAT = naAT.tolist()
        lQP = wf_interpolated_data.get_matrix_col('QP').tolist()
        naWS = wf_interpolated_data.get_matrix_col('WS')
        lWS = naWS.tolist()
        naTD = wf_interpolated_data.get_matrix_col('TD') 
        lTD = naTD.tolist()
        lAP = wf_interpolated_data.get_matrix_col('AP').tolist()
        lSF = wf_interpolated_data.get_matrix_col('SF').tolist()
        lIR = wf_interpolated_data.get_matrix_col('IR').tolist()
        lFT = wf_interpolated_data.get_matrix_col('Time').tolist()
        lPI = wf_interpolated_data.get_matrix_col('PI').tolist()
        lSC = wf_interpolated_data.get_matrix_col('SC').tolist()

        # Number of 30 seconds step.
        naFT = wf_interpolated_data.get_matrix_col('Time')
        nNbrTimeSteps = self.__get_nb_timesteps(forecast)
        lAH = wf_interpolated_data.get_matrix_col('AH').tolist()

        # Observation data ###############################################
        ro_interpolated_data = observation.get_interpolated_data()
        lAT_obs = ro_interpolated_data.get_matrix_col('AT').tolist()
        lST_obs =  ro_interpolated_data.get_matrix_col('ST').tolist()
        lSST_obs =  ro_interpolated_data.get_matrix_col('SST').tolist()
        lTime_obs = ro_interpolated_data.get_matrix_col('Time').tolist()

        nLenObservation = self.__get_observation_lenght(observation)        
        fDeltaTMetroObservation = self.__get_observation_delta_t(observation)
        
        # Concatenate the information to send it to C.
        npSWO1 = observation.get_attribute('SST_VALID_INTERPOLATED')\
                 .astype(numarray.Int32)
        npSWO2 = observation.get_attribute('AT_VALID_INTERPOLATED')\
                 .astype(numarray.Int32)
        npSWO3 = observation.get_attribute('TD_VALID_INTERPOLATED')\
                 .astype(numarray.Int32)
        npSWO4 = observation.get_attribute('WS_VALID_INTERPOLATED')\
                 .astype(numarray.Int32)
        npSWO = numarray.zeros(4*metro_constant.nNL)        
        for i in range(0,len(npSWO1)):
               npSWO[4*i] = npSWO1[i]
               npSWO[4*i+1] = npSWO2[i]
               npSWO[4*i+2] = npSWO3[i] 
               npSWO[4*i+3] = npSWO4[i] 
        lSWO = npSWO.astype(numarray.Int32).tolist()
        
        bNoObs = observation.get_attribute('NO_OBS')
        
        sMessage = _( "------------station config START---------------------")
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)
                
        sShort_time_zone = metro_date.get_short_time_zone\
                           (fIssue_time,sTimeZone)

        sMessage = _( "Short time zone = ") + sShort_time_zone
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        tLatlon = dStation_header['COORDINATE']
        fLat = tLatlon[0]
        fLon = tLatlon[1]
        sMessage = _( "lat,lon: ")+ "(" + str(fLat) + ", " + str(fLon) + ")"
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)

        bFlat = cs_data.get_station_type()

        sMessage = _("------------station config END---------------------")
        metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                   sMessage)
        
        bSilent = not metro_config.get_value('INIT_LOGGER_SHELL_DISPLAY')

        metro_logger.print_message(metro_logger.LOGGER_MSG_EXECPRIMARY,
                                   _("Start sending data to METRo core"))

        bEchec = []
        macadam.Do_Metro(bFlat,\
                         fLat, fLon, \
                         lLayerThick, nNbrOfLayer, lLayerType, \
                         lAT, lQP, lWS, lAP, lSF, lIR, lFT, lPI, lSC,\
                         lAT_obs,lST_obs, lSST_obs, \
                         lAH, lTime_obs, lSWO, bNoObs,\
                         fDeltaTMetroObservation, nLenObservation, \
                         nNbrTimeSteps, bSilent)

        bEchec = (macadam.get_echec())[0]
        # Check if the execution of the model was a succes:
        if bEchec != 0:
            sError_message = _("Fatal error in METRo physical model.") 
            metro_logger.print_message(metro_logger.LOGGER_MSG_STOP,
                                       sError_message)
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_EXECPRIMARY,
                                       _("End of METRo core"))

    def __create_roadcast_collection( self, forecast, observation, station ):
        

        # Creation of the Metro_data object for the roadcast
        lStandard_items = metro_config.get_value( \
            'XML_ROADCAST_PREDICTION_STANDARD_ITEMS')
        lExtended_items = metro_config.get_value( \
            'XML_ROADCAST_PREDICTION_EXTENDED_ITEMS')        
        lItems = lStandard_items + lExtended_items
        roadcast = metro_data.Metro_data(lItems)
        # Extraction of forecast data used to create the roadcasts.
        wf_data = forecast.get_interpolated_data()
                
        #
        # Generate the header of roadcast
        #

        # extraction of informations
        sRoadcast_version = \
            metro_config.get_value('FILE_ROADCAST_CURRENT_VERSION')
        sRoadcast_station = station.get_station_name()
        fRoadcast_production_date = time.time()
        
        # Adding the informations to the header
        roadcast.set_header_value('VERSION',sRoadcast_version)
        roadcast.set_header_value('ROAD_STATION',sRoadcast_station)
        roadcast.set_header_value('PRODUCTION_DATE',fRoadcast_production_date)
        roadcast.set_header_value('LATITUDE', station.get_header()['COORDINATE'][0])
        roadcast.set_header_value('LONGITUDE', station.get_header()['COORDINATE'][1])
        roadcast.set_header_value('FILETYPE','roadcast')

        #
        # Generate the roadcast matrix of data
        #

        # Extraction of data used by metro_core in the computation
        iObservation_len = self.__get_observation_lenght(observation)
        fObservation_delta_t = self.__get_observation_delta_t(observation)
        iNb_timesteps = self.__get_nb_timesteps(forecast)


        # Extraction of roadcast data computed by metro_core
        lRA = (macadam.get_ra())[:iNb_timesteps]
        lSN = (macadam.get_sn())[:iNb_timesteps]
        lRC = (macadam.get_rc())[:iNb_timesteps]
        lST = (macadam.get_rt())[:iNb_timesteps]
        lFV = (macadam.get_fv())[:iNb_timesteps]
        lSF = (macadam.get_sf())[:iNb_timesteps]
        lIR = (macadam.get_ir())[:iNb_timesteps]
        lFC = (macadam.get_fc())[:iNb_timesteps]
        lFA = (macadam.get_fa())[:iNb_timesteps]
        lFG = (macadam.get_g())[:iNb_timesteps]
        lBB = (macadam.get_bb())[:iNb_timesteps]
        lFP = (macadam.get_fp())[:iNb_timesteps]
        lSST =  (macadam.get_sst())[:iNb_timesteps]

        # TODO MT: Le +30 est la pour que l'output soit au bon moment.
        #  Il y a eu un probleme dans la conversion entre le C et le fortran
        #  qui fait en sorte qu'il y a un decalage d'un indice.  Il faudra que
        #  ce soit corrige.
        naRT = wf_data.get_matrix_col('FORECAST_TIME')[:iNb_timesteps]
        naRT = naRT + 30
        naHH = wf_data.get_matrix_col('Time')[:iNb_timesteps]
        naAT = wf_data.get_matrix_col('AT')[:iNb_timesteps]
        # 3.6 is to convert from m/s to km/h
        naWS = wf_data.get_matrix_col('WS')[:iNb_timesteps]*3.6
        naTD = wf_data.get_matrix_col('TD')[:iNb_timesteps]
        naQP_SN = wf_data.get_matrix_col('SN')[:iNb_timesteps]
        naQP_RA = wf_data.get_matrix_col('RA')[:iNb_timesteps]
        naCC = wf_data.get_matrix_col('CC')[:iNb_timesteps]


        roadcast.init_matrix(iNb_timesteps, roadcast.get_nb_matrix_col())

        # Data added to the roadcast matrix
        roadcast.set_matrix_col('RA', lRA)
        roadcast.set_matrix_col('SN', lSN)
        roadcast.set_matrix_col('RC', lRC)
        roadcast.set_matrix_col('ST', lST)
        roadcast.set_matrix_col('ROADCAST_TIME', naRT)
        roadcast.set_matrix_col('HH', naHH)
        roadcast.set_matrix_col('AT', naAT)
        roadcast.set_matrix_col('WS', naWS)
        roadcast.set_matrix_col('TD', naTD)
        roadcast.set_matrix_col('QP-SN', naQP_SN)
        roadcast.set_matrix_col('QP-RA', naQP_RA)
        roadcast.set_matrix_col('IR', lIR)
        roadcast.set_matrix_col('SF', lSF)
        roadcast.set_matrix_col('FV', lFV)
        roadcast.set_matrix_col('FC', lFC)
        roadcast.set_matrix_col('FA', lFA)
        roadcast.set_matrix_col('FG', lFG)
        roadcast.set_matrix_col('BB', lBB)
        roadcast.set_matrix_col('FP', lFP)
        roadcast.set_matrix_col('CC', naCC)
#        print "ST=" + str(lST)
#        print "SST=" + str(lSST)
        roadcast.set_matrix_col('SST', lSST)

        # Creation of the object Metro_data_collection for the roadcast
        lStandard_attributes = metro_config.get_value( \
            'DATA_ATTRIBUTE_ROADCAST_STANDARD')
        lExtended_attributes = metro_config.get_value( \
            'DATA_ATTRIBUTE_ROADCAST_EXTENDED')        
        lAttributes = lStandard_attributes + lExtended_attributes
        
        roadcast_collection = \
            metro_data_collection_output.\
            Metro_data_collection_output(roadcast,lAttributes)

        
        # Writing of data needed for the roadcast.
        roadcast_collection.set_attribute('OBSERVATION_LENGTH',
                                          iObservation_len)
        roadcast_collection.set_attribute('OBSERVATION_DELTAT_T',
                                          fObservation_delta_t)
        roadcast_collection.set_attribute('FORECAST_NB_TIMESTEPS',
                                          iNb_timesteps)

        return roadcast_collection
