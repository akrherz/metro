# -*- coding: UTF8 -*-
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


import os
import sys
import getopt

import metro_logger
from toolbox import metro_config_validation
from toolbox import metro_xml
from toolbox import metro_xml_dtd
from toolbox import metro_util
from external_lib.Plist_config import plist_writer
from external_lib.Plist_config import plist_reader

_ = metro_util.init_translation('metro_config')




#===============================================================================
#
# Nom:         metro_config
#
# Auteur:      Francois Fortin
#
# Date:        30/03/2004
#
# Description: Module permettant de centraliser les configuration de metro.
#              Les configurations sont conserve dans un dictionnaire
#              de dictionnaire.
#
# TODO:        Lecture de la ligne de  commande
#              Lecture du fichier de config
#              NB La ligne de commande peut specifier le nom d'un
#              fichier de config
#
#===============================================================================


# Configuration pour la traduction des messages en differentes langues
METRO_CONFIG_GETTEXT_PACKAGE  = "metro_config"
METRO_CONFIG_GETTEXT_LOCALEDIR = "./locale"

# definition des constantes
CFG_METRO_VERSION="3.0.3"
CFG_METRO_DATE="2007-02-27"


# provenance de la valeur( ligne de commande, fichier de configuration,
#                         valeur "hardcoder", constante interne a metro )
# NB: 1. la ligne de commande a preseance sur le fichier de config qui a
#        preseance sur les valeurs "hardcoder"
#     2. On ne devrait pas redefinir les constantes interne a metro dans
#        un fichier de configuration ou sur la ligne de commande
CFG_INTERNAL    = 0
CFG_HARDCODED   = 1
CFG_CONFIGFILE  = 2
CFG_COMMANDLINE = 3

CFG_SHORT_OPTIONS = "ho:v:c:l:"
CFG_LONG_OPTIONS  = ["help","version",
                     "input-observation-ref=",
                     "input-forecast=", "input-observation=",
                     "input-station=", 
                     "output-forecast=", "output-roadcast=",
                     "bypass-core",
                     "generate-dtd-catalog",
                     "config=","generate-config=","log-file=","verbose-level=",
                     "selftest", "silent", "roadcast-start-date=", "lang="]



dConfig = {}

def read_config_file( sFilename ):

    try:
        sConfig_file = open(sFilename).read()
    except IOError, sError:
        dConf = {}

        sIOError =  _("Cant open METRo configuration file:'%s' test\n") \
                   % (sFilename) +\
                   _("The following error occured:\n%s") % (sError)

        metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                        sIOError)

        sys.exit(2)        
    else:
                
        plreader = plist_reader.Plist_reader()
# DESACTIVATION DE LA VALIDATION
#        try:
#            metro_xml.validate_string(sConfig_file)
#        except "metroValidationError", sError:
#            sMessage = _("Fatal error when validating configuration file.\n" +
#                         "The error is: %s\nThe configuration file:\n%s") \
#                         % (sError,sConfig_file)
#            metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
#                                            sMessage)
#            sys.exit(2)
        try: 
            dConf = plreader.read(sConfig_file)
        except:
            dConf = {}
            sError = _("Error when reading METRo configuration\n") + \
                     _("file:'%s'. Please make sure that the file\nhave ") % (sFilename)+ \
                     _("valid XML synthax and is not corrupted. You can ") + \
                     _("check it with the command 'xmllint %s'. You can\n") % (sFilename)+ \
                     _("also generated a new one with the option: %s.\n") \
                     % ("--generate-config",)
            metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                            sError)
            sys.exit(2)
        else:
            sSuccess = _("Configuration file:'%s' loaded with success") \
                       % (sFilename)
            metro_logger.print_init_message(metro_logger.LOGGER_INIT_SUCCESS,
                                            sSuccess)

    return dConf

def write_config_file( sFilename, bFull_config=False ):
    plwriter = plist_writer.Plist_writer()
    try:
        plwriter.write(sFilename,dConfig,True,bFull_config)
    except IOError, sError:
        sError =  _("Unable to write to file '%s', the following\n") %(sFilename)+ \
                  _("error occured: %s") % (sError)
        metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                        sError)
        sys.exit(2)

def overlay_config( dBase, dNew, iConfig_level ):

    for sKey in dNew.keys():
        if sKey in dBase:
            dBase[sKey]['VALUE'] = dNew[sKey]
            dBase[sKey]['FROM']  = iConfig_level
        else:
            sMessage = _("Additionnal configuration value added to METRo config.")
            sComments = "'%s', %s" % (sKey,sMessage)
            dBase[sKey] = {'VALUE'   :dNew[sKey],
                           'FROM'    :iConfig_level,
                           'COMMENTS':sMessage}

            sWarning = _("%s\nkey='%s'\nvalue='%s'") % (sMessage, sKey, dNew[sKey])
            metro_logger.print_init_message(metro_logger.LOGGER_INIT_SUCCESS,
                                            sWarning)



def init( ):
    # activation du module de traduction
#    gettext.textdomain(METRO_CONFIG_GETTEXT_PACKAGE)#, METRO_CONFIG_GETTEXT_LOCALEDIR)
#    print "dans init", gettext.textdomain()
    
    set_default_value()

def get_cmdline_conf( lArgs ):
    return save_command_line_parameter(lArgs[1:],
                                       CFG_SHORT_OPTIONS,
                                       CFG_LONG_OPTIONS)

def process_command_line_parameter( dCmdline ):

    # si l'usager desire generer un fichier de configuration
    if 'FILE_GENERATE_CONFIGMETRO' in dCmdline:
        sConfig_filename = dCmdline['FILE_GENERATE_CONFIGMETRO']

        sGenerate = _("Generating METRo configuration file...")
        metro_logger.print_init_message(metro_logger.LOGGER_INIT_MESSAGE,
                                        sGenerate)
        write_config_file(sConfig_filename)
        sSuccess = _("METRo configuration file '%s' created with \n") \
                     % (sConfig_filename) + \
                   _("success. To launch METRo with that configuration\n") + \
                   _("file you need to use the --config option\n") 

        metro_logger.print_init_message(metro_logger.LOGGER_INIT_SUCCESS,
                                        sSuccess)
        sys.exit(0)

def read_configuration_file( dCmdline_conf ):
    if 'FILE_CONFIGMETRO_FILENAME' in dCmdline_conf:
        sConfig_filename = dCmdline_conf['FILE_CONFIGMETRO_FILENAME']
        sMessage = _("Reading and validating configuration file...")
        metro_logger.print_init_message(metro_logger.LOGGER_INIT_MESSAGE,
                                        sMessage)
        dConfigfile = read_config_file(sConfig_filename)

    else:
        dConfigfile = None

    return dConfigfile

def overlay_configuration( dFile_conf, dCmdline_conf ):
    if dFile_conf != None:
        overlay_config(dConfig, dFile_conf, CFG_CONFIGFILE)

    if dCmdline_conf != None:
        # Ajoute les valeurs passe sur la ligne de commande
        overlay_config(dConfig, dCmdline_conf, CFG_COMMANDLINE)

def validating_configuration( ):
    # validation de toute les options
    sMessage = _("Validating configuration")
    metro_logger.print_init_message(metro_logger.LOGGER_INIT_MESSAGE,
                                    sMessage)
    
#    metro_config_validation.validate_config(dConfig)

    metro_logger.print_init_message(metro_logger.LOGGER_INIT_SUCCESS,
                                    _("METRo configuration validated"))

#-------------------------------------------------------------------------------
#
# Nom:          get_value
#
# Parametres:   I sKey : cle de l'information a extraire du dictionnaire
#                        de configuration
#
# Retourne:     valeur de la cle extraite, None si echec
#
# Descriptions: recupere l'information identifie par une cle(sKey)
#
#-------------------------------------------------------------------------------
def get_value( sKey ):

    return dConfig[sKey]['VALUE']

def set_value( sKey, value ):
    dConfig[sKey]['VALUE'] = value


def get_comment( sKey ):
    return dConfig[sKey]['COMMENTS']

def key_exist( sKey ):
    return sKey in dConfig




#-------------------------------------------------------------------------------
#
# Nom:          get_metro_version
#
# Parametres:   aucun
#
# Retourne:     string: le numero de version
#
# Descriptions: recupere le niveau de version de metro
#
#-------------------------------------------------------------------------------
def get_metro_version( ):
    return CFG_METRO_VERSION + " (" + CFG_METRO_DATE + ")" 

def save_command_line_parameter( lArgv, sShort_opt, lLong_opt ):

    dConf = {}

    try:
        opts, args = getopt.gnu_getopt( lArgv, sShort_opt, lLong_opt)

    except getopt.GetoptError, sError:
        # print help information and exit:
        sMessage = _("bad arg: ") + str(sError)
        print sMessage
        sys.exit(2)

    if args != []:
        sMessage = _("problem with arg: ") + str(args) +\
                   _("\nString(s) was not recognize as an argument.")
        print sMessage
        sys.exit(3)
    
    output = None
    verbose = False
    for o, a in opts:
        if o in ("-v", "--verbose-level"):
            if a == "0":
                dConf['INIT_LOGGER_VERBOSE_LEVEL'] = \
                    metro_logger.LOGGER_VERBOSE_LEVEL_NOLOG
            elif a == "1":
                dConf['INIT_LOGGER_VERBOSE_LEVEL'] = \
                    metro_logger.LOGGER_VERBOSE_LEVEL_MINIMAL
            elif a == "2":
                dConf['INIT_LOGGER_VERBOSE_LEVEL'] = \
                    metro_logger.LOGGER_VERBOSE_LEVEL_NORMAL
            elif a == "3":
                dConf['INIT_LOGGER_VERBOSE_LEVEL'] = \
                    metro_logger.LOGGER_VERBOSE_LEVEL_FULL
            elif a == "4":
                dConf['INIT_LOGGER_VERBOSE_LEVEL'] = \
                    metro_logger.LOGGER_VERBOSE_LEVEL_DEBUG
            else:
                sError = _("%s is not a valid value for %s. Please use ") %\
                         (a,o)+ \
                         _("one of the\nfollowing: 0, 1 , 2, 3, 4. High ") + \
                         _("value mean higher verbosity")
                metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                                sError)
                sys.exit(3)
               
        if o in ("-l", "--log-file"):
            dConf['FILE_LOGGER_FILENAME'] = a

        if o in ("-c", "--config"):
            dConf['FILE_CONFIGMETRO_FILENAME'] = a

        if o == "--generate-config":
            dConf['FILE_GENERATE_CONFIGMETRO'] = a

        if o == "--generate-dtd-catalog":
            metro_xml_dtd.generate_dtd_catalog()
            sys.exit(0)

        if o == "--bypass-core":
            dConf['T_BYPASS_CORE'] = True

        if o in ("-h", "--help"):
            sMetro_root_path = metro_util.get_metro_root_path()
            sMetro_man_path = sMetro_root_path + "/doc/metro.1"
            print _("see man page: 'man %s'") % sMetro_man_path
            sys.exit()

        if o == "--version":
            print "METRo version: ",get_metro_version()
            sys.exit(0)

        if o in ("-o", "--output"):
            print "output"
            output = a

        if o == "--roadcast-start-date":
            dConf['INIT_ROADCAST_START_DATE'] = a

        if o == "--input-forecast":
            dConf['FILE_FORECAST_IN_FILENAME'] = a

        if o == "--input-observation":
            dConf['FILE_OBSERVATION_FILENAME'] = a

        if o == "--input-observation-ref":
            dConf['FILE_OBSERVATION_REF_FILENAME'] = a

        if o == "--input-station":
            dConf['FILE_STATION_FILENAME'] = a

        if o == "--output-forecast":
            dConf['FILE_FORECAST_OUT_FILENAME'] = a
        
        if o == "--output-roadcast":
            dConf['FILE_ROADCAST_FILENAME'] = a
    

#        if o == "--xml-lib":
#            if a == "pyxml":
#                a = "metro_xml_pyxml"
#            if a == "libxml2":
#                a = "metro_xml_libxml2"
#
#            dConf['INIT_XML_LIB'] = a


        if o == "--selftest":
            dConf['INIT_ROADCAST_START_DATE'] = "2004-01-30T20:00Z"

            dConf['FILE_FORECAST_IN_FILENAME'] = \
                metro_util.get_metro_root_path() +\
                "/data/forecast/forecast_selftest.xml"
            dConf['FILE_OBSERVATION_FILENAME'] = \
                metro_util.get_metro_root_path() +\
                "/data/observation/observation_selftest.xml" 
            dConf['FILE_STATION_FILENAME'] = \
                 metro_util.get_metro_root_path() +\
                "/data/station/station_selftest.xml" 
            dConf['FILE_ROADCAST_FILENAME'] = \
                 metro_util.get_metro_root_path() +\
                "/data/roadcast/roadcast_selftest.xml"
            dConf['INIT_LOGGER_VERBOSE_LEVEL'] = \
                metro_logger.LOGGER_VERBOSE_LEVEL_DEBUG

        if o == "--silent":
            dConf['INIT_LOGGER_SHELL_DISPLAY']= False



            
    return dConf



#-------------------------------------------------------------------------------
#
# Nom:          set_default_value
#
# Parametres:   NA
#
# Retourne:     NA
#
# Descriptions: initilisation de la configuration avec des valeurs "hardcoder"
#
#-------------------------------------------------------------------------------
def set_default_value( ):

#===============================================================================
# Filename and version
#===============================================================================

# ------------------------------------ input -----------------------------------

    # FORECAST 
    dConfig['FILE_FORECAST_IN_FILENAME'] = \
        {'VALUE'   :"",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("forecast filename")}

    dConfig['FILE_FORECAST_IN_CURRENT_VERSION'] = \
        {'VALUE'   :"1.1",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("current version for forecast file")}    

    dConfig['FILE_FORECAST_IN_MIN_VERSION'] = \
        {'VALUE'   :"1.1",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("min version for valid forecast file")}

    dConfig['FILE_FORECAST_IN_MAX_VERSION'] = \
        {'VALUE'   :"1.1",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("max version for valid forecast file")}    

    # OBSERVATION
    dConfig['FILE_OBSERVATION_FILENAME'] = \
        {'VALUE'   :"",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("observation filename")}

    dConfig['FILE_OBSERVATION_CURRENT_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("current version for observation file")}

    dConfig['FILE_OBSERVATION_MIN_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("min version for valid observation file")}

    dConfig['FILE_OBSERVATION_MAX_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("max version for valid observation file")}  

    # STATION
    dConfig['FILE_STATION_FILENAME'] = \
        {'VALUE'   :"",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("station configuration filename")}

    dConfig['FILE_STATION_CURRENT_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("current version for station file")}

    dConfig['FILE_STATION_MIN_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("min version for valid station file")}

    dConfig['FILE_STATION_MAX_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("max version for valid station file")}  

# ----------------------------------- output -----------------------------------

    dConfig['FILE_FORECAST_OUT_FILENAME'] = \
        {'VALUE'   :"forecast_out.xml",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("forecast output filename")}

    dConfig['FILE_FORECAST_OUT_CURRENT_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("current version for forecast output file")}

    dConfig['FILE_ROADCAST_FILENAME'] = \
        {'VALUE'   :"roadcast.xml",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("roadcast filename")}

    dConfig['FILE_ROADCAST_CURRENT_VERSION'] = \
        {'VALUE'   :"1.4",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("current version for roadcast file")}

# ------------------------------------ misc ------------------------------------

    dConfig['FILE_CONFIGMETRO_FILENAME'] = \
        {'VALUE'   :"metro_config.xml",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("METRo configuration filename")}

    dConfig['FILE_CONFIGMETRO_CURRENT_VERSION'] = \
        {'VALUE'   :"1.1",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("current version for METRo configuration file")}

    dConfig['FILE_LOGGER_FILENAME'] = \
        {'VALUE'   :"log_metro",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("logger filename")}

    dConfig['FILE_LOGGER_CURRENT_VERSION'] = \
        {'VALUE'   :"1.0",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("logger filename")}



#===============================================================================
# XML file definition
#===============================================================================

# ---------------------------------- Metro data type ---------------------------


    dConfig['XML_DATATYPE_STANDARD'] = \
        {'VALUE':{'INTEGER': \
                      {'READ'  :"toolbox.metro_dom2metro_handler.read_integer",
                       'WRITE' :"toolbox.metro_metro2dom_handler.write_integer"},

                  'REAL': \
                      {'READ' :"toolbox.metro_dom2metro_handler.read_real",
                       'WRITE':"toolbox.metro_metro2dom_handler.write_real"},

                  'DATE': \
                      {'READ' :"metro_dom2metro_handler.read_date",
                       'WRITE':"metro_metro2dom_handler.write_date"},

                  'STRING': \
                      {'READ' :"metro_dom2metro_handler.read_string",
                       'WRITE':"metro_metro2dom_handler.write_string"},

                  'COORDINATE': \
                      {'READ' :"metro_dom2metro_handler.read_coordinate",
                       'WRITE':"",
                       'CHILD':[{'NAME':"LATITUDE",
                                 'XML_TAG':"latitude",
                                 'DATA_TYPE':'REAL'},

                                {'NAME':"LONGITUDE",
                                 'XML_TAG':"longitude",
                                 'DATA_TYPE':'REAL'},
                                ]},

                  'ROADLAYER_TYPE': \
                      {'READ'	:"metro_dom2metro_handler.read_roadlayer_type",
                       'WRITE':""},

                  'DEFAULT': \
                      {'READ' :"metro_dom2metro_handler.read_string",
                       'WRITE':"metro_metro2dom_handler.write_string"}
                  },
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("standard data type for METRo XML files")}

    dConfig['XML_DATATYPE_EXTENDED'] = \
        {'VALUE'   :{},
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("extended data type for METRo XML files")}


    # -------------------------------- Forecast --------------------------------
    
    dConfig['XML_FORECAST_XPATH_ROOT'] = \
        {'VALUE'   :"forecast",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("xpath path for forecast root")}

    dConfig['XML_FORECAST_XPATH_HEADER'] = \
        {'VALUE'   :"/forecast/header",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("xpath path for forecast header")}

    dConfig['XML_FORECAST_XPATH_PREDICTION'] = \
        {'VALUE'   :"/forecast/prediction-list/prediction",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("xpath path for forecast prediction")}


    dConfig['XML_FORECAST_HEADER_STANDARD_ITEMS'] = \
        {'VALUE'    :[{'NAME':"VERSION",
                       'XML_TAG':"version",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"STATION_ID",
                       'XML_TAG':"station-id",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"PRODUCTION_DATE",
                       'XML_TAG':"production-date",
                       'DATA_TYPE':"DATE"},

                      {'NAME':"FILETYPE",
                       'XML_TAG':"filetype",
                       'DATA_TYPE':"STRING"},

                      
                      ],

         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard forecast header items")}


    dConfig['XML_FORECAST_HEADER_EXTENDED_ITEMS'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended forecast header items")}


    dConfig['XML_FORECAST_PREDICTION_STANDARD_ITEMS'] = \
        {'VALUE' :[{'NAME':"FORECAST_TIME",
                    'XML_TAG':"forecast-time",
                    'DATA_TYPE':"DATE"},

                   {'NAME':"WS",
                    'XML_TAG':"ws",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"AP",
                    'XML_TAG':"ap",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"AT",
                    'XML_TAG':"at",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"TD",
                    'XML_TAG':"td",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"CC",
                    'XML_TAG':"cc",
                    'DATA_TYPE':"INTEGER"},

                   {'NAME':"SN",
                    'XML_TAG':"sn",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"RA",
                    'XML_TAG':"ra",
                    'DATA_TYPE':"REAL"},
                   ],
         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard forecast prediction items")}


    dConfig['XML_FORECAST_PREDICTION_EXTENDED_ITEMS'] = \
        {'VALUE'   :[ ],
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("extended forecast prediction items.")}


    # --------------------------------- Observation ----------------------------
    
    dConfig['XML_OBSERVATION_XPATH_ROOT'] = \
        {'VALUE'     :"observation",
         'FROM'      :CFG_INTERNAL,
         'COMMENTS'  :_("xpath path for observation root")}

    dConfig['XML_OBSERVATION_XPATH_HEADER'] = \
        {'VALUE'     :"/observation/header",
         'FROM'      :CFG_INTERNAL,
         'COMMENTS'  :_("xpath path for observation header")}

    dConfig['XML_OBSERVATION_XPATH_MEASURE'] = \
        {'VALUE'   :"/observation/measure-list/measure",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("xpath path for observation measure data")}
    
    dConfig['XML_OBSERVATION_HEADER_STANDARD_ITEMS'] = \
        {'VALUE'    :[{'NAME':"VERSION",
                       'XML_TAG':"version",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"ROAD_STATION",
                       'XML_TAG':"road-station",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"FILETYPE",
                       'XML_TAG':"filetype",
                       'DATA_TYPE':"STRING"},

                      ],

         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard observation header items")}


    dConfig['XML_OBSERVATION_HEADER_EXTENDED_ITEMS'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended observation header items")}


    dConfig['XML_OBSERVATION_MEASURE_STANDARD_ITEMS'] = \
        {'VALUE' :[{'NAME':"OBSERVATION_TIME",
                    'XML_TAG':"observation-time",
                    'DATA_TYPE':"DATE"},

                   {'NAME':"AT",
                    'XML_TAG':"at",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"TD",
                    'XML_TAG':"td",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"PI",
                    'XML_TAG':"pi",
                    'DATA_TYPE':"INTEGER"},

                   {'NAME':"WS",
                    'XML_TAG':"ws",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"SC",
                    'XML_TAG':"sc",
                    'DATA_TYPE':"INTEGER"},
                   
                   {'NAME':"ST",
                    'XML_TAG':"st",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"SST",
                    'XML_TAG':"sst",
                    'DATA_TYPE':"REAL"},
                   ],

         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard observation header items")}


    dConfig['XML_OBSERVATION_MEASURE_EXTENDED_ITEMS'] = \
        {'VALUE'   :[],
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("extended observation measure list.")}

    
    # ----------------------------------- Station ------------------------------
    

    dConfig['XML_STATION_XPATH_ROOT'] = \
        {'VALUE'     :"station",
         'FROM'      :CFG_INTERNAL,
         'COMMENTS'  :_("xpath path for station root")}

    dConfig['XML_STATION_XPATH_HEADER'] = \
        {'VALUE'     :"/station/header",
         'FROM'      :CFG_INTERNAL,
         'COMMENTS'  :_("xpath path for station header")}

    dConfig['XML_STATION_XPATH_ROADLAYER'] = \
        {'VALUE'   :"/station/roadlayer-list/roadlayer",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("xpath path for station road layer")}

    dConfig['XML_STATION_HEADER_STANDARD_ITEMS'] = \
        {'VALUE'    :[{'NAME':"VERSION",
                       'XML_TAG':"version",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"ROAD_STATION",
                       'XML_TAG':"road-station",
                       'DATA_TYPE':"STRING"},
                      
                      {'NAME':"TIME_ZONE",
                       'XML_TAG':"time-zone",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"PRODUCTION_DATE",
                       'XML_TAG':"production-date",
                       'DATA_TYPE':"DATE"},

                      {'NAME':"COORDINATE",
                       'XML_TAG':"coordinate",
                       'DATA_TYPE':"COORDINATE"},

                      {'NAME':"STATION_TYPE",
                       'XML_TAG':"station-type",
                       'DATA_TYPE':"STRING"},
                      ],

         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard station header items")}


    dConfig['XML_STATION_HEADER_EXTENDED_ITEMS'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended station header items")}

    dConfig['XML_STATION_ROADLAYER_STANDARD_ITEMS'] = \
        {'VALUE' :[{'NAME':"POSITION",
                    'XML_TAG':"position",
                    'DATA_TYPE':'INTEGER'},

                   {'NAME':"TYPE",
                    'XML_TAG':"type",
                    'DATA_TYPE':'ROADLAYER_TYPE'},

                   {'NAME':"THICKNESS",
                    'XML_TAG':"thickness",
                    'DATA_TYPE':'REAL'},
                   ],
         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard road layer items")}

    dConfig['XML_STATION_ROADLAYER_EXTENDED_ITEMS'] = \
        {'VALUE' :[],
         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("extended road layer items")}


    dConfig['XML_STATION_ROADLAYER_VALID_TYPE'] = \
        {'VALUE':{'ASPHALT':1,'ASPHALTE':1,
                  'CRUSHED ROCK':2,'GRAVIER':2,
                  'CEMENT':3,'BETON':3,
                  },
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("valid station layer type")}
    
    
    # -----------------------------------  Roadcast ----------------------------
    

    dConfig['XML_ROADCAST_XPATH_ROOT'] = \
        {'VALUE'     :"roadcast",
         'FROM'      :CFG_INTERNAL,
         'COMMENTS'  :_("xpath path for roadcast root")}

    dConfig['XML_ROADCAST_XPATH_HEADER'] = \
        {'VALUE'     :"/roadcast/header",
         'FROM'      :CFG_INTERNAL,
         'COMMENTS'  :_("xpath path for roadcast header")}

    dConfig['XML_ROADCAST_XPATH_PREDICTION'] = \
        {'VALUE'   :"/roadcast/prediction-list/prediction",
         'FROM'    :CFG_INTERNAL,
         'COMMENTS':_("xpath path for roadcast measure data")}

    dConfig['XML_ROADCAST_HEADER_STANDARD_ITEMS'] = \
        {'VALUE'    :[{'NAME':"VERSION",
                       'XML_TAG':"version",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"PRODUCTION_DATE",
                       'XML_TAG':"production-date",
                       'DATA_TYPE':"DATE"},

                      {'NAME':"ROAD_STATION",
                       'XML_TAG':"road-station",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"LATITUDE",
                       'XML_TAG':"latitude",
                       'DATA_TYPE':"REAL"},

                      {'NAME':"LONGITUDE",
                       'XML_TAG':"longitude",
                       'DATA_TYPE':"REAL"},

                      {'NAME':"FILETYPE",
                       'XML_TAG':"filetype",
                       'DATA_TYPE':"STRING"},

                      {'NAME':"FIRST_ROADCAST",
                       'XML_TAG':"first-roadcast",
                       'DATA_TYPE':"STRING"},

                      ],

         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard roadcast header items")}


    dConfig['XML_ROADCAST_HEADER_EXTENDED_ITEMS']  = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended roadcast header items")}

    dConfig['XML_ROADCAST_PREDICTION_STANDARD_ITEMS'] = \
        {'VALUE' :[{'NAME':"ROADCAST_TIME",
                    'XML_TAG':"roadcast-time",
                    'DATA_TYPE':"DATE"},
                   
                   {'NAME':"HH",
                    'XML_TAG':"hh",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},
                   
                   {'NAME':"AT",
                    'XML_TAG':"at",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},

                   {'NAME':"TD",
                    'XML_TAG':"td",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},

                   {'NAME':"WS",
                    'XML_TAG':"ws",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},

                   {'NAME':"SN",
                    'XML_TAG':"sn",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},

                   {'NAME':"RA",
                    'XML_TAG':"ra",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},

                   {'NAME':"QP-SN",
                    'XML_TAG':"qp-sn",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"QP-RA",
                    'XML_TAG':"qp-ra",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"CC",
                    'XML_TAG':"cc",
                    'DATA_TYPE':"INTEGER"},

                   {'NAME':"SF",
                    'XML_TAG':"sf",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"IR",
                    'XML_TAG':"ir",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"FV",
                    'XML_TAG':"fv",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"FC",
                    'XML_TAG':"fc",
                    'DATA_TYPE':"REAL"},
                   
                   {'NAME':"FA",
                    'XML_TAG':"fa",
                    'DATA_TYPE':"REAL"},
                   
                   {'NAME':"FG",
                    'XML_TAG':"fg",
                    'DATA_TYPE':"REAL"},
                   
                   {'NAME':"BB",
                    'XML_TAG':"bb",
                    'DATA_TYPE':"REAL"},
                   
                   {'NAME':"FP",
                    'XML_TAG':"fp",
                    'DATA_TYPE':"REAL"},

                   {'NAME':"RC",
                    'XML_TAG':"rc",
                    'DATA_TYPE':"INTEGER"},

                   {'NAME':"ST",
                    'XML_TAG':"st",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},

                   {'NAME':"SST",
                    'XML_TAG':"sst",
                    'DATA_TYPE':"REAL",
                    'PRECISION':2},
                   ],

         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("standard roadcast prediction items")}


    dConfig['XML_ROADCAST_PREDICTION_EXTENDED_ITEMS'] = \
        {'VALUE'   :[ ],
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("extended roadcast prediction list.")}



#===============================================================================
# Metro data definition
#===============================================================================

# ---------------------------------- forecast ----------------------------------
    dConfig['DATA_ATTRIBUTE_FORECAST_STANDARD'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("default forecast attribute.")}

    dConfig['DATA_ATTRIBUTE_FORECAST_EXTENDED'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended forecast attribute.")}

# --------------------------------- observation --------------------------------

    dConfig['DATA_ATTRIBUTE_OBSERVATION_STANDARD'] = \
        {'VALUE'    :["SST_VALID",                      
                      "AT_VALID",
                      "TD_VALID",
                      "WS_VALID",
                      "SST_VALID_INTERPOLATED",                      
                      "AT_VALID_INTERPOLATED",
                      "TD_VALID_INTERPOLATED",
                      "WS_VALID_INTERPOLATED",
                      "DELTA_T",
                      "NO_OBS"
                      ],
         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("default observation attribute.")}

    dConfig['DATA_ATTRIBUTE_OBSERVATION_EXTENDED'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended observation attribute.")}

# --------------------------------- roadcast --------------------------------

    dConfig['DATA_ATTRIBUTE_ROADCAST_STANDARD'] = \
        {'VALUE'    :["OBSERVATION_LENGTH",
                      "OBSERVATION_DELTAT_T",
                      "FORECAST_NB_TIMESTEPS"
                      ],
         'FROM'     :CFG_INTERNAL,
         'COMMENTS' :_("default observation attribute.")}

    dConfig['DATA_ATTRIBUTE_ROADCAST_EXTENDED'] = \
        {'VALUE'    :[],
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("extended observation attribute.")}

#===============================================================================
# Metro initialisation variable
#===============================================================================

    dConfig['INIT_LOGGER_VERBOSE_LEVEL'] = \
        {'VALUE'   :metro_logger.LOGGER_VERBOSE_LEVEL_FULL,
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("logger verbosity level")}

    dConfig['INIT_LOGGER_SHELL_DISPLAY'] = \
        {'VALUE'   :True,
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("logger shell display")}

    dConfig['INIT_DEFAULT_TIME_ZONE'] = \
        {'VALUE'   :"UTC",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("default time zone if no TZ environment variable ") +
                    _("is defined")}

    dConfig['METRO_LANGUAGE'] = \
        {'VALUE'   :"en",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("default language is english")}

#    dConfig['INIT_XML_LIB'] = \
#        {'VALUE'   :"metro_xml_libxml2",
#         'FROM'    :CFG_HARDCODED,
#         'COMMENTS':_("xml implementation for METRo")}

    dConfig['INIT_MODULE_EXECUTION_SEQUENCE'] = \
        {'VALUE'   :["metro_read_forecast",
                     "metro_validate_forecast",
                     "metro_string2dom_forecast",
                     "metro_read_observation",
                     "metro_validate_observation",
                     "metro_string2dom_observation",
                     "metro_read_station",
                     "metro_validate_station",
                     "metro_string2dom_station",
#                     "metro_read_observation_ref",
#                     "metro_validate_observation_ref",
#                     "metro_string2dom_observation_ref",      
                     "metro_dom2metro",
                     "metro_preprocess_validate_input",
                     "metro_preprocess_qa_qc_forecast",
                     "metro_preprocess_interpol_forecast",
                     "metro_preprocess_fsint2",
                     "metro_preprocess_qa_qc_observation",
                     "metro_preprocess_validate_input",
                     "metro_preprocess_interpol_observation",
                     "metro_preprocess_combine",
                     "metro_model",
                     "metro_postprocess_subsample_roadcast",
                     "metro_postprocess_round_roadcast",
                     "metro_metro2dom",
                     "metro_write_roadcast",
                     "metro_write_forecast",
                     ],
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("METro module execution sequence")}

    dConfig['INIT_ROADCAST_START_DATE'] = \
        {'VALUE'   :"",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("roadcast start time")}

    dConfig['INIT_USER_ROADCAST_START_DATE'] = \
        {'VALUE'   :"",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("user roadcast start time")}

    dConfig['INIT_MODEL_ROADCAST_START_DATE'] = \
        {'VALUE'   :"",
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("model roadcast start time")}



#===============================================================================
# Default value
#===============================================================================

    # ---------------------------- forecast ------------------------------------
    
    dConfig['DEFAULT_FORECAST_HEADER_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for forecast header date")}

    dConfig['DEFAULT_FORECAST_PREDICTION_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for forecast prediction date")}


    # ---------------------------- observation ---------------------------------


    dConfig['DEFAULT_OBSERVATION_HEADER_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for observation header date")}

    dConfig['DEFAULT_OBSERVATION_MEASURE_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for observation measure date")}


    # ------------------------------ station -----------------------------------

    dConfig['DEFAULT_STATION_HEADER_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for station header date")}

    dConfig['DEFAULT_STATION_ROADLAYER_TYPE'] = \
        {'VALUE':'ASPHALT',
         'FROM' :CFG_HARDCODED,
         'COMMENTS':_("default station roadlayer type")}


    # ------------------------------ roadcast ----------------------------------


    dConfig['DEFAULT_ROADCAST_HEADER_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for roadcast header date")}

    dConfig['DEFAULT_ROADCAST_PREDICTION_TIMEZONE'] = \
        {'VALUE'    :'UTC',
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default time zone for roadcast prediction date")}

    dConfig['DEFAULT_ROADCAST_PREDICTION_PRECISION'] = \
        {'VALUE'    :2,
         'FROM'     :CFG_HARDCODED,
         'COMMENTS' :_("default precision for roadcast prediction value")}    

#===============================================================================
# ????????????????????
#===============================================================================


    dConfig['T_BYPASS_CORE'] = \
        {'VALUE'   :False,
         'FROM'    :CFG_HARDCODED,
         'COMMENTS':_("test")}


#    dConfig['FILE_OBSERVATION_REF_FILENAME'] = \
#        {'VALUE'   :None,
#         'FROM'    :CFG_HARDCODED,
#         'COMMENTS':_("observation filename")}

  
