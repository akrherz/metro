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

import sys
import string

import metro_config
import metro_logger
from toolbox import metro_util

_ = metro_util.init_translation('metro_xml')

def init( sMetro_xml_lib = ""):
    global metro_xml_lib
    if sMetro_xml_lib != "":
        try:
            metro_xml_lib = __import__(sMetro_xml_lib)
        except "MetroImportError":
            sMessage =  _("Fatal error! Can't import '%s' xml library") \
                       % (sMetro_xml_lib)
            metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                            sMessage)
        else:
            metro_logger.print_init_message( \
                    metro_logger.LOGGER_INIT_SUCCESS,
                    _("XML library '%s' will be use.") % (sMetro_xml_lib))
            metro_xml_lib = __import__(sMetro_xml_lib)
        
    else:
#        metro_logger.print_init_message(metro_logger.LOGGER_INIT_MESSAGE,
#                                        _("Auto configure METRo XML library"))
        try:
            metro_util.test_import("metro_xml_libxml2")
        except "MetroImportError":
            try:
                metro_util.test_import("metro_xml_pyxml")
            except "MetroImportError":
                sMessage = _("Fatal error! No METRo XML library can be use. ") +\
                           _("\nMETRo need one of the following XML library ") +\
                           _("installed on the system.\nSupported library:") +\
                           "python-libxml2, PyXML"
                metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                                sMessage)
                sys.exit(3)
            else:
                sMessage = _("metro_xml_pyxml will be used.\nWE STRONGLY ") +\
                           _("RECOMMAND THAT YOU USED libxml2, METRo") +\
                           _("WOULD BE 10 TIMES FASTER.")
                metro_logger.print_init_message( \
                    metro_logger.LOGGER_INIT_SUCCESS,
                    sMessage)
                metro_xml_pyxml = metro_util.import_name('toolbox',
                                                         "metro_xml_pyxml")
                metro_xml_lib = metro_xml_pyxml.Metro_xml_pyxml()
        else:
#            metro_logger.print_init_message( \
#                    metro_logger.LOGGER_INIT_SUCCESS,
#                    _("metro_xml_libxml2 will be used."))
            metro_xml_libxml2 = metro_util.import_name('toolbox',
                                                       "metro_xml_libxml2")
            metro_xml_lib = metro_xml_libxml2.Metro_xml_libxml2()
                
    metro_xml_lib.start()

def stop():
    sMessage = metro_xml_lib.stop()
    if sMessage:
        metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,sMessage)

def validate_string( sXml_content ):
    if metro_xml_lib.get_name() == "Metro_xml_libxml2":
        metro_xml_lib.validate_xml_string(sXml_content)
    else:
        sMessage = _("Validation is only supported with metro_xml_libxml2")
        if metro_logger.is_initialised():
            metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                                       sMessage)
        else:
            metro_logger.print_init_message(metro_logger.LOGGER_INIT_ERROR,
                                            sMessage)


def xpath( domDom, sXpath ):
    nodeLeaf = metro_xml_lib.xpath(metro_xml_lib.get_dom_root(domDom),sXpath)    
    if nodeLeaf:
        sRslt = metro_xml_lib.get_node_value(nodeLeaf[0]);
    else:
        sRslt = None
    return sRslt

def extract_xpath(lDefs, domDom, sXpath, bIs_matrix=False):

    items = metro_xml_lib.xpath(metro_xml_lib.get_dom_root(domDom),
                                sXpath)

    if bIs_matrix == True:
        if len(items) > 0:
            rslt = []
            for node_item in items:
                data = extract_data(lDefs,node_item)
                rslt.append(data)
        else:
            rslt = None
    else:
        if len(items) == 1:
            node_item = items[0]
            rslt = extract_data(lDefs,node_item)
        elif len(items) > 1:
            rslt = []
            for node_item in items:
                data = extract_data(lDefs,node_item)
                rslt.append(data)
        else:
            rslt = None

    return rslt

def extract_data(lDefs,nodeItems):

    lData = []

    # recupere les informations sur les types de donnees
    dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
    dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')

    dData_type = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)

    # pour chacune des definitions d'item
    for dDef in lDefs:
        # recupere le nom et le type de l'item
        sTag = dDef['XML_TAG']
        sData_type_name = dDef['DATA_TYPE']

        if sData_type_name not in dData_type.keys():
            metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                                       _("Invalid data_type: (%s) for the ") \
                                       % (sData_type_name) +\
                                       _("following tag:(%s). Default data ")\
                                       % (sTag) +\
                                       _("type will be used.")) 
            sData_type_name = 'DEFAULT'

        # extraction de l'information

        # le type de donne requiert l'appel d'une fonction
        # pour recueillir l'information
        sReadHandler = dData_type[sData_type_name]['READ']

        # construction du code necessaire pour importer le module dans lequelle
        # se trouve la fonction servant a extraire la donne
        lFunctionPart = string.split(sReadHandler,'.')
        sFunction_module = string.join(lFunctionPart[:-1],".")
        sCode = "import " +sFunction_module
        exec sCode

        if dData_type[sData_type_name].has_key('CHILD'):
            # construction de l'instruction effectuant l'appel de la fonction
            # qui va extraire la donne qui contient une liste de "sous-donne"

            nodeTmp = metro_xml_lib.xpath(nodeItems,
                                          sTag)
            lChildList = dData_type[sData_type_name]['CHILD']
            sCode = "data = " + sReadHandler + "(lChildList,nodeTmp)"
        else:
            # construction de l'instruction effectuant l'appel de la fonction
            # qui va extraire la donnee
            sCode = "data = " + sReadHandler + "(sTag,nodeItems)"

        exec sCode
        lData.append(data)

    # Si il y a juste 1 definition de donnees (lDefs) c'est que c'est un "array".
    # Alors dans ce cas il faut extraire le "array" de la liste
    # [[1,2,3,...,N]]  ==>  [1,2,3,...,N]
    if len(lDefs) == 1:
        if lData:
            lData = lData[0]

    return lData

def extract_data_from_node(sTag,nodeBranch):

    nodeLeaf = metro_xml_lib.xpath(nodeBranch,
                                   sTag + "/text()")

    if nodeLeaf:
        sRslt = metro_xml_lib.get_node_value(nodeLeaf[0]);
    else:
        sRslt = None

    return sRslt

def read_dom( sFilename ):
    return metro_xml_lib.read_dom(sFilename)

def dom_from_string( sXml ):
    return metro_xml_lib.dom_from_string(sXml)

def free_dom( domDoc ):
    metro_xml_lib.free_dom(domDoc)

def create_dom( sDom_root_name ):
    return metro_xml_lib.create_dom(sDom_root_name)

def get_dom_root( domDoc ):
    return metro_xml_lib.get_dom_root(domDoc)

def mkdir_xpath( domDoc, nodeBranch, sXpath ):

    nodeParent = nodeBranch
    lNode_name = string.split(sXpath,'/')

    for sNode_name in lNode_name:
        nodeChild = metro_xml_lib.create_node(domDoc, sNode_name)
        metro_xml_lib.append_child(nodeParent,  nodeChild)
        nodeParent = nodeChild

    return nodeParent



def cd_xpath( nodeBranch, sXpath ):
    lChild = metro_xml_lib.xpath(nodeBranch, sXpath)
    if lChild != []:
        nodeChild = lChild[0]
    else:
        nodeChild = None
    return nodeChild

def create_node( domDoc, sNode_name):
    return metro_xml_lib.create_node(domDoc, sNode_name)

def create_text_node( domDoc, sNode_name, sNode_value ):
    return metro_xml_lib.create_text_node( domDoc, sNode_name, sNode_value )

def append_child( nodeParent, nodeChild ):
    metro_xml_lib.append_child(nodeParent, nodeChild)

def write_to_file( domDoc, sFilename ):
    metro_xml_lib.write_xml_file(domDoc, sFilename)


def create_node_tree_from_dict( domDoc, nodeParent, lDefs, dData ):

    # recupere les informations sur les types de donnees
    dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
    dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')

    dData_type = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)

    for dDef in lDefs:
        # recupere le nom et le type de l'item
        sTag = dDef['NAME']
        sXml_tag = dDef['XML_TAG']
        sData_type_name = dDef['DATA_TYPE']

        if sData_type_name not in dData_type.keys():
            sMessage = _("Invalid data_type: (%s) for the following tag:(%s).") \
                       % (sData_type_name, sTag)+\
                       _(" Default data type will be used.")
            metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                                       sMessage)
            sData_type_name = 'DEFAULT'


        # creation de la node

        # le type de donne requiert l'appel d'une fonction pour creer la node
        sWriteHandler = dData_type[sData_type_name]['WRITE']

        # construction du code necessaire pour importer le module dans lequelle
        # se trouve la fonction servant a creer la node
        lFunctionPart = string.split(sWriteHandler,'.')
        sFunction_module = string.join(lFunctionPart[:-1],".")
        sCode = "import " +sFunction_module
        exec sCode

        if dData_type[sData_type_name].has_key('CHILD'):
            # construction de l'instruction effectuant l'appel de la fonction
            # qui va creer la node qui contient une liste de "sous-node"

            lChildList = dData_type[sData_type_name]['CHILD']
            sCode = "nodeData = " + sWriteHandler + \
                    "(domDoc,sXml_tag,lChildList,dData[sTag])"
        else:
            # construction de l'instruction effectuant l'appel de la fonction
            # qui va creer la node
            sCode = "nodeData = " + sWriteHandler + \
                    "(domDoc,sXml_tag,dData[sTag])"
        exec sCode

        append_child(nodeParent, nodeData)


# sPrediction_xpath : Chacune des predictions sera contenue dans une node
#                     qui portera le nom indique par sPrediction_xpath.
def create_node_tree_from_matrix( domDoc, nodeParent, sPrediction_xpath,
                                  lDefs, metro_data_object, naMatrix ):

    # recupere les informations sur les types de donnees
    dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
    dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')
    dData_type = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)

    for naData in naMatrix:

        # si necessaire, creation d'une node pour contenir la prediction
        if sPrediction_xpath != None and sPrediction_xpath != "":
            nodePrediction = mkdir_xpath(domDoc, nodeParent, sPrediction_xpath)
        else:
            nodePrediction = nodeParent

        for dDef in lDefs:
            # recupere le nom et le type de l'item
            sTag = dDef['NAME']
            sXml_tag = dDef['XML_TAG']
            sData_type_name = dDef['DATA_TYPE']

            if sData_type_name not in dData_type.keys():
                sMessage = _("Invalid data_type: (%s) for the following ") \
                           % (sData_type_name) +\
                           _("tag:(%s). Default data type will be used.") \
                           % (sTag)
                metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                                           sMessage)
                sData_type_name = 'DEFAULT'

            # creation de la node

            # le type de donne requiert l'appel
            # d'une fonction pour creer la node
            sWriteHandler = dData_type[sData_type_name]['WRITE']

            # construction du code necessaire pour importer le module
            # dans lequelle se trouve la fonction servant a creer la node
            lFunctionPart = string.split(sWriteHandler,'.')
            sFunction_module = string.join(lFunctionPart[:-1],".")
            sCode = "import " +sFunction_module
            exec sCode

            # extraction de la donner du numarray
            val = naData[metro_data_object.index_of_matrix_col(sTag)]
            
            if dData_type[sData_type_name].has_key('CHILD'):
                # construction de l'instruction effectuant l'appel de
                # la fonction qui va creer la node qui contient une
                # liste de "sous-node"

                lChildList = dData_type[sData_type_name]['CHILD']
                sCode = "nodeData = " + sWriteHandler + \
                        "(domDoc,sXml_tag,lChildList,val)"
            else:
                # construction de l'instruction effectuant l'appel de la
                # fonction qui va creer la node
                sCode = "nodeData = " + sWriteHandler + "(domDoc,sXml_tag,val)"

            exec sCode

            append_child(nodePrediction, nodeData)
