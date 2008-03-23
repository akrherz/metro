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
Name       : metro_xml.py
Description: Methods to create and manipulate xml DOM
Work on    : 
Notes      :   
Author     : Francois Fortin
Date       : 2004
"""

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
            metro_logger.print_init_message( \
                    metro_logger.LOGGER_INIT_SUCCESS,
                    _("metro_xml_libxml2 will be used."))
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
    """
    Name: extract_xpath

    Arguments: [I] lDefs: list of dictionnaries containing the data definitions
                            as defined in metro_config.py
               [I] domDOM: DOM in which data has to be extracted.
               [I] sXPath: Path of the node containing all the subnodes
                            (header, prediction, etc.)
               [I] bIs_matrix: Boolean indicating if there is more than one
                               value to fetch

    Output:  llExtractedValue : A list of lists containing the expected values.

    Description: Return one or all the value contained under the xpath in the DOM.
    """
    
    node_items = metro_xml_lib.xpath(metro_xml_lib.get_dom_root(domDom),
                                sXpath)

    if bIs_matrix == True:
        if len(node_items) > 0:
            llExtractedValue = []
            for node_item in node_items:
                lData = extract_data(lDefs, node_item)
                llExtractedValue.append(lData)
        else:
            llExtractedValue = None
    else:
        if len(node_items) == 1:
            node_item = node_items[0]
            llExtractedValue = extract_data(lDefs, node_item)
        elif len(node_items) > 1:
            llExtractedValue = []
            for node_item in node_items:
                lData = extract_data(lDefs, node_item)
                llExtractedValue.append(lData)
        else:
            llExtractedValue = None


    return llExtractedValue

def extract_data(lDefs, nodeItems):
    """
    Name: extract_data

    Arguments:  [I] lDefs: list of dictionary containing the
                           definition of XML element in metro_config.py
                [I] nodeItems: fecth the elements in theses nodes

    Output:   lData :: list of values, one per definition in lDefs.

    Description: Extract the data from one node containing more nodes.
    """


    lData = []

    # Retrieve the informations about the data type
    dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
    dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')

    dData_type = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)

    # For each definitions of item
    for dDef in lDefs:
        # Get the name and the type of the item
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

        # Information extraction 

        # The given type needs the function call
        #  to retrieve the data
        sReadHandler = dData_type[sData_type_name]['READ']

        # Creation of code necessary to import the module into which
        #  we can find the function needed to extract the data
        lFunctionPart = string.split(sReadHandler,'.')
        sFunction_module = string.join(lFunctionPart[:-1],".")
        sCode = "import " +sFunction_module
        exec sCode

        if dData_type[sData_type_name].has_key('CHILD'):
            # Construction of instruction doing the function call that will
            #  extract the data containing a list of "sub-data"
            nodeTmp = metro_xml_lib.xpath(nodeItems,
                                          sTag)
            lChildList = dData_type[sData_type_name]['CHILD']
            sCode = "data = " + sReadHandler + "(lChildList,nodeTmp)"
        else:
            # Construction of instruction doing the function call that will
            #  extract the data 
            sCode = "data = " + sReadHandler + "(sTag,nodeItems)"

        exec sCode
        lData.append(data)

    # If there is only one definition of data (lDefs), it is an "array".
    #  In this case, we must extract the "array" from the list
    #  [[1,2,3,...,N]]  ==>  [1,2,3,...,N]
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

    # Retrieve the informations on the data types
    dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
    dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')

    dData_type = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)

    for dDef in lDefs:
        # Retrieve the name and the item type
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


        # Node creation 

        # The data type needs the call of a function to create the node
        sWriteHandler = dData_type[sData_type_name]['WRITE']

        # Creation of code needed to import the module in which we can find
        #  the function used to create the node.
        lFunctionPart = string.split(sWriteHandler,'.')
        sFunction_module = string.join(lFunctionPart[:-1],".")
        sCode = "import " +sFunction_module
        exec sCode

        if dData_type[sData_type_name].has_key('CHILD'):
            # Construction of instruction doing the function call that will
            #  create the node containing a list of "sub-node"
            lChildList = dData_type[sData_type_name]['CHILD']
            sCode = "nodeData = " + sWriteHandler + \
                    "(domDoc,sXml_tag,lChildList,dData[sTag])"
        else:
            # Construction of instruction doing the function call that will
            #  create the node 
            sCode = "nodeData = " + sWriteHandler + \
                    "(domDoc,sXml_tag,dData[sTag])"
        exec sCode

        append_child(nodeParent, nodeData)


def create_node_tree_from_matrix( domDoc, nodeParent, sPrediction_xpath,
                                  lDefs, metro_data_object, npMatrix ):
    """
    Each prediction will be contained in a node that will have the name
    given by sPrediction_xpath.
    """


    # Retrieve the informations about the data types
    dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
    dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')
    dData_type = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)

    for npData in npMatrix:

        # If needed, creation of a node to contain the prediction
        if sPrediction_xpath != None and sPrediction_xpath != "":
            nodePrediction = mkdir_xpath(domDoc, nodeParent, sPrediction_xpath)
        else:
            nodePrediction = nodeParent

        for dDef in lDefs:
            # Get the name and the type of the item
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

            # Node creation 

            # The data type needs the call of a function to create the node
            sWriteHandler = dData_type[sData_type_name]['WRITE']

            # Creation of code needed to import the module in which we can find
            #  the function used to create the node.
            lFunctionPart = string.split(sWriteHandler,'.')
            sFunction_module = string.join(lFunctionPart[:-1],".")
            sCode = "import " +sFunction_module
            exec sCode

            # Extraction of the data from the matrix
            val = npData[metro_data_object.index_of_matrix_col(sTag)]
            
            if dData_type[sData_type_name].has_key('CHILD'):
                # Construction of instruction doing the function call that will
                #  create the node containing a list of "sub-node"
                lChildList = dData_type[sData_type_name]['CHILD']
                sCode = "nodeData = " + sWriteHandler + \
                        "(domDoc,sXml_tag,lChildList,val)"
            else:
                # Construction of instruction doing the function call that will
                #  create the node 
                sCode = "nodeData = " + sWriteHandler + "(domDoc,sXml_tag,val)"

            exec sCode

            append_child(nodePrediction, nodeData)

def get_handler(sHandlerType, dDefData, dAllDefData=None):

    if dAllDefData == None:
        # Retrieve the informations about the data types
        dStandard_data_type = metro_config.get_value('XML_DATATYPE_STANDARD')
        dExtended_data_type = metro_config.get_value('XML_DATATYPE_EXTENDED')
        dAllDefData = metro_util.join_dictionaries(dStandard_data_type,
                                              dExtended_data_type)
    
    sTag = dDefData['NAME']
    sXml_tag = dDefData['XML_TAG']
    sData_type_name = dDefData['DATA_TYPE']

    if sData_type_name not in dAllDefData.keys():
        sMessage = _("Invalid data_type: (%s) for the following ") \
                   % (sData_type_name) +\
                   _("tag:(%s). Default data type will be used.") \
                   % (sTag)
        metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,
                                           sMessage)
        sData_type_name = 'DEFAULT'
                
    # The data type needs the call of a function to create the node
    sHandler = dAllDefData[sData_type_name][sHandlerType]

    return sHandler

def get_handler_import_code(sHandler):

    # Creation of code needed to import the module in which we can find
    #  the function used to create the node.
    lFunctionPart = string.split(sHandler,'.')
    sFunction_module = string.join(lFunctionPart[:-1],".")
    sCode = "import " +sFunction_module

    return sCode
