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

import numpy

import metro_logger
from toolbox import metro_constant
from toolbox import metro_util

_ = metro_util.init_translation('metro_data')

#Exception
ERROR_METRO_DATA = "MetroDataError"
MESSAGE_READONLY = _("This metro_data object is READONLY")
MESSAGE_COL_EXIST = _("This column name already exist")
ERROR_EMPTY_MATRIX = "sEmptyMatrixError"

class Metro_data:
    """Basic data class for METRo.

    This class contains all the data stucture and operation to manipulate
    standard METRo data obtained from various sources such as forecast and
    observation files.

    Data are separated in two categories: Header and Matrix.

    Header : contains all the informations that describe the data obtained
             from the file. ex: production date, version, etc
             A dictionary is use to represent the header.

    Matrix : contains the actual data stored in a matrix where line contains
             a set of related data. Each column as a name associated to it.
             To access a column you must use its name.
             ex 'AT', 'FORECAST_TIME', etc.
             Numpy are used to represent the matrix.

    It is possible to prevent modification of the data by using the read_only
    functionnality provided by the class.

    When an error occur in one of the method, a MetroDataError exception
    is raised.
    """

    def __init__(self, lStdData_types=[], lExtData_types=[]):
        """
        Perform initialisation of a Metro_data object.
        Column are of to kind: Standard Element and Extended element.
        Both are treated the same way but a call to the method:
        'is_standardCol' can be use to distinguish both.

        Arguments:        
        lStdData_types = use to initialised the matrix with a number of column
                         determined by len(lStdData_types) + len(lExtData_types).

        lExtData_types = use to initialised the matrix with a number of column
                         determined by len(lStdData_types) + len(lExtData_types).                         
        """
                      
        
        self.bRead_only = False
        self.dHeader = {}
        self.npMatrix = numpy.array([], dtype=numpy.float)

        # Name of the columns of the matrix
        self.lMatrix_std_col_name = []
        for data_type in lStdData_types:
            self.lMatrix_std_col_name.append(data_type['NAME'])

        self.lMatrix_ext_col_name = []
        for data_type in lExtData_types:
            self.lMatrix_ext_col_name.append(data_type['NAME'])

        self.lMatrix_col_name = self.lMatrix_std_col_name + self.lMatrix_ext_col_name
        


    def set_readonly( self, bIs_read_only ):
        """
        Set read only.status to the value of bIs_read_only.
        """
        self.bRead_only = bIs_read_only

    def is_readonly( self ):
        """
        Get status of the read only flag.
        """
        return self.bRead_only

    def is_standardCol( self, sColName ):
        """
        Return True if sColName is a standard column.
        """
        return sColName in self.lMatrix_std_col_name

    def set_header_value( self, sKey, value ):
        """
        Set/add a value to the header

        Parameters:
          sKey (string): element to set this value to
          value (??): value of the element sKey

        """
        if not self.is_readonly():
            self.dHeader[sKey] = value
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


    def set_header( self, dComplete_header ):
        """
        Set complete header

        Parameters:
        dComplete_header (dictionnary): dictionnary containing the tag of the
        elements (the keys) and their corresponding value.

        """
        if not self.is_readonly():
            self.dHeader = dComplete_header
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


    def init_matrix( self, iNb_row, iNb_col, fVal=metro_constant.NaN ):
        """
        Init a matrix of n row and m column filled with a value.

        
        """
        if not self.is_readonly():
            self.npMatrix = numpy.zeros((iNb_row,iNb_col))
            self.npMatrix = self.npMatrix + fVal
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY        


    def set_matrix( self, npMatrix ):
        """Set the whole matrix with a new one.

        This method should be used with care because the new and old matrix
        must have the same column count. Also column name of the old matrix
        will be the only way to access column of the new one.
        """
        if not self.is_readonly():
            self.npMatrix = npMatrix
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


    def set_matrix_col( self, sCol_name, npCol ):
        """
        Set matrix column with a new one. Overwrite the old values.

        Parameters:
        sCol_name (string): tag of the column to set
        npCol (numpy array): the array corresponding to this column.
        
        """

        if sCol_name in self.lMatrix_col_name:
            iCol = self.lMatrix_col_name.index(sCol_name)
        else:
            sMatrix_col_list = metro_util.list2string(self.lMatrix_col_name)
            sError = _("%s is not a valid column name. Valid column name ") \
                     % (sCol_name) + \
                     _("are: %s") % (sMatrix_col_list)
            raise ERROR_METRO_DATA, sError
        
        if not self.is_readonly():
            if iCol > len(self.npMatrix[0,:]):
               sOutOfBoundError = _("Array does not contain this indice: %d") \
                                  % iCol
               raise ERROR_METRO_DATA, sOutOfBoundError
            elif len(self.npMatrix[:,iCol]) != len(npCol):
                sLengthError = _("Array does not have the right lenght.\n") + \
                               _("Array length: %d \n") % len(npCol) + \
                               _("Matrix length: %d \n") % len(self.npMatrix[:,icol])
                raise  ERROR_METRO_DATA, sLengthError
            else:
                self.npMatrix[:,iCol] = npCol
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


    def append_matrix_row( self, lData_row ):
        """
        Name:         append_matrix_row

        Parameters:   I lData_row : line of the matrix

        Returns:      0 if success

        Descriptions: Append a new row of data to the matrix.
        """
        if not self.is_readonly():
            # Replace the None by metro_constant.NaN 
            while None in lData_row:
                iIndex = lData_row.index(None)
                lData_row[iIndex] = metro_constant.NaN
            self.npMatrix = self.__append_row_to_matrix(self.npMatrix,lData_row)
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


    def append_matrix_col( self, sCol_name, npData_col ):
        """
        Name:         append_matrix_col

        Parameters:   I sCol_name  : column name
                      I npData_col : column to insert in the matrix.
  
        Returns:      0 if success

        Descriptions:  Append a new column of data to the matrix. Matix column
                       will be accessible with the name specified by sCol_name.
                       Column will be treated as extended
        
        """
        if not self.is_readonly():
            if sCol_name not in self.lMatrix_col_name:
                self.lMatrix_col_name.append(sCol_name)
                self.lMatrix_ext_col_name.append(sCol_name)

                # Append column in the matrix
                self.npMatrix = self.__append_col_to_matrix(self.npMatrix,\
                                                            npData_col)
            else:
                sError = _("Cant append column '%s'.%s") % (sCol_name,
                                                            MESSAGE_COL_EXIST)
                raise ERROR_METRO_DATA, sError

        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


    def get_header( self ):
        """
        Get the complete header dictionary. Dictionnary could be empty if it is not
        set before.

        Returns self.dHeader (dictionnary)
        """
        return self.dHeader


    def get_header_value(self, sKey ):
        """
        Get value of a specific header key
        
        Parameters:
        sKey (string): key of the value to return (tag of element)

        Returns value (undefined format) for this key.
        """
        if sKey in self.dHeader:
            return self.dHeader[sKey]
        else:
            sError = _("'%s' is not a valid key. Valid keys are:\n%s") \
                     % (sKey, metro_util.list2string(self.dHeader.keys()))
            raise ERROR_METRO_DATA, sError


    def get_matrix( self ):
        """
        Get a copy of the whole matrix. Matrix is empty if not set before.

        """
        return self.npMatrix.copy()


    def get_matrix_col( self, sCol_name ):
        """
        Name:         get_matrix_col

        Parameters:   I sCol_name : name of the column to return.

        Returns:      Column of the matrix.

        Descriptions: Get a copy of a matrix column identified by sCol_name.
        """

        if sCol_name in self.lMatrix_col_name:
            iCol = self.lMatrix_col_name.index(sCol_name)
        else:
            sMatrix_col_list = metro_util.list2string(self.lMatrix_col_name)
            sError = _("%s is not a valid column name. Valid column name ") \
                     % (sCol_name) + \
                     _("are: %s") % (sMatrix_col_list)
            raise ERROR_METRO_DATA, sError

        iCol = self.lMatrix_col_name.index(sCol_name)

        return self.npMatrix[:,iCol].copy()

    def index_of_matrix_col( self, sCol_name ):
        """Get index value of a matrix column identified by sCol_name."""
        if sCol_name in self.lMatrix_col_name:
            return self.lMatrix_col_name.index(sCol_name)
        else:
            sMatrix_col_list = metro_util.list2string(self.lMatrix_col_name)
            sError = _("%s is not a valid column name. Valid column name ") \
                     % (sCol_name) + \
                     _("are: %s") % (sMatrix_col_list)
            raise ERROR_METRO_DATA, sError

    def get_matrix_col_list( self ):
        """Get list of all the matrix column name."""
        return self.lMatrix_col_name

    def get_nb_matrix_col( self ):
        """Get number of matrix column."""
        return len(self.lMatrix_col_name)

    
    def del_matrix_row(self, npIndiceToRemove):
        """
        Delete one or more row identified by indices.
        Used in QC to remove wrong data. Indices must be in increasing
        order.

        Arguments:        
        npIndiceToRemove (numpy) of indices to remove.
                          Indices must be in increasing order.
        """
        if not self.is_readonly():
            for i in range(len(npIndiceToRemove)-1,-1,-1):
                nIndice = npIndiceToRemove[i]                
                sMessage =  "removing %d" % ((nIndice))
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                   sMessage)
                npFirstPart = self.npMatrix[0:nIndice,:]
                sMessage = "len(%s)" % (len(self.npMatrix))
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                   sMessage)
                if nIndice+1 < len(self.npMatrix) :
                    npSecondPart = self.npMatrix[nIndice+1:len(self.npMatrix),:]
                    self.npMatrix = numpy.concatenate((npFirstPart,
                                                          npSecondPart))
                else:
                    self.npMatrix = npFirstPart
                # Check if there at least one element left
                if len(self.npMatrix) ==0:
                    sEmptyMatrixError = _("All the data are invalid")
                    metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,\
                                               sMessage)
                    raise ERROR_EMPTY_MATRIX
                
        else:
            raise ERROR_METRO_DATA, MESSAGE_READONLY

    def __append_row_to_matrix( self, npMatrix, npRow ):
        iCol = len(npRow)
        iRow = len(npMatrix)
        npMatrix = numpy.resize(npMatrix, (iRow+1,iCol) )
        npMatrix[iRow:] = npRow
        return npMatrix

    def __append_col_to_matrix( self, npMatrix, npCol ):
        npMatrix = npMatrix.transpose()
        npMatrix = self.__append_row_to_matrix(npMatrix, npCol)
        npMatrix = npMatrix.transpose()
        return npMatrix
