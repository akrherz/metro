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

import numarray

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
             Numarray are used to represent the matrix.

    It is possible to prevent modification of the data by using the read_only
    functionnality provided by the class.

    When an error occur in one of the method, a MetroDataError exception
    is raised.
    """

    def __init__(self, lData_types=[]):
        """Perform initialisation of a Metro_data object

        Arguments:        
        lData_types = use to initialised the matrix with a number of column
                      determined by len(lData_types).
        """
                      
        
        self.bRead_only = False
        self.dHeader = {}
        self.naMatrix = numarray.array([],type=numarray.Float)

         # nom des colonne de la matrice
        self.lMatrix_col_name = []
        for data_type in lData_types:
            self.lMatrix_col_name.append(data_type['NAME'])

    def set_readonly( self, bIs_read_only ):
        """Set read only.status to the value of bIs_read_only."""
        
        self.bRead_only = bIs_read_only

    def is_readonly( self ):
        """Get status of the read only flag"""
        
        return self.bRead_only

#-------------------------------------------------------------------------------
#
# Name:         set_header_value
#
# Parameters:   I key, I value :
#
# Returns:      0 si succes
#
# Descriptions: sauvegarde de l'entete d'un Infile
#
#-------------------------------------------------------------------------------
    def set_header_value( self, sKey, value ):
        """Set/add a value to the header"""
        if not self.is_readonly():
            self.dHeader[sKey] = value
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


#-------------------------------------------------------------------------------
#
# Name:         set_header
#
# Parameters:   I header : dictionnaire respectant le format defini plus haut
#
# Returns:      0 si succes
#
# Descriptions: sauvegarde de l'entete d'un Infile
#
#-------------------------------------------------------------------------------
    def set_header( self, dComplete_header ):
        """Set complete header"""
        if not self.is_readonly():
            self.dHeader = dComplete_header
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


#-------------------------------------------------------------------------------
#
# Name:         init_matrix
#
# Parameters:   iNb_row, iNb_col, fVal
#
# Returns:      
#
# Descriptions: initialise ume matrice de nb_row par nb_col avec la valeur val
#
#-------------------------------------------------------------------------------
    def init_matrix( self, iNb_row, iNb_col, fVal=metro_constant.NaN ):
        """Init a matrix of n row and m column filled with a value."""
        if not self.is_readonly():
            self.naMatrix = numarray.zeros((iNb_row,iNb_col),numarray.Float)
            self.naMatrix = self.naMatrix + fVal
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY        

#-------------------------------------------------------------------------------
#
# Name:         set_matrix
#
# Parameters:   I data : matrice respectant le format defini plus haut
#
# Returns:      0 si succes
#
# Descriptions:
#
#-------------------------------------------------------------------------------
    def set_matrix( self, naMatrix ):
        """Set the whole matrix with a new one.

        This method should be used with care because the new and old matrix
        must have the same column count. Also column name of the old matrix
        will be the only way to access column of the new one.
        """
        if not self.is_readonly():
            self.naMatrix = naMatrix
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


#-------------------------------------------------------------------------------
#
# Name:         set_matrix_col
#
# Parameters:   I sCol_name : nom de la colonne, I naCol : tableau de donnee
#
# Returns:      0 si succes
#
#
# Descriptions: ecrase le contenue de la colonne iCol avec de nouvelle donnees
#
#-------------------------------------------------------------------------------
    def set_matrix_col( self, sCol_name, naCol ):
        """Set matrix column with a new one."""

        if sCol_name in self.lMatrix_col_name:
            iCol = self.lMatrix_col_name.index(sCol_name)
        else:
            sMatrix_col_list = metro_util.list2string(self.lMatrix_col_name)
            sError = _("%s is not a valid column name. Valid column name ") \
                     % (sCol_name) + \
                     _("are: %s") % (sMatrix_col_list)
            raise ERROR_METRO_DATA, sError
        
        if not self.is_readonly():
            if iCol > len(self.naMatrix[0,:]):
               sOutOfBoundError = _("Array does not contain this indice: %d") \
                                  % iCol
               raise ERROR_METRO_DATA, sOutOfBoundError
            elif len(self.naMatrix[:,iCol]) != len(naCol):
                sLengthError = _("Array does not have the right lenght.\n") + \
                               _("Array length: %d \n") % len(naCol) + \
                               _("Matrix length: %d \n") % len(self.naMatrix[:,icol])
                raise  ERROR_METRO_DATA, sLengthError
            else:
                self.naMatrix[:,iCol] = naCol
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


#-------------------------------------------------------------------------------
#
# Name:         append_matrix_row
#
# Parameters:   I lData_row : une ligne de la matrice
#
# Returns:      0 si succes
#
# Descriptions:
#
#-------------------------------------------------------------------------------
    def append_matrix_row( self, lData_row ):
        """Append a new row of data to the matrix."""
        if not self.is_readonly():
            #remplace les None par la constante NaN
            while None in lData_row:
                iIndex = lData_row.index(None)
                lData_row[iIndex] = metro_constant.NaN

            self.naMatrix = self.__append_row_to_matrix(self.naMatrix,lData_row)
        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY

#-------------------------------------------------------------------------------
#
# Name:         append_matrix_col
#
# Parameters:   I sCol_name  : nom de la colonne
#               I naData_col : la colonne a inserer dans la matrice
#
# Returns:      0 si succes
#
# Descriptions:
#
#-------------------------------------------------------------------------------
    def append_matrix_col( self, sCol_name, naData_col ):
        """Append a new column of data to the matrix.

        Matix column will be accessible with the name specified by sCol_name.
        """
        if not self.is_readonly():

            if sCol_name not in self.lMatrix_col_name:
                self.lMatrix_col_name.append(sCol_name)

                #ajoute a la matrice
                self.naMatrix = self.__append_col_to_matrix(self.naMatrix,\
                                                            naData_col)
            else:
                sError = _("Cant append column '%s'.%s") % (sCol_name,
                                                            MESSAGE_COL_EXIST)
                raise ERROR_METRO_DATA, sError

        else:
            metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,
                                       MESSAGE_READONLY)
            raise ERROR_METRO_DATA, MESSAGE_READONLY


#-------------------------------------------------------------------------------
#
# Name:         get_header
#
# Parameters:   aucun
#
# Returns:      dictionnaire respectant le format defini plus haut, si il y a une
#               erreur, retourne un dictionnaire vide
#
# Descriptions: recupere le header
#
#-------------------------------------------------------------------------------
    def get_header( self ):
        """Get the complete header dictionary."""
        return self.dHeader

#-------------------------------------------------------------------------------
#
# Name:         get_header_value
#
# Parameters:   sKey
#
# Returns:      valuer identifier par la cle(sKey)
#
# Descriptions: recupere une valeur du header
#
#-------------------------------------------------------------------------------
    def get_header_value( self, sKey ):
        """Get value of a specific header key"""
        if sKey in self.dHeader:
            return self.dHeader[sKey]
        else:
            sError = _("'%s' is not a valid key. Valid keys are:\n%s") \
                     % (sKey, metro_util.list2string(self.dHeader.keys()))
            raise ERROR_METRO_DATA, sError

#-------------------------------------------------------------------------------
#
# Name:         get_matrix
#
# Parameters:   aucun
#
# Returns:      matrice respectant le format defini plus haut, si il y a une
#               erreur, retourne une matrice vide
#
# Descriptions: Recupere une copie des donnees
#
#-------------------------------------------------------------------------------
    def get_matrix( self ):
        """Get a copy of the whole matrix."""
        return self.naMatrix.copy()

#-------------------------------------------------------------------------------
#
# Name:         get_matrix_col
#
# Parameters:   I sCol_name : nom de la colonne desire,
#
# Returns:      colonne de la matrice
#
#
# Descriptions: Retourne une copie d'une colone de la matrice.
#
#-------------------------------------------------------------------------------
    def get_matrix_col( self, sCol_name ):
        """Get a copy of a matrix column identified by sCol_name."""
        
        if sCol_name in self.lMatrix_col_name:
            iCol = self.lMatrix_col_name.index(sCol_name)
        else:
            sMatrix_col_list = metro_util.list2string(self.lMatrix_col_name)
            sError = _("%s is not a valid column name. Valid column name ") \
                     % (sCol_name) + \
                     _("are: %s") % (sMatrix_col_list)
            raise ERROR_METRO_DATA, sError

        iCol = self.lMatrix_col_name.index(sCol_name)

        return self.naMatrix[:,iCol].copy()

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


    
####################################################
# Name: del_matrix_row
#
# Parameters: numarray naIndices : Indices of row to delete
#
# Returns: None
#
# Functions Called: 
#   
#
# Description: Used in QC to remove wrong data. Indices must be in increasing
#               order.
#
# Notes: 
#
# Revision History:
#  Author		Date		Reason
# Miguel Tremblay      August 4th 2004
#####################################################
    def del_matrix_row(self, naIndiceToRemove):
        """Delete one or more row identified by indices.

        Arguments:        
        naIndiceToRemove: numarray of indices to remove.
                          Indices must be in increasing order.
        """
        if not self.is_readonly():
            for i in range(len(naIndiceToRemove)-1,-1,-1):
                nIndice = naIndiceToRemove[i]                
                sMessage =  "removing %d" % ((nIndice))
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                   sMessage)
                naFirstPart = self.naMatrix[0:nIndice,:]
                sMessage = "len(%s)" % (len(self.naMatrix))
                metro_logger.print_message(metro_logger.LOGGER_MSG_DEBUG,\
                                   sMessage)
                if nIndice+1 < len(self.naMatrix) :
                    naSecondPart = self.naMatrix[nIndice+1:len(self.naMatrix),:]
                    self.naMatrix = numarray.concatenate((naFirstPart,
                                                          naSecondPart))
                else:
                    self.naMatrix = naFirstPart
                # Check if there at least one element left
                if len(self.naMatrix) ==0:
                    sEmptyMatrixError = _("All the data are invalid")
                    metro_logger.print_message(metro_logger.LOGGER_MSG_WARNING,\
                                               sMessage)
                    raise ERROR_EMPTY_MATRIX
                
        else:
            raise ERROR_METRO_DATA, MESSAGE_READONLY

    def __append_row_to_matrix( self, naMatrix, naRow ):
        iCol = len(naRow)
        iRow = len(naMatrix)        
        naMatrix = numarray.resize(naMatrix, (iRow+1,iCol) )
        naMatrix[iRow:] = naRow
        return naMatrix

    def __append_col_to_matrix( self, naMatrix, naCol ):
        naMatrix.transpose()
        naMatrix = self.__append_row_to_matrix(naMatrix, naCol)
        naMatrix.transpose()
        return naMatrix
