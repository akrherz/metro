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
Name:		Metro_physics.py
Description: Contains the physical equation used in metro
Notes: 
Author: Miguel Tremblay
Date: August 24th 2004
"""

import math
import numarray

from toolbox import metro_constant
from toolbox import metro_util


def foqst(dTD, dPO):
    """
     Name: fodqst

     Parameters:[I] double dTD : dew point
                [I] double dPO : surface pressure
     Returns: None

     Functions Called:

     Description: Computation of the specific saturating humidity

     Notes: 

     Revision History:
     Author		Date		Reason
     Miguel Tremblay      August 24th 2004
     """
    return metro_constant.fEps1/(max(1.0, dPO/foew(dTD))\
                                 - metro_constant.fEps2)

    
def foew(dPO):
    """
     Name: foew

     Parameters: [I] double dPO : surface pressure
     Returns: None

     Functions Called: Vapour pressure

     Description: 

     Notes: 

     Revision History:
     Author		Date		Reason
     Miguel Tremblay      August 24th 2004
     """
    dDiff = dPO-metro_constant.fTrpl
    fResult =  610.78*math.exp(min(metro_util.sign(17.269,dDiff),\
                               metro_util.sign(21.875,dDiff))*\
                           abs(dDiff)/ \
                           (dPO-35.86+max(0.0,metro_util.sign(28.2,-dDiff))))
    
    return fResult
