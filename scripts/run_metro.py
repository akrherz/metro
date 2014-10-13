'''
 Run the Bridge Model for each gridpoint in the given netcdf file, yeah
'''
import netCDF4
import sys
import pytz
import datetime
import numpy as np
import numpy.ma as ma
import subprocess
from pyiem.datatypes import temperature
import os
import shutil

IOFFSET = 62
JOFFSET = 70
CONDITIONS = ['Dry', 'frosty', 'Icy/Snowy', 'Melting', 'Freezing', 'Wet']

def find_initts( nc ):
    """ Provided the given netcdf file object, figure out the start time """
    tm = nc.variables['time']
    ts = datetime.datetime.strptime( tm.units[14:], "%Y-%m-%d %H:%M:%S")
    ts = ts.replace(tzinfo=pytz.timezone("UTC"))
    return ts

def make_output(nc, initts):
    """ Generate an output file to hold our results """
    fn = 'output/%s_output.nc' % (initts.strftime("%Y%m%d%H%M"),)
    ncout = netCDF4.Dataset(fn, 'w')
    # Setup dimensions
    ncout.createDimension('i_cross', len(nc.dimensions['i_cross']))
    ncout.createDimension('j_cross', len(nc.dimensions['j_cross']))
    ncout.createDimension('time', 72*4+1)

    # Setup variables
    tm = ncout.createVariable('time', np.int, ('time',))
    tm.units = "minutes since %s" % (initts.strftime("%Y-%m-%d %H:%M:%S"),)
    tm[:] = range(0,72*60+1,15)
    
    i_cross = ncout.createVariable('i_cross', np.float, ('i_cross',))
    i_cross.units = "m"
    i_cross[:] = range(len(nc.dimensions['i_cross']))

    j_cross = ncout.createVariable('j_cross', np.float, ('j_cross',))
    j_cross.units = "m"
    j_cross[:] = range(len(nc.dimensions['j_cross']))

    lat = ncout.createVariable('lat', np.float, ('i_cross', 'j_cross'))
    lat.long_name = "latitude"
    lat.units = "degrees_north"
    lat[:] = nc.variables['latitcrs'][:]

    lon = ncout.createVariable('lon', np.float, ('i_cross', 'j_cross'))
    lon.long_name = "longitude"
    lon.units = "degrees_east"
    lon[:] = nc.variables['longicrs'][:]

    # DEFINE THE VARS, PLEASE!
    dims = ('time', 'i_cross', 'j_cross')
    icond = ncout.createVariable('icond', np.byte, dims)
    icond.coordinates = "lon lat"
    icond.long_name = "Pavement Textual Condition"
    icond.value0 = 'Dry'
    icond.value1 = 'frosty'
    icond.value2 = 'Icy/Snowy'
    icond.value3 = 'Melting'
    icond.value4 = 'Freezing'
    icond.value5 = 'Wet'
        
    bdeckt = ncout.createVariable('bdeckt', np.float, dims)
    bdeckt.coordinates = "lon lat"
    bdeckt.units = "K"
    bdeckt.long_name = 'Bridge Deck Temperature'
    bdeckt.missing_value = np.array(1e20, bdeckt.dtype)

    h = ncout.createVariable('h', np.float, dims)
    h.coordinates = "lon lat"
    #h.units = "m"
    #h.long_name = 'Depth of Frost'
    h.missing_value = np.array(1e20, h.dtype)

    swout = ncout.createVariable('swout', np.float, dims)
    swout.coordinates = "lon lat"
    swout.units = "W m s-2"
    swout.long_name = 'Shortwave outgoing'
    swout.missing_value = np.array(1e20, swout.dtype)

    lwout = ncout.createVariable('lwout', np.float, dims)
    lwout.coordinates = "lon lat"
    lwout.units = "W m s-2"
    lwout.long_name = 'Longwave outgoing'
    lwout.missing_value = np.array(1e20, lwout.dtype)

    lf = ncout.createVariable('lf', np.float, dims)
    lf.coordinates = "lon lat"
    lf.missing_value = np.array(1e20, lf.dtype)

    tmpk = ncout.createVariable('tmpk', np.float, dims)
    tmpk.coordinates = "lon lat"
    tmpk.units = 'K'
    tmpk.missing_value = np.array(1e20, tmpk.dtype)

    dwpk = ncout.createVariable('dwpk', np.float, dims)
    dwpk.coordinates = "lon lat"
    dwpk.missing_value = np.array(1e20, dwpk.dtype)

    wmps = ncout.createVariable('wmps', np.float, dims)
    wmps.coordinates = "lon lat"
    wmps.missing_value = np.array(1e20, wmps.dtype)

    ifrost = ncout.createVariable('ifrost', np.int, dims)
    ifrost.coordinates = "lon lat"
    ifrost.missing_value = 0
    ifrost.missing_value = -1

    frostd = ncout.createVariable('frostd', np.float, dims)
    frostd.coordinates = "lon lat"
    frostd.missing_value = -99.
    frostd.missing_value = np.array(1e20, frostd.dtype)

    ncout.close()
    return netCDF4.Dataset(fn, 'a')

def make_rwis(lon, lat, initts):
    """ Generate spinup file """
    o = open('rwis.xml', 'w')
    o.write("""<?xml version="1.0"?>
<observation>
 <header>
  <filetype>rwis-observation</filetype>
  <version>1.0</version>
  <road-station>oaa</road-station>
  </header>
  <measure-list>""")
    # at Air Temp in C
    # td Dew point in C
    # pi presence of precipitation 0: No -- 1: Yes
    # ws wind speed in km / hr
    # sc condition code  1=DryCond 2=Wet 3=Ice 4=MixWaterSnow 
    #                    5=dew 6=Meltsnow 7=Frost 8=Ice
    # st road surface temp
    # sst sub surface temp
    for hr in range(-12,10):
        ts = initts + datetime.timedelta(hours=hr)
        o.write("""<measure>
      <observation-time>%s</observation-time>
      <at>7.10</at>
      <td>5.50</td>
      <pi>0</pi>
      <ws>4</ws>
      <sc>33</sc>
      <st>9.80</st>
      <sst>17.00</sst>
      </measure>
      """ % (ts.strftime("%Y-%m-%dT%H:%MZ"),))
    
    o.write("</measure-list></observation>")
    o.close()

    return 'rwis.xml'

def run_model(nc, initts, ncout, oldncout):
    """ Actually run the model, please """
    t2 = nc.variables['t2']
    u10 = nc.variables['u10']
    v10 = nc.variables['v10']
    tm = nc.variables['time']
    lwdown = nc.variables['lwdown']
    swdown = nc.variables['swdown']
    q2 = nc.variables['q2']
    rc = nc.variables['rain_con']
    rn = nc.variables['rain_non']
    lats = nc.variables['latitcrs']
    lons = nc.variables['longicrs']

    # keep masking in-tact as we only write data below when we have it
    otmpk = ma.array(ncout.variables['tmpk'][:])
    owmps = ma.array(ncout.variables['wmps'][:])
    oswout = ma.array(ncout.variables['swout'][:])
    olwout = ma.array(ncout.variables['lwout'][:])
    oh = ma.array(ncout.variables['h'][:])
    olf = ma.array(ncout.variables['lf'][:])
    obdeckt = ma.array(ncout.variables['bdeckt'][:])
    oifrost = ma.array(ncout.variables['ifrost'][:])
    odwpk = ma.array(ncout.variables['dwpk'][:])
    ofrostd = ma.array(ncout.variables['frostd'][:])
    oicond = ma.array(ncout.variables['icond'][:])
    #mini = 200
    #minj = 200
    #maxi = 0
    #maxj = 0
    errorcount = 0
    
    cmd = "python model/usr/bin/metro "
    cmd += "--roadcast-start-date %s " % (initts.strftime("%Y-%m-%dT%H:%MZ"),)
    cmd += "--input-forecast isumm5.xml " 
    cmd += "--input-observation rwis.xml "
    cmd += "--input-station station.xml "
    cmd += "--output-roadcast roadcast.xml "
    cmd += "--log-file /dev/null "
    #cmd += "--verbose-level 4 "
    cmd += "--use-solarflux-forecast --use-infrared-forecast"
    
    
    for i in range(len(nc.dimensions['i_cross'])):
        if errorcount > 100:
            print 'Too many errors, aborting....'
            sys.exit()
        #loopstart = datetime.datetime.now()
        for j in range(len(nc.dimensions['j_cross'])):
            lat = lats[i,j]
            lon = lons[i,j]
            #Hey, we only care about Iowa data! -97 40 -90 43.75
            if lat < 40 or lat > 43.75 or lon < -97 or lon > -90:
                continue
            rwisfn = make_rwis(lon, lat, initts)
            o = open('isumm5.xml', 'w')
            o.write("""<?xml version="1.0"?> 
<forecast>
  <header>
      <production-date>%s</production-date>     
      <version>1.1</version>
      <filetype>forecast</filetype>
      <station-id>ofr</station-id>
  </header>
  <prediction-list>""" % (
                datetime.datetime.utcnow().strftime("%Y-%m-%dT%H:%MZ"),))
            for t in range(1, len(nc.dimensions['time'])):
                ts = initts + datetime.timedelta(minutes=int(tm[t]))
                o.write("""<prediction>
          <forecast-time>%s</forecast-time>
          <at>10.0</at>
          <td>3.0</td>
          <ra>0.0</ra>
          <sn>0.0</sn>
          <ws>20</ws>
          <ap>993.8</ap>
          <wd>300</wd>
          <cc>0</cc>
          <sf>999</sf>
          <ir>999</ir>
      </prediction>
                """ % (ts.strftime("%Y-%m-%dT%H:%MZ"),))
            
            o.write("</prediction-list></forecast>")
            o.close()
            
            proc = subprocess.Popen(cmd, shell=True,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            se = proc.stderr.read()
            if se != "":
                errorcount += 1
                print 'metro error i:%03i j:%03i stderr:|%s|' % (i, j, 
                                                            se.strip())
                continue
            print proc.stdout.read()
    ncout.variables['tmpk'][:] = otmpk
    ncout.variables['wmps'][:] = owmps
    ncout.variables['swout'][:] = oswout
    ncout.variables['lwout'][:] = olwout
    ncout.variables['h'][:] = oh
    ncout.variables['lf'][:] = olf
    ncout.variables['bdeckt'][:] = obdeckt
    ncout.variables['ifrost'][:] = oifrost
    ncout.variables['frostd'][:] = ofrostd
    ncout.variables['dwpk'][:] = odwpk
    ncout.variables['icond'][:] = oicond
    # ncks -d i_cross,62,82 -d j_cross,70,98 201312131200_output.nc 
    # 201312131200_output2.nc
    #print mini, minj, maxi, maxj #62 70 82 98

def find_last_output(initts):
    ''' See if we have a previous run on file, that can be used to spin up
    our current run '''
    for i in range(-12,-73,-12):
        ts = initts + datetime.timedelta(hours=i)
        testfn = 'output/%s_iaoutput.nc' % (ts.strftime("%Y%m%d%H%M"),)
        if os.path.isfile(testfn):
            print '  Using %s as warmup values' % (testfn,)
            return netCDF4.Dataset(testfn, 'r')
    print 'Did not find a previous output, will use dummy RWIS data :('
    return None

def downsize_output(initts):
    ''' Subset the output file, so to save some space 66% actually '''
    fn1 = "output/%s_output.nc" % (initts.strftime("%Y%m%d%H%M"),)
    fn2 = "output/%s_iaoutput.nc" % (initts.strftime("%Y%m%d%H%M"),)
    fn3 = "/mesonet/share/frost/%s_iaoutput.nc" % (
                                                initts.strftime("%Y%m%d%H%M"),)
    if os.path.isfile(fn2):
        os.unlink(fn2)
    cmd = "ncks -d i_cross,%s,82 -d j_cross,%s,98 %s %s" % (IOFFSET, JOFFSET,
                                                            fn1, fn2)
    p = subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE,
                         stdout=subprocess.PIPE)
    p.stdout.read()
    # Make sure fn2 exists before deleting the old one
    if os.path.isfile(fn2):
        os.unlink(fn1)
        print '    Copy %s to %s' % (fn2, fn3)
        shutil.copyfile(fn2, fn3)
    

if __name__ == '__main__':
    ''' Do something please '''
    fn = sys.argv[1]
    nc = netCDF4.Dataset(fn)
    
    initts = find_initts( nc )
    ncout = make_output(nc, initts)
    oldncout = find_last_output(initts)
    run_model(nc, initts, ncout, oldncout)
    ncout.close()
    nc.close()
    downsize_output( initts )
    if os.path.isfile('faux_rwis.txt'):
        os.unlink('faux_rwis.txt')
    
    