'''
 Run the Bridge Model for each gridpoint in the given netcdf file, yeah
'''
from __future__ import print_function
import sys
import datetime
import subprocess
import os
import shutil

import netCDF4
import pytz
import numpy as np
import numpy.ma as ma
import xml.etree.ElementTree as ET
import pyiem.datatypes as dt
import pyiem.meteorology as met

IOFFSET = 62
JOFFSET = 70
CONDITIONS = ['Dry', 'frosty', 'Icy/Snowy', 'Melting', 'Freezing', 'Wet']


def find_initts(nc):
    """ Provided the given netcdf file object, figure out the start time """
    tm = nc.variables['time']
    ts = datetime.datetime.strptime(tm.units[14:], "%Y-%m-%d %H:%M:%S")
    ts = ts.replace(tzinfo=pytz.timezone("UTC"))
    return ts


def make_output(nc, initts):
    """ Generate an output file to hold our results """
    fn = 'output/%s_output.nc' % (initts.strftime("%Y%m%d%H%M"),)
    ncout = netCDF4.Dataset(fn, 'w')
    # Setup dimensions
    ncout.createDimension('i_cross', len(nc.dimensions['i_cross']))
    ncout.createDimension('j_cross', len(nc.dimensions['j_cross']))
    ncout.createDimension('time', 72*3+1)

    # Setup variables
    tm = ncout.createVariable('time', np.int, ('time',))
    tm.units = "minutes since %s" % (initts.strftime("%Y-%m-%d %H:%M:%S"),)
    tm[:] = range(0, 72*60+1, 20)

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
    icond.value1 = 'Dry'
    icond.value2 = 'Wet road'
    icond.value3 = 'Ice/snow'
    icond.value4 = 'Mix snow/water'
    icond.value5 = 'Dew'
    icond.value6 = 'Melting Snow'
    icond.value7 = 'Black Ice'
    icond.value8 = 'Icing Rain'

    bdeckt = ncout.createVariable('bdeckt', np.float, dims)
    bdeckt.coordinates = "lon lat"
    bdeckt.units = "K"
    bdeckt.long_name = 'Bridge Deck Temperature'
    bdeckt.missing_value = np.array(1e20, bdeckt.dtype)

    subsfct = ncout.createVariable('subsfct', np.float, dims)
    subsfct.coordinates = "lon lat"
    subsfct.units = "K"
    subsfct.long_name = 'Road Sub-Surface Temperature (40cm)'
    subsfct.missing_value = np.array(1e20, bdeckt.dtype)

    h = ncout.createVariable('h', np.float, dims)
    h.coordinates = "lon lat"
    # h.units = "m"
    # h.long_name = 'Depth of Frost'
    h.missing_value = np.array(1e20, h.dtype)

    swout = ncout.createVariable('swout', np.float, dims)
    swout.coordinates = "lon lat"
    swout.units = "W m s-2"
    swout.long_name = 'Shortwave outgoing'
    swout.missing_value = np.array(1e20, swout.dtype)

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


def fake_rwis(o, initts):
    """ Generate fake data, just to bootstrap us """
    for hr in range(-12, 10):
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


def make_rwis(i, j, initts, oldncout):
    """ Generate spinup file """
    i = i - IOFFSET
    j = j - JOFFSET

    o = open('rwis.xml', 'w')
    o.write("""<?xml version="1.0"?>
<observation>
 <header>
  <filetype>rwis-observation</filetype>
  <version>1.0</version>
  <road-station>oaa</road-station>
  </header>
  <measure-list>""")
    if oldncout is None:
        fake_rwis(o, initts)
        return

    ts0 = find_initts(oldncout)
    # at Air Temp in C
    tmpc = dt.temperature(oldncout.variables['tmpk'][:, i, j], 'K').value('C')
    # td Dew point in C
    dwpc = dt.temperature(oldncout.variables['dwpk'][:, i, j], 'K').value('C')
    # pi presence of precipitation 0: No -- 1: Yes
    # ws wind speed in km / hr
    ws = dt.speed(oldncout.variables['wmps'][:, i, j], 'MPS').value('KMH')
    # sc condition code  1=DryCond 2=Wet 3=Ice 4=MixWaterSnow
    #                    5=dew 6=Meltsnow 7=Frost 8=Ice
    # Was set to 33 for SSI ?
    icond = oldncout.variables['icond'][:, i, j]
    # st road surface temp
    bridgec = dt.temperature(
        oldncout.variables['bdeckt'][:, i, j], 'K').value('C')
    # sst sub surface temp
    subsfc = dt.temperature(
        oldncout.variables['subsfct'][:, i, j], 'K').value('C')
    t1 = initts + datetime.timedelta(hours=12)
    for tstep in range(4, len(oldncout.dimensions['time']), 4):
        ts = ts0 + datetime.timedelta(
                                minutes=int(oldncout.variables['time'][tstep]))
        if ts > t1:
            break
        o.write("""<measure><observation-time>%s</observation-time>
<at>%.2f</at><td>%.2f</td><pi>0</pi><ws>%.2f</ws><sc>%s</sc><st>%.2f</st>
<sst>%.2f</sst></measure>
      """ % (ts.strftime("%Y-%m-%dT%H:%MZ"), tmpc[tstep], dwpc[tstep],
             ws[tstep], icond[tstep], bridgec[tstep], subsfc[tstep]))

    o.write("</measure-list></observation>")
    o.close()


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
    otmpk._sharedmask = False
    owmps = ma.array(ncout.variables['wmps'][:])
    owmps._sharedmask = False
    oswout = ma.array(ncout.variables['swout'][:])
    oswout._sharedmask = False
    oh = ma.array(ncout.variables['h'][:])
    oh._sharedmask = False
    olf = ma.array(ncout.variables['lf'][:])
    olf._sharedmask = False
    obdeckt = ma.array(ncout.variables['bdeckt'][:])
    obdeckt._sharedmask = False
    osubsfct = ma.array(ncout.variables['subsfct'][:])
    osubsfct._sharedmask = False
    oifrost = ma.array(ncout.variables['ifrost'][:])
    oifrost._sharedmask = False
    odwpk = ma.array(ncout.variables['dwpk'][:])
    odwpk._sharedmask = False
    ofrostd = ma.array(ncout.variables['frostd'][:])
    ofrostd._sharedmask = False
    oicond = ma.array(ncout.variables['icond'][:])
    oicond._sharedmask = False
    # mini = 200
    # minj = 200
    # maxi = 0
    # maxj = 0
    errorcount = 0

    cmd = "python2 model/usr/bin/metro "
    cmd += "--roadcast-start-date %s " % (initts.strftime("%Y-%m-%dT%H:%MZ"),)
    cmd += "--input-forecast isumm5.xml "
    cmd += "--input-observation rwis.xml "
    cmd += "--input-station station.xml "
    cmd += "--output-roadcast roadcast.xml "
    cmd += "--log-file /dev/null "
    # cmd += "--verbose-level 4 "
    cmd += "--use-solarflux-forecast --use-infrared-forecast"

    # We don't have pressure from MM5 (yet)
    pressure = dt.pressure(1000.0, 'MB')

    for i in range(len(nc.dimensions['i_cross'])):
        if errorcount > 100:
            print('Too many errors, aborting....')
            sys.exit()
        # loopstart = datetime.datetime.now()
        for j in range(len(nc.dimensions['j_cross'])):
            lat = lats[i, j]
            lon = lons[i, j]
            # Hey, we only care about Iowa data! -97 40 -90 43.75
            if lat < 40 or lat > 43.75 or lon < -97 or lon > -90:
                continue
            make_rwis(i, j, initts, oldncout)
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
                tmpk = dt.temperature(t2[t, i, j], 'K')
                mr = dt.mixingratio(q2[t, i, j], 'KG/KG')
                dwp = met.dewpoint_from_pq(pressure, mr)
                sped = dt.speed((u10[t, i, j]**2 + v10[t, i, j]**2)**.5, 'MPS')
                # sn - snow accumulation in cm
                # ap - surface pressure in mb
                o.write("""<prediction>
          <forecast-time>%s</forecast-time>
          <at>%.1f</at>
          <td>%.1f</td>
          <ra>%.1f</ra>
          <sn>0.0</sn>
          <ws>%.1f</ws>
          <ap>993.8</ap>
          <wd>300</wd>
          <cc>0</cc>
          <sf>%.1f</sf>
          <ir>%.1f</ir>
      </prediction>
                """ % (ts.strftime("%Y-%m-%dT%H:%MZ"),
                       tmpk.value("C"), dwp.value("C"),
                       (rn[t, i, j] + rc[t, i, j])*10.,
                       sped.value("KMH"),
                       swdown[t, i, j], lwdown[t, i, j])
                )

            o.write("</prediction-list></forecast>")
            o.close()

            proc = subprocess.Popen(cmd, shell=True,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE)
            se = proc.stderr.read().decode("utf-8")
            if se != "":
                errorcount += 1
                print(('metro error i:%03i j:%03i stderr:|%s|'
                       ) % (i, j, se.strip()))
                continue

            # Starts at :20 minutes after start time
            tree = ET.parse('roadcast.xml')
            root = tree.getroot()
            tstep = 0
            for c in root.findall('./prediction-list/prediction'):
                tstep += 1

                # Road surface temperature    st    Celsius
                obdeckt[tstep, i, j] = float(c.find('./st').text) + 273.15
                # Road sub surface temperature* (40 cm)    sst    Celsius
                osubsfct[tstep, i, j] = float(c.find('./sst').text) + 273.15
                # Air temperature    at    Celsius
                otmpk[tstep, i, j] = float(c.find('./at').text) + 273.15
                # Dew point    td    Celsius
                odwpk[tstep, i, j] = float(c.find('./td').text) + 273.15
                # Wind speed    ws    km/h
                owmps[tstep, i, j] = float(c.find('./ws').text)
                # Quantity of snow or ice on the road    sn    cm
                # Quantity of rain on the road    ra    mm
                # Total (1 hr) snow precipitation    qp-sn    cm
                # Total (1 hr) rain precipitation    qp-ra    mm
                # Solar flux    sf    W/m2
                oswout[tstep, i, j] = float(c.find('./sf').text)
                # Incident infra-red flux    ir    W/m2
                # Vapor flux    fv    W/m2
                # Sensible heat    fc    W/m2
                # Anthropogenic flux    fa    W/m2
                # Ground exchange flux    fg    W/m2
                # Blackbody effect    bb    W/m2
                # Phase change    fp    W/m2
                # Road condition    rc    METRo code
                oicond[tstep, i, j] = int(c.find('./rc').text)
                # Octal cloud coverage**    cc    octal
    ncout.variables['tmpk'][:] = otmpk
    ncout.variables['wmps'][:] = dt.speed(owmps, 'KMH').value('MPS')
    ncout.variables['swout'][:] = oswout
    ncout.variables['h'][:] = oh
    ncout.variables['lf'][:] = olf
    ncout.variables['bdeckt'][:] = obdeckt
    ncout.variables['subsfct'][:] = osubsfct
    ncout.variables['ifrost'][:] = oifrost
    ncout.variables['frostd'][:] = ofrostd
    ncout.variables['dwpk'][:] = odwpk
    ncout.variables['icond'][:] = oicond
    # ncks -d i_cross,62,82 -d j_cross,70,98 201312131200_output.nc
    # 201312131200_output2.nc
    # print mini, minj, maxi, maxj #62 70 82 98


def find_last_output(initts):
    ''' See if we have a previous run on file, that can be used to spin up
    our current run '''
    for i in range(-12, -73, -12):
        ts = initts + datetime.timedelta(hours=i)
        testfn = 'output/%s_iaoutput.nc' % (ts.strftime("%Y%m%d%H%M"),)
        if os.path.isfile(testfn):
            print('  Using %s as warmup values' % (testfn,))
            return netCDF4.Dataset(testfn, 'r')
    print('  Did not find a previous output, will use dummy RWIS data :(')
    return None


def downsize_output(initts):
    """ Subset the output file, so to save some space 66% actually """
    fn1 = "output/%s_output.nc" % (initts.strftime("%Y%m%d%H%M"),)
    fn2 = "output/%s_iaoutput.nc" % (initts.strftime("%Y%m%d%H%M"),)
    fn3 = "/mesonet/share/frost/metro/%s_iaoutput.nc" % (
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
        print('  Copy %s to /mesonet/share/frost/metro' % (fn2,))
        shutil.copyfile(fn2, fn3)


def main():
    """ Go Main Go """
    fn = sys.argv[1]
    print(("--> Start run_metro.py %s %s"
           ) % (fn.split("/")[-1], datetime.datetime.now().strftime("%H:%M")))
    if not os.path.isfile(fn):
        print("ISUMM5 input: %s is missing, abort" % (fn,))
        return
    nc = netCDF4.Dataset(fn)

    initts = find_initts(nc)
    ncout = make_output(nc, initts)
    oldncout = find_last_output(initts)
    run_model(nc, initts, ncout, oldncout)
    ncout.close()
    nc.close()
    downsize_output(initts)
    if os.path.isfile('faux_rwis.txt'):
        os.unlink('faux_rwis.txt')

    print("--> End run_metro.py %s" % (
                            datetime.datetime.now().strftime("%H:%M"),))


if __name__ == '__main__':
    # Do something please
    main()
