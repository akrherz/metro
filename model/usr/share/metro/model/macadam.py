# This file was automatically generated by SWIG (http://www.swig.org).
# Version 2.0.10
#
# Do not make changes to this file unless you know what you are doing--modify
# the SWIG interface file instead.



from sys import version_info
if version_info >= (2,6,0):
    def swig_import_helper():
        from os.path import dirname
        import imp
        fp = None
        try:
            fp, pathname, description = imp.find_module('_macadam', [dirname(__file__)])
        except ImportError:
            import _macadam
            return _macadam
        if fp is not None:
            try:
                _mod = imp.load_module('_macadam', fp, pathname, description)
            finally:
                fp.close()
            return _mod
    _macadam = swig_import_helper()
    del swig_import_helper
else:
    import _macadam
del version_info
try:
    _swig_property = property
except NameError:
    pass # Python < 2.2 doesn't have 'property'.
def _swig_setattr_nondynamic(self,class_type,name,value,static=1):
    if (name == "thisown"): return self.this.own(value)
    if (name == "this"):
        if type(value).__name__ == 'SwigPyObject':
            self.__dict__[name] = value
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    if (not static):
        self.__dict__[name] = value
    else:
        raise AttributeError("You cannot add attributes to %s" % self)

def _swig_setattr(self,class_type,name,value):
    return _swig_setattr_nondynamic(self,class_type,name,value,0)

def _swig_getattr(self,class_type,name):
    if (name == "thisown"): return self.this.own()
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError(name)

def _swig_repr(self):
    try: strthis = "proxy of " + self.this.__repr__()
    except: strthis = ""
    return "<%s.%s; %s >" % (self.__class__.__module__, self.__class__.__name__, strthis,)

try:
    _object = object
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0



def print_args(*args):
  return _macadam.print_args(*args)
print_args = _macadam.print_args

def Do_Metro(*args):
  return _macadam.Do_Metro(*args)
Do_Metro = _macadam.Do_Metro

def get_ra():
  return _macadam.get_ra()
get_ra = _macadam.get_ra

def get_sn():
  return _macadam.get_sn()
get_sn = _macadam.get_sn

def get_rc():
  return _macadam.get_rc()
get_rc = _macadam.get_rc

def get_rt():
  return _macadam.get_rt()
get_rt = _macadam.get_rt

def get_ir():
  return _macadam.get_ir()
get_ir = _macadam.get_ir

def get_sf():
  return _macadam.get_sf()
get_sf = _macadam.get_sf

def get_fv():
  return _macadam.get_fv()
get_fv = _macadam.get_fv

def get_fc():
  return _macadam.get_fc()
get_fc = _macadam.get_fc

def get_g():
  return _macadam.get_g()
get_g = _macadam.get_g

def get_bb():
  return _macadam.get_bb()
get_bb = _macadam.get_bb

def get_fp():
  return _macadam.get_fp()
get_fp = _macadam.get_fp

def get_echec():
  return _macadam.get_echec()
get_echec = _macadam.get_echec

def get_sst():
  return _macadam.get_sst()
get_sst = _macadam.get_sst

def get_depth():
  return _macadam.get_depth()
get_depth = _macadam.get_depth

def get_nbr_levels():
  return _macadam.get_nbr_levels()
get_nbr_levels = _macadam.get_nbr_levels

def get_lt():
  return _macadam.get_lt()
get_lt = _macadam.get_lt
# This file is compatible with both classic and new-style classes.


