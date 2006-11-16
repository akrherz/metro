# This file was created automatically by SWIG 1.3.29.
# Don't modify this file, modify the SWIG interface instead.
# This file is compatible with both classic and new-style classes.

import _macadam
import new
new_instancemethod = new.instancemethod
def _swig_setattr_nondynamic(self,class_type,name,value,static=1):
    if (name == "thisown"): return self.this.own(value)
    if (name == "this"):
        if type(value).__name__ == 'PySwigObject':
            self.__dict__[name] = value
            return
    method = class_type.__swig_setmethods__.get(name,None)
    if method: return method(self,value)
    if (not static) or hasattr(self,name):
        self.__dict__[name] = value
    else:
        raise AttributeError("You cannot add attributes to %s" % self)

def _swig_setattr(self,class_type,name,value):
    return _swig_setattr_nondynamic(self,class_type,name,value,0)

def _swig_getattr(self,class_type,name):
    if (name == "thisown"): return self.this.own()
    method = class_type.__swig_getmethods__.get(name,None)
    if method: return method(self)
    raise AttributeError,name

def _swig_repr(self):
    try: strthis = "proxy of " + self.this.__repr__()
    except: strthis = ""
    return "<%s.%s; %s >" % (self.__class__.__module__, self.__class__.__name__, strthis,)

import types
try:
    _object = types.ObjectType
    _newclass = 1
except AttributeError:
    class _object : pass
    _newclass = 0
del types


print_args = _macadam.print_args
Do_Metro = _macadam.Do_Metro
Echo = _macadam.Echo
mydebug = _macadam.mydebug
get_ra = _macadam.get_ra
get_sn = _macadam.get_sn
get_rc = _macadam.get_rc
get_rt = _macadam.get_rt
get_ir = _macadam.get_ir
get_sf = _macadam.get_sf
get_fv = _macadam.get_fv
get_fc = _macadam.get_fc
get_fa = _macadam.get_fa
get_g = _macadam.get_g
get_bb = _macadam.get_bb
get_fp = _macadam.get_fp
get_echec = _macadam.get_echec


