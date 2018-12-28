
# INSTALL

Check if [this](https://github.com/supki/ldap-client/pull/14) has been merged.
If not (as is the case at the time of this writing), initialize the git
submodule containing it:
```
$ git submodule init
```

`collapd` can then be build with `stack` via:
```
$ stack build
$ sudo stack install --local-bin-path /usr/local/bin
```

# Static build
Unfortunately, stack does not support optional build targets, hence in order to
build static executables, you need to comment in all lines between
```
# BEGIN comment in for static build
...
# END comment in for static build
```
in `package.yaml` and rebuild.

