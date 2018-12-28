# collapd

`collapd` "collapses" several ldap directories from different servers into one
virtual directory, thereby allowing application that can only authenticate
against a single ldap directory to be used with several authentification
sources.

It acts as a read-only ldap directory, supporting only `search` and `bind`
commands. Bind requests are forwarded to the corresponding backend, no
passwords are stored locally.

We define the backends to collapse in a simple `.yaml` file. The ordering of
the hosts determines which one serves an entry in case of duplicates (early
beats late).

```yaml
# The base under which all virtual entries reside
collapsed_base: ou=collapsed,dc=subtree,dc=com
# Which attributes should be forwared to the collapsed basetree?
attributes_forward:
        - uid
        - gidNumber
        - uidNumber
        - cn
        - sn
# Which attributes should be appended to every entry to make them valid (e.g.
# objectClasses)?
# Should be a list of dictionaries (mimicking LDIF syntax).
attributes_append:
        - objectClass: posixAccount
# When duplicate entries occur, hosts that appear earlier in the list take
# priority.
backends:
    - base: ou=subtree,dc=host1,dc=com
      host: host1
      port: 389
      filter: (objectClass=posixAccount)
    - base: ou=another-subtree,dc=something-else,dc=de
      host: host2
      port: 389
    - base: ou=yet-another-subtree,ou=even-deeper,dc=foobar,dc=org
      host: host3
      port: 389
```

`collapd` is intended to be used as a SHELL backend (as described
[here](http://umich.edu/~dirsvcs/ldap/doc/guides/slapd/13.html)), but can be
used on the command line as well.

```
collapd - collapse several LDAP backends into a single tree

Usage: collapd [-V|--version] [-c|--config PATH] [COMMAND]
  Without any command given, run in Shell-backend mode. That is, read the given
  LDAP request from stdin and write the response to stdout.

Available options:
  -h,--help                Show this help text
  -V,--version             Show version
  -c,--config PATH         Explicit path to config yaml-file. Alternatively set
                           COLLAPD_CONF.

Available commands:
  list                     List all available entries
  bind                     Bind with given DN.

Run -h for each command to see additional command line options.
```

## As a `shell` backend
Drop the following lines into your `slapd.conf` to have `collapd` act as
unified backend:
```
# shell backend collapsing everything
database        shell
suffix          "<collapsed_base from collapd.yaml>"
bind            <path-to-collapd>
search          <path-to-collapd>
readonly        on
```

## `list`-ing from command line
```

Usage: collapd list [-f|--filter ARG] [-s|--scope ARG] [--original]
                    [--non-unique] [-a|--attribute ATTR]
  List all available entries

Available options:
  -f,--filter ARG          LDAP filter to apply to listed data.
  -s,--scope ARG           Scope for the ldap request (base|one|sub).
  --original               Return original entries.
  --non-unique             Do not make entries unique.
  -a,--attribute ATTR      Attributes to fetch.
  -h,--help                Show this help text
```

## `bind`-ing from command line
```

Usage: collapd bind --dn DN [-p|--password PW] [-r|--repeat]
  Bind with given DN.

Available options:
  --dn DN                  DN with which to bind.
  -p,--password PW         Password with which to bind. If not supplied it will
                           be asked for/read from StdIn.
  -r,--repeat              Upon failed login attempt, keep asking for new
                           passwords.
  -h,--help                Show this help text
```
