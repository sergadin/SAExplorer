# **Scientific area explorer**
[![Build Status](https://travis-ci.org/sergadin/SAExplorer.svg?branch=master)](https://travis-ci.org/sergadin/SAExplorer)
[![Coverage Status](https://coveralls.io/repos/github/sergadin/SAExplorer/badge.svg?branch=master)](https://coveralls.io/github/sergadin/SAExplorer?branch=master)

Description
===========
Lorem ipsum

How to start
============

The system is written in Common LISP. In order to run it locally, you
should setup a working LISP environment and install required system
packages.

Setting up Common LISP
----------------------

There are many Common LISP systems available, both open-source, such
as SBCL, ClozureCL, or CLISP, and comercial, e.g. LispWorks, Allegro
CL. In short, you need a LISP compiler and quicklisp (quicklisp.org)
to be properly configured. Every modern implementation of Common LISP
should work. The following commands give a clue how this can be done
for ClozureCL under Linux. The procedure is essentially the same for
Windows and MacOS operating systems (see https://ccl.clozure.com/ and
https://www.quicklisp.org/ for more information).

```bash
wget https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-linuxx86.tar.gz
tar xzf ccl-1.11.5-linuxx86.tar.gz
wget https://beta.quicklisp.org/quicklisp.lisp
```

Start ClozureCL
```bash
ccl --load quicklisp.lisp
```

and then, when the ClosureCL prompt symbol * (asterisk) appears, enter
the following commands, one by one:

```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

File `quicklisp.lisp` may be deleted from the current directory.

Install required packages
-------------------------

SAExplorer depends on SQLite, Redis and ElasticSearch, which are used
to store downloaded documents locally. Please, install these systems
before running SAExplorer. Redis and ElasticSearch may be installed on
separate servers accessable via network.


Running SAExplorer
------------------

Once you have successfully installed a Common LISP enviroment of your
chioce, you are ready to run SAExplorer!


Load the system by evaluating `(ql:quickload "saexplorer")`

or `(asdf:operate 'asdf:load-op :saexplorer)`.

Then run "the main" function
  `(saexplorer:main)`

and open
http://localhost:8135/ in your favorite browser that supports WebSockets.


Configuration
=============

Configuration file allows to set up local storage (caches and
databases), web server parameters, and access credentials for
bibliographic databases.

Please, use `sample.local.cfg` as a template for your local configuration.

Bibliographic databases
-----------------------

The default configuration supports open-access resources only. At the
moment, the list of supported open-access resources includes
Crossref.org and DBLP.org. If you need an access to databases with
restricted access, relevant authentication information must be
specified in the configuration file.

Proxy server
------------

Bibliographic databases may be accessed via a proxy server.

Local storage
-------------

Webserver
---------


Information for developers
==========================


How to run tests
----------------

(asdf:operate 'asdf:test-op :saexplorer)


### External (non-quicklisp) dependencies

https://github.com/sergadin/dbd-oracle
