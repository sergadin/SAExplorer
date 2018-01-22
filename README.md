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
for ClozureCL under Linux. Note, that you can install ClozureCL in any
directory, or move `ccl` directory later. The procedure is essentially
the same for Windows and MacOS operating systems (see
https://ccl.clozure.com/ and https://www.quicklisp.org/ for more
information).

```bash
wget https://github.com/Clozure/ccl/releases/download/v1.11.5/ccl-1.11.5-linuxx86.tar.gz
tar xzf ccl-1.11.5-linuxx86.tar.gz
wget https://beta.quicklisp.org/quicklisp.lisp
```

Start ClozureCL (64bit version)
```bash
./ccl/lx86cl64 --load quicklisp.lisp
```

and then, when the ClosureCL prompt symbol ? (question) appears, enter
the following commands, one by one:

```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(quit)
```

File `quicklisp.lisp` may be deleted from the current directory.

At this point your HOME directory should contains a ClozureCL
initialization file `.ccl-init.lisp`. In order to simplify SAExplorer
loading, add the following line at the end of this file (we assume
that the repository was cloned into HOME directory; if it is not the
case, replace "~/SAExplorer" by an appropriate value):

```lisp
(push #p"~/SAExplorer/" asdf:*central-registry*)
```

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

Then run "the main" function
  `(saexplorer:main)`

and open
http://localhost:8135/index.html in your favorite browser that supports WebSockets.


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

LISP IDE
--------

The most popular development environment for Common LISP is Emacs/SLIME.
```bash
apt-get install emacs
```

While SLIME is available in many Linux distributions by default, it is
recommended to download it manually from the official page. https://github.com/slime/slime/archive/v2.20.tar.gz

SLIME archive may be decompressed whenever you like, while the common
location is the Emacs packages directory, say
`~/.emacs.d/slime-2.0/`. The final step is putting some configuraton
command into emacs dot-file `~/.emacs`.

```lisp
(add-to-list 'load-path "~/.emacs.d/slime-2.0/")  ; Inform Emacs where SLIME may be found

(require 'slime)

(add-hook 'lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-m") 'newline-and-indent)
            (slime-setup '(slime-fancy slime-repl))
            (slime-mode t)
            (add-hook 'lisp-mode-hook
                      (lambda () (add-to-list 'write-file-functions
                                              'delete-trailing-whitespace)))))

(eval-after-load "slime"
  `(progn
     (custom-set-variables
      '(inhibit-splash-screen t)
      '(slime-complete-symbol*-fancy t)
      '(slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
      '(slime-net-coding-system 'utf-8-unix)
      '(slime-startup-animation nil)
      '(slime-lisp-implementations '((sbcl ("/usr/local/bin/sbcl"))
                                     (ccl ("/opt/lisp/ccl-1.11/lx86cl64"))))
      '(inferior-lisp-program "/opt/lisp/ccl-1.11/lx86cl64") ; default Lisp system
      '(slime-default-lisp 'ccl t)
      '(safe-local-variable-values '((Base . 10) (Syntax . ANSI-Common-Lisp) (Encoding . utf-8))))
     (slime-setup '(slime-fancy slime-repl))))
```

If everythong was configured proprly, you can start Emacs editor and
run SLIME by entering Emacs command `M-x slime <RET>` (Alt+X slime
Enter).

The folowing descriptions of basic SLIME/Emacs commands might be useful:

* http://pchristensen.com/wp-content/uploads/2008/02/slimecommands.pdf
* http://dept-info.labri.fr/~idurand/enseignement/lst-info/PFS/Common/slime-refcard.pdf


How to run tests
----------------

```lisp
(ql:quickload "saexplorer-test")
(lift:run-tests :suite 'root :break-on-errors? t)
```

Or, using the ASDF testing facility

```lisp
(ql:quickload "saexplorer-test")
(asdf:test-system "saexplorer")
;; or (asdf:operate 'asdf:test-op :saexplorer)
```
