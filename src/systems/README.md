
Access to specific systems
==========================

Each file in this directory defines system-specific interface for a bibliography database.
A class and methods for generic functions defined in `bibsystem.lisp` should be defined.
The utility macro define-bibsystem emits all calss definitions.

```lisp
(define-bibsystem ("Springer")
    :accept-encodings '(:json "application/json"))
```

Methods that might be redefined for a system are:

* bibsys:rest-endpoint
* bibsys:rest-query-parameters
* bibsys:rest-query-headers
* bibsys:parse-response
