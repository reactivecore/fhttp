fhttp, functional Http-Api definition in Scala
==============================================

`fhttp` is a little proof of concept, whether it's possible to define a HTTP-API in a functional manner
without binding directly to a framework.

From the definition it should be possible to generate Server and Client implementations without that much boilerplate.

The definition should not have any dependencies to the Server/Client library.

Current State
-------------

This is a proof of concept. Although it works, expect bugs everywhere. There are no published packages.


How it works
------------

Each `ApiCall` instance is made of various transitions, stored in a full typed shapeless `HList`.

The Framework Implementation (at the moment only Akka HTTP) 
lifts this list into an Encoder / Decoder for HTTP-Requests and their response.
 

Parts
-----

- `core` Definition system
- `akka` Implementation for Akka HTTP
- `example` A simple Example

Features
--------

- Support for path adding/extracting
- Support for Circe JSON, Strings
- Support for async file upload and download
- Support Query Parameters.
- Elegant transport for errors in error responses.

TODO
----

- Testing
- Extending 