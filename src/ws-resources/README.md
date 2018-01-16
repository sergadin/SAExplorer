# **Description of the web sockets protocol**

The server supports several REST *resources* destinguished by the
prefix of request's URI. A typical interaction between a client and
the server consists of the following messages. First, the Client
submit her *requests* to a resource. Server replies with zero or more
*notification* messages, and evetually sends either a *response*
message, or an error message. All messages between the client and the
server are JSON-encoded strings.

# Message structure

 There are four kinds of messages:

* request (from a client),
* response (successful),
* error message, and
* notification.

## Requests from a client
```javascript
{
  operation: "operation name, e.g. 'find'",
  data: {
    // opearation-specific parameters, see below
  }
}
```

## Messages from the server
Responses for successful operations:

```javascript
{
  operation: "operation name from the request",
  category: "result",
  result: {
    // operation-specific values
  }
}
```

Error messages:
```javascript
{
  operation: "operation name from the request",
  category: "error",
  message: "Error message"
}
```

### Notifications

Notifications inform the client on specific conditions related to the
current request, e.g. progress data, or information about recoverable
error.

```javascript
{
  operation: "operation name from the request",
  category: "notification",
  type: "type of the notification, e.g. 'progress', 'info'",
  data: {
    // type-specific values
  }
}
```

# Available resources and their operations

The server supports several *resources* that are destinguished by prefixes in URI request.

* `/conference/` -- operations related to conferences.
* `/keywords/` -- everything related to keywords analysis.
* `/persons/` -- authors.

## Conferences

Conference resource `/conference/` supports following operations
(values of operation field of the request):

* `find` -- search for conferences by some criteria, such as title, or keywords;
* `describe` -- information about given conference;
* `similar` -- list of conferences that are similar to the given one;
* `impact` -- compute impact value of a conference.

### find

Request:
```javascript
data: {
  keywords: ["keyword1", "keyword2", ...],
  title: "string"
}
```

Response:
```javascript
result: {
  entries: [
    {
      title: "Conference title",
      acronym: "Acronym",        // optional
    },
    // ... other records
  ]
}
```

### describe -- retrieve information about the conference
Request:
```javascript
data: {
  categories: ["category1", "category2", ...]
}
```

Supported categories are: `frequent_authors`, `years`.


[//]: #
[//]: # (-------------------------- Examples ----------------------------)
[//]: #

# Example

A client submits the following request to conference resource at `http://localhost:12345/conference/`:
```javascript
{
  operation: "find",
  data: {
    keywords: "constraint solving",
    title: "symposium"
  }
}
```

to find symposiums related to *constraint solving*, and gets
```javascript
{
  operation: "find",
  category: "result",
  result: {
    entries: [
     { title: "International Computer Science Symposium in Russia", acronym: "CSR" },
     { title: "International Symposium on Logic-based Program Synthesis and Transformation" },
     { title: "International Symposium on Functional and Logic Programming", acronym: "FLOPS" }
    ]
  }
}
```

[//]: #
[//]: # (-------------------------- Implementation ----------------------------)
[//]: #

# Implementation
