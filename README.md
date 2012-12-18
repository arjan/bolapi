Erlang bindings for the BOL API
===============================

See http://developers.bol.com/

Add the following to your application environment:

    [{bolapi,
     [
      {access_key, "FD8S09F8DS0F9S8D0"},
      {access_secret, FLDSJFLKSADJFLKDSA..."}
      ]
     }
    ].

Then you can use the functions in the `bolapi` module to access the
API. The most simple one is `bolapi:ping/0`, which should return
`pong` if authorization succeeded.
