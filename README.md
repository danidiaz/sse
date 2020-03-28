# SSE problem

A repo with an exmaple for wai-extra server-sent events.

The example uses cabal-install >= 3.0 (check the cabal.freeze file for the
exact versions of the packages).

The example also requires the warp executable from wai-app-static.

The set-up is as follows: there are two concurrent threads, one writes events
in a channel and the other runs a warp server and publishes them as server-sent
events.

# How to run

In a console, run:

    warp -p 3001

to publish the index page.

In another console, run:

     cabal run --allow-newer

and wait until the server builds and starts.

Then, in the browser, go to

    http://localhost:3001

# See also

[a look at server-sent events](https://simonprickett.dev/a-look-at-server-sent-events/)


