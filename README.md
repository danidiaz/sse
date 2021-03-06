# SSE problem

**UPDATE:** Actually, there wasn't any problem. I just misunderstood how server-sent events worked and my code was wrong. See [this link](https://simonprickett.dev/a-look-at-server-sent-events/) for a good overview. Notice in particular that properly closing the connection from the server side involves sending a special message (not defined by the protocol itself) that the client must recognize and expect.

A repo for reproducing a problem (?) with wai-extra server-sent events.

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

In the page, open the Chrome Dev Tools and look at the console. In a few
seconds, errors should start to appear.

Like this one:

![errormsg](console.PNG):

    (index):11 Event {isTrusted: true, type: "error", target: EventSource, currentTarget: EventSource, eventPhase: 2, …}

