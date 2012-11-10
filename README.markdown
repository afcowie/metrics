Gathering telemetry from systems, for rendering and analysis
============================================================

Work in progress, very preliminary.

Parsing
-------

So far, this code is only of interest because it parses collectd's "command"
format, as send by the write\_http plugin -- and that parser turned out to be a
good demonstration of how frustrating parsing can be in Haskell.

At the moment, there is an equivalent parser written for Attoparsec, Trifecta,
and Parsec.

If you want to build that, grab a branch and then

    $ make build-junk
    $ ./snippet

Right now it's rigged just to show the resultant objects from each of
the three libraries parsing the same test line of data; to switch to
benchmarking, just change the last line of src/Snippet.hs from

        demo

to

        profile

For what it's worth, I make no suggestion that this code is wonderful or
anything. I'm trying to learn, but having done my best to make sense of
the API documentation and conflicting types in each of the three
libraries, this is what I was able to came up with. I was actually
trying to get to the nirvana of "profile your parser so that you can
tune it", per Bryan, but to be honest when one has no idea what the hell
the attoparsec code is doing, changing anything is a bit like poking a
stick at a nest of pit vipers.

Which is why I really loved Trifeca (it helped me debug it) and now just
need to learn how to use it better [once you release a new version].

AfC

