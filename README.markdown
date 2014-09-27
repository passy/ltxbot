# ltxbot

A Twitter bot rendering mentions to PNGs.

![Screenshot](media/screenshot.png)

## Config

Modify `ltxbot.example.conf` and enter your [Twiter OAuth
credentials](https://apps.twitter.com/) for an R/W application.
You also need to provide a user token and secret for an authenticated account.
An easy way to obtain these is through
[`twurl authenticate`](https://github.com/twitter/twurl).

## Setup

You need a reasonably recent Haskell installation (GHC 7.8.3 at the time of this
writing) and [Docker](https://docker.com) on your system.

*Docker preparation*

```bash
$ # Pull my docker image with texlive-full and poppler-utils
$ docker pull passy/texlive-poppler
```


If you poor soul are on OS X, you need to set up a shared network drive
with boot2docker:

```bash
$ # Make a volume container (only need to do this once)
$ docker run -v /data --name my-data busybox true
$ # Share it using Samba (Windows file sharing)
$ docker run --rm -v /usr/local/bin/docker:/docker -v /var/run/docker.sock:/docker.sock svendowideit/samba my-data
$ # then find out the IP address of your Boot2Docker host
$ boot2docker ip
192.168.59.103
```

And connect to it through Finder by using `cifs://192.168.59.103/data`.

You may need to adjust the two `*tex2png.sh` scripts if any of your paths or
volume names differ.

## Provisioning

There's also a set of [Ansible](http://ansible.com) scripts available unter
[`ansible/`](ansible/) in this repository that can be used to set up a server.

## Building

```bash
$ cabal sandbox init
$ cabal install -j --only-dependencies
$ cabal run -- ltxbot.conf
```
