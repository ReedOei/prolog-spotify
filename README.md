# Overview

A (SWI) Prolog library for interacting with the Spotify API.
This primarily exists to allow me to export playlists from my account, but it should be easy to expand to do more.

Currently basically just a nice wrapper around `curl`.

# Usage

## Interactive

You need to create a `secrets.pl` file that looks like this in the same directory as `main.pl`:

```prolog
:- module(secrets, [client_id/1, client_secret/1]).

client_id('YOUR_CLIENT_ID').
client_secret('YOUR_CLIENT_SECRET').
```

Once you have created this file, the library will handle all of the authorization stuff for you.
You can now simply load `spotify.pl` (e.g., via `prolog main.pl`) and run the following examples.

To export to a CSV, use the following.
```prolog
?- playlist_to_csv('kytuzian', _-'Spacey Music', 'test.csv').
true.
```

You can replace 'kytuzian' and 'Spacey Music' with your user id and playlist name.
Alternatively, instead of the `_`, you can specify the playlist id.

```prolog
?- findall(Track, playlist_track_info('kytuzian', _-'Spacey Music', Track), Tracks).
Tracks = ['08-Flawless Theory'-['Amplitude Studios'], '22-Bleu Argon'-['FlyByNo'], 'Ascolta'-['Ludovico Einaudi'], 'Sigur 3 (Untitled)'-['Sigur Rós'], 'Individuation'-['Eluvium'], 'Perfect Neglect In A Field Of Statues'-['Eluvium'], 'Strangeworks'-['Eluvium'], 'Fugue State'-[...], ... - ...|...].
```

If you want to do a low level query (e.g., to some specific endpoint), you can do:

```prolog
?- run_curl([endpoint('users/kytuzian')], Response).
Response = json([display_name='Reed Oei', external_urls=json([spotify='https://open.spotify.com/user/kytuzian']), followers=json([href= @(null), total=6]), href='https://api.spotify.com/v1/users/kytuzian', id=kytuzian, images=[json(...)], type=user, ... = ...]).
```

If the endpoint you are querying returns a list of items, you can use `retrieve_all/2` or `retrieve_all/4`, and the library will handle the pagination for you, unifying `Result` with one item at a time, and making a new request when the next page is needed.

```prolog
?- retrieve_all([endpoint('users/kytuzian/playlists')], Item).
Item = json([collaborative= @(false), external_urls=json([spotify='https://open.spotify.com/playlist/299scdzrJvmXJuD1QZNaGi']), href='https://api.spotify.com/v1/playlists/299scdzrJvmXJuD1QZNaGi', id='299scdzrJvmXJuD1QZNaGi', images=[json([...|...])], name='Dark Morph – Dark Morph', owner=json(...), ... = ...|...]) .
```

The JSON will be returned in the format specified by the [JSON library](https://www.swi-prolog.org/pldoc/man?section=jsonsupport).

## Library

If you wish to use this as a library, you will still need a secrets file containing `client_id/1` and `client_secret/1`.
Naturally, this can be set up however you wish.
The rest of the usage is the same.

