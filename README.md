# RSS2IMAP

RSS2IMAP is a utility which pulls RSS or Atom feeds
and adds them as e-mails to "RSS" folder at your IMAP server.


## Usage

Basically, you just launch rss2imap and that's it.
RSS2IMAP connects to the IMAP server specified in the configuration file,
iteratively reads the feeds from the list of feeds, creates e-mails for
unread items and appends them to "RSS" folder at the IMAP server.

The utility needs 3 files in your `~/.rss2imap` directory
in order to work. These files are:

* Main configuration file `config.yml`
  You can use the following sample:

```
    ---
      server: imap.gmail.com
      port: 993
      username: nobody@gmail.com
      password: secret
      email-to: nobody@gmail.com
      email-from: rss2imap@localhost
```

  The options are pretty self-explanatory.
  RSS2IMAP uses `email-to` and `email-from` when generating e-mails
  that are appended to "RSS" folder at your IMAP server.

* List of feed URLs `feeds.txt`.
  This is just a simple text file containing URLs of the RSS/Atom
  feeds that you want to pull the data from. RSS2IMAP is not fool-proof,
  so don't put rubbish there. Please.
  There should probably be an option to use a popular [OPML]
  file format for this too, so feel free to contribute.

* List of read items `read.txt`.
  You don't normally need to touch this file. It contains a list of read
  items which helps RSS2IMAP to determine whether it should add a feed item
  to your IMAP folder.


## Authors

* [Slava Kravchenko](https://github.com/cordawyn)


## Thanks

* [Max Gonzih](https://github.com/Gonzih) for the original
  [feeds2imap.rb](https://github.com/Gonzih/feeds2imap.rb)
  implementation.

See [feeds2gmail](http://www.greghendershott.com/2013/05/feeds2gmail.html)
for detailed info and references.

[OPML]: https://en.wikipedia.org/wiki/OPML
