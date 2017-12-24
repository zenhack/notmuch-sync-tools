Tools for synchronizing the tags in [notmuch][1] database.

Right now this just contains nmt.hs, a 3-way merge tool for the output
of [notmuch-dump(1)][2]. It is suitable for use as a merge driver in
git.

However, `notmuch-dump`/`notmuch-restore` on the full tags database is
very slow when the database is large, so I plan to add tooling for
keeping things in sync while only having to extract recent changes to
the tags database. This is still TODO.

N.B. This is basically a re-write of [notmuch-sync][3], which at the
moment is at about the same level of functionality (though this one is a
bit faster). It's hard to get me to write Python for free these days.

[1]: https://notmuchmail.org/
[2]: https://notmuchmail.org/manpages/notmuch-dump-1/
[3]: [https://github.com/zenhack/notmuch-sync
