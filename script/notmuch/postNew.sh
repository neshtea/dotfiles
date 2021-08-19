#!/bin/bash
# Based On: https://gist.githubusercontent.com/frozencemetery/5042526/raw/57195ba748e336de80c27519fe66e428e5003ab8/post-new
# Install this by moving this file to <maildir>/.notmuch/hooks/post-new
# NOTE: you need to define your maildir in the vardiable nm_maildir (just a few lines below in this script)
# Also create empty files for:
# 1. thefeed.db (things you want to read every once in a while)
# 2. spam.db (things you never want to see)
# 3. screened.db (your inbox)
# 4. ledger.db (papertrail)
# in the hooks folder.
# More info about hooks: https://notmuchmail.org/manpages/notmuch-hooks-5/

echo "starting NM post-new script"
# NOTE: You will need to define your maildir here!
export nm_maildir="$HOME/Mail"
export start="-1"

function timer_start {
    echo -n "    starting $1"
    export start=$(date +"%s")
}

function timer_end {
    end=$(date +"%s")
    delta=$(($end-$start))
    mins=$(($delta / 60))
    secs=$(($delta - ($mins*60)))
    echo " -- $1 completed: ${mins} minutes, ${secs} seconds"
    export start="-1" # sanity requires this or similar
}

timer_start "ledger"
while IFS= read -r line; do
    nm_tag=$(echo "$line" | cut -d' ' -f1 -)
    nm_entry=$(echo "$line" | cut -d' ' -f2 -)
    if [ -n "$nm_entry" ]; then
        notmuch tag +archived +ledger/"$nm_tag" -inbox -- tag:inbox and tag:unread and from:"$nm_entry"
    fi
    # echo -n "Handling entry: $nm_tag, $nm_entry"
done < $nm_maildir/.notmuch/hooks/ledger.db
timer_end "ledger"

timer_start "unsubscribable_spam"
for entry in $(cat $nm_maildir/.notmuch/hooks/spam.db); do
    if [ -n "$entry" ]; then
        notmuch tag +spam +deleted +archived -inbox -unread -- tag:inbox and tag:unread and from:"$entry"
    fi
done
timer_end "unsubscribable_spam"

timer_start "thefeed"
for entry in $(cat $nm_maildir/.notmuch/hooks/thefeed.db); do
    if [ -n "$entry" ]; then
        notmuch tag +thefeed +archived -inbox -- tag:inbox and tag:unread and from:"$entry"
    fi
done
timer_end "thefeed"

timer_start "Screened"
for entry in $(cat $nm_maildir/.notmuch/hooks/screened.db); do
    if [ -n "$entry" ]; then
        notmuch tag +screened -- tag:inbox and tag:unread and from:"$entry"
    fi
done
timer_end "Screened"

echo "Completing NM post-new script; goodbye"
