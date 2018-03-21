# hs-notifications

A Super Simple Desktop Notification Server in Haskell.

![A Screenshot Showing the Notification Windows Created by hs-notifications](http://bugs.sleepanarchy.com/projects/hs-notifications/repository/revisions/master/entry/screenshot.png  "hs-notifications Screenshot")

`DBUS_SESSION_BUS_ADDRESS` needs to be set, which can be accomplished by adding
`export $(dbus-launch)` to your `~/.xinitrc`.


Install `stack` & `gobject-introspection`:

    sudo pacman -S stack gobject-introspection

Build:

    stack build

Run:

    stack exec hs-notifications

Install to `~/.local/bin`:

    stack install

`Mod4-Ctrl-w` removes a notification. You can also click them.

My eventual goal is to import this into my xmonad config & just run it with
`forkOS`.



## Architecture

This was hacked together in a day or so, so there's lots of room for
improvement, but it's something like this:

There's a DBus client & a GTK app. Main just initializes the shared state,
forks the DBus client and starts the GTk app.

### Init

The DBus client connects to the current dbus session, identifies itself as the
notification server, and sets up it's event handlers.

The GTK app initializes itself, gets the notification placement position,
attaches some CSS sytles, and sets up some keybindings & timers.

### New Notification

When the DBus client receives a notification, it adds it to the notification
queue in the shared state.

Every second, the GTK loop checks the notification queue for any new messages.
Any messages are rendered & added to the window list.

As windows are added, the application tracks the position of the next window by
adding the new window's height & some spacing.

### Removing Notifications

You can click notifications to remove them, or press `Mod4-Ctrl-w` to remove
the oldest.

Removing a notification updates the window list, re-adjusts the positions of
windows below it, & adds its `NotificationID` to the removal queue.

The DBus client wakes up every second to check the removal queue & send the
`NotificationClosed` signal for any removed notifications.


## License

GPL-3.0
