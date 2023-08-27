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
    export PATH="$HOME/.local/bin/:$PATH"
    hs-notifications

`Mod4-Shift-Ctrl-w` removes all the notifications. `Mod4-Ctrl-w` removes the
oldest notification. You can also click them.

You can change the colors & keybindings by creating a
`~/.config/hs-notifications/config.ini`, see the `config-example.ini` for
available options.

My plan is to import this into my xmond config, build my notification Config in
there, & use `forkProcess` to run the application.

I've built this to my needs - if you would like to use this but want some more
features or customizability, feel free to open an issue.


## TODO

* Add icon support
* Add ability to override colors & icon - matching by app name or title/body
  text. Specify overrides in config file sections.
* Add action support?
* Handle monitor change signals - reset notification positions
* Refactor to `ReaderT Env IO`

    * Add `data Env = Env Config AppState`
    * Use TQueue for Queues instead of wrapping all of `AppState` in a TVar.
    * Write typeclasses for accessing & modifying small parts of the Env.
      E.g., `class Monad m => RootPosition m where getRootPosition :: m (Int32, Int32)`
    * Refactor function types to use typeclasses to restrict their scope -
      instead of passing the entire Config & AppState around & using the IO
      monad everywhere.


## Architecture

This was hacked together in a day or so, there's lots of room for
improvement.

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

You can click notifications to remove them, press `Mod4-Ctrl-w` to remove
the oldest, or press `Mod4-Ctrl-Shift-w` to remove them all.

Removing a notification updates the window list, re-adjusts the positions of
windows below it, & adds its `NotificationID` to the removal queue.

The DBus client wakes up every second to check the removal queue & send the
`NotificationClosed` signal for any removed notifications.


## License

GPL-3.0
