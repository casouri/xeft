![Demo gif](./demo.gif)

# Usage

To use Xeft the note searching interface, install it and type `M-x
xeft RET` to bring up the panel. If the dynamic module isn’t compiled,
you are prompted to compile it. Refer to the next section for
prerequisites for compiling the module.

Once the xeft buffer is up, type the search phrase in the first line.
Press `C-n` and `C-p` to go through each file. You can preview a file
in another window by pressing `SPC` on a file, or click the file with
the mouse. Press `RET` to open the file in the current window.

Directory `xeft-directory` stores note files, directory
`xeft-database` stores the database. Xeft uses
`xeft-default-extension` to create new files, and it ignores files
with `xeft-ignore-extension`.

By default, Xeft only searches for first level files in
`xeft-directory`, to make it search recursively, set `xeft-recursive`
to t.

See the “xeft” customize group for more custom options and faces.

# building the dynamic module

To build the module, you need to have Xapian installed. On Mac, it can
be installed with macports by

```shell
sudo port install xapian-core
```

Then, build the module by

```shell
make PREFIX=/opt/local
```

Here `/opt/local` is the default prefix of macports, which is what I
used to install Xapian. Homebrew and Linux users probably can leave it
empty.

# notdeft

I owe many thanks to the author of notdeft. I don’t really know C++ or
Xapian, without reading his code I wouldn’t be able to write Xeft.

Also, if you want a more powerful searching experience, you will be
happier using notdeft instead.

# Xapian dynamic module

I wrote a xapian dynamic module that you can use too. Check it out at <https://github.com/casouri/xapian-lite>.
