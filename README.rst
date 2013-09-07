These are my dotfiles. There are many like them, but these ones are mine.

My dotfiles are my best friend. They are my life. I must master them as I must
master my life.

My dotfiles, without me, are useless. Without my dotfiles, I am useless.

Usage
=====

Procedure to update a new user account with these dotfiles:

1. Check out the repository to ~/dotfiles
2. cd ~/dotfiles
3. stow <application>

   This will automatically symlink whatever files live inside the application's
   directory into `~/`

   For example, in `~/dotfiles/emacs` we have `.emacs.d` Running the command
   `stow emacs` will create a symlink `~/.emacs.d` which points to
   `~/dotfiles/emacs/.emacs.d`.

Adding configuration files for a new app
----------------------------------------

See `this post
<http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html>`_
for more details on how to add configurations for applications that follow the
`XDG Base Directory Specification
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>`_.