### User home environment for Bivio development

`home-env` is a collection of "dot-files" and executables for Unix
(Mac, Linux, and Cygwin). This home environment simplifies the use
of a rich development environment.

These files are policy rich so that we can all get on with our jobs. That's
how we do things at Bivio: _agree and automate_. If you don't agree,
don't use this module.

The maintenance of all the files is on GitHub so you can fix anything
at any time. For now, we are not installing these files globally, but
there is global config so these files undo values that conflict here and
any global aliases in bash.

These files work with Emacs 24+, Bash 3+, and Git 1.7+. There is some
attempt at backward compatibility, but it's better if you upgrade.

You'll have to search through the files to see what's available. No
time for detailed documentation now.

#### Installation

To install in your home directory:

```
curl -s -L https://raw.githubusercontent.com/biviosoftware/home-env/master/install.sh | bash
```

After installing, your old dot-files will me moved to `*.old`, and minimalistic
dot-files will be linked to `~/src/biviosoftware/home-env`.

#### Customizing

You can specify pre and post files:

* `~/.pre_bivio_bashrc` and `~/.post_bivio_bashrc`
* `~/emacs/pre-bivio-init.el` and `~/emacs/post-bivio-init.el`

These will be executed before and after the Bivio init files.

Sometimes tools will automatically modify your `~/.bashrc` or `~/.emacs`.
You'll need to migrate those changes to the "post" files.

#### Auto-updating

When you login to an interactive shell, your bashrc will check for
updates to `home-env` once a day.  If there are updates,
`_bivio_home_env_update` will pull them and re-read your bashrc. You'll get a message
like:

```bash
Updating: ~/src/biviosoftware/home-env
Sourcing: ~/.bashrc
```

You'll get the latest editions to `home-env` automatically.

#### Cygwin

You need to bring in: emacs, curl, diffutils, and git. Don't install the windows
git, because it has its own bash, and it doesn't behave nicely with SHELLOPTS or
/cygdrive/c.

#### pyenv

There are default `bivio_pyenv_2` and `bivio_pyenv_3` that change the
global pyenv environment to python 2.7.8 and 3.4.2, respectively. They will
also install the environments, if they are not already installed.

You can create a local pyenv virtualenv if there's a `requirements.txt` or
`setup.py` in the current directory and enter:

```bash
bivio_pyenv_local
```

This will use the global pyenv as the basis. It's likely that you should
just create a ~/src pyenv:

```bash
cd ~/src

cat > requirements.txt <<'EOF'
matplotlib
numpy
scipy
pykern
# PyQt
EOF

bivio_pyenv_2
bivio_pyenv_local
```
