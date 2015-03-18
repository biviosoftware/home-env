### User home environment for Bivio development

Install in your home directory:

```
curl -s -L https://raw.githubusercontent.com/biviosoftware/home-env/master/install.sh | bash
```

To setup python:

```
b_install_pyenv 3
```

Will install the latest version of python 3 in a pyenv and virtualenv.

#### Cygwin

You need to bring in: emacs, curl, diffutils, and git. Don't install the windows
git, because it has its own bash, and it doesn't behave nicely with SHELLOPTS or
/cygdrive/c.
