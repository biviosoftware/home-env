### User home environment for Bivio development

Install in your home directory:

```
curl -s -L https://raw.githubusercontent.com/biviosoftware/home-env/master/install.sh | bash
```

#### Cygwin

You need to bring in: emacs, curl, diffutils, and git. Don't install the windows
git, because it has its own bash, and it doesn't behave nicely with SHELLOPTS or
/cygdrive/c.

#### pyenv

[pyenv](https://github.com/yyuu/pyenv) is tightly integrated into the environment.
`$PROMPT_COMMAND` is set if pyenv is installed.

There are default `bivio_pyenv_2` and `bivio_pyenv_3` that change the
global pyenv environment to python 2.7.8 and 3.4.2, respectively. They will
also install the environments, if they are not already installed.

To create a local virtualenv, you go to the project directory and run
`bivio_pyenv_local`, e.g.

```
cd ~/src/biviosoftware/pybivio
bivio_pyenv_local
```

If there isn't a `.python-version` in the directory one will be created using
the global python version.

Every project directory should have a `.python-version`. You can force
a particular python environment by using ``pyenv activate <name>``, e.g.

```
pyenv activate pybivio
```

To switch back to the global environment, use `pyenv deactivate`.

