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
