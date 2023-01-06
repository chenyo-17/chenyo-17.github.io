---
title: "Configure Pyenv and Pipenv with Emacs"
date: 2023-01-06 16:00:00 +0200
categories: os 
tags: python venv pipenv pyenv
math: true
---

### Why no longer Conda

I used miniconda to manage python virtual environment for several months, but I feel:

1. Conda does not have a nice integration in Doom Emacs

2. Conda is slow to manage packages


### How to install Pipenv

Pipenv is another tool to create virtual environment with Pipfile.
It also allows me to specify python version when creating the environment, but there are several things to be careful about during the installation, especially when integrating it with Doom Emacs.
Here are the installation steps on Ubuntu 20.04:

1. Install pipenv with the system pip `pip install --user pipenv`

2. Install pyenv with `curl https://pyenv.run | bash` and add the following lines to `~/.bashrc` and don't forget `source ~/.bashrc` afterwards. 
Pyenv is used to install and manage different python versions.

```bash
export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
```

3. Install the python version with `pyenv install 3.10.9` **before** creating the environment.
**Don't** use `pyvenv --python 3.10` because if there is issues with the python installation, the error won't be promted.
For instance, when I used the command `pyvenv --python 3.10` to create an environment and activate it with `pipenv shell` in the project directory, I could not install any package with `pipenv install` due to some SSL certificate error (and I can't use `pip install` neither in the environment because my system python version is different).
It took me **quite a while** to realize that the problem might occur in my python installation (as the package installation was only successful in the system environment).
It was only when I uninstalled python and reinstalled with `pyenv install`, I was reported the missing library error, and after installing the related libraries, the problem was resolved.

4. Create the project directory.

5. Inside the project directory, run `pipenv --python 3.10`, this will create the specified environment and `Pipfile` in the directory. 
Then run `pipenv shell` to activate the environment.

6. To install any package, use `pipenv install <package>`.
Note that when there was an installation failure reporting `Failed building wheel for ...`, try `pipenv install setuptools` and `pipenv install wheel` first, this solved my problem.

### Integration with Doom Emacs

I am still very very new to Emacs so I don't know how to write config files to improve my workflow.
But with the Pipfile listed the lanauge server should be able to recognize the python file, otherwise can use `pipenv-python` to specify the python version.

An important thing to note is the language server should be also installed in the environment with `pipenv install python-lsp-server`, otherwise emacs cannot resolve the installed package import.

