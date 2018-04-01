# Dotfile

### Xcode
``` bash
$ xcode-select --install
```

### Homebrew
``` bash
$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
$ brew install python ansible
```

### Install
The following command executes ansible and install apps written in packages.yml.
``` bash
$ make install
```

### Put
``` bash
$ make put
```

### ssh
Add the following config to .ssh/config.
```
Host *
     AddKeysToAgent yes
     UseKeychain yes
     IdentityFile ~/.ssh/id_rsa
```

