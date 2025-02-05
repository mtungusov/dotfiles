# Homebrew
https://brew.sh

 ``` shell
xcode-select --install
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

``` shell
brew install git
git clone git@github.com:mtungusov/dotfiles.git ~/projects/dotfiles
```

# Dotfiles Install

## Chezmoi dotfiles manager
https://www.chezmoi.io/

```shell
brew install chezmoi
chezmoi init --apply $GITHUB_USERNAME
```

# Software CLI

- git
- [delta](https://github.com/dandavison/delta)
- [difftastic](https://difftastic.wilfred.me.uk/)
- tmux
- fish
- starship [Cross-shell prompt for astronauts](https://starship.rs)
- eza [improved ls](https://github.com/eza-community/eza)
- coreutils (gnu ls)
- ripgrep [RipGrep](https://github.com/BurntSushi/ripgrep)
- xh [замена http]
- bat [Like Cat]
- fd [FD - fast alternative for find](https://github.com/sharkdp/fd)
- fzf [FZF: Command-line fuzzy finder](https://github.com/junegunn/fzf)
- zoxide [zoxide: smarter cd command](https://github.com/ajeetdsouza/zoxide)
- yazi [Blazing fast terminal file manager](https://yazi-rs.github.io/)
- macmon [Sudoless performance monitoring CLI tool for Apple Silicon processors](https://github.com/vladkens/macmon)

- clifm [File Manager](https://github.com/leo-arch/clifm)
- zellij [Zellij is a terminal workspace](https://zellij.dev/)

```
brew install git git-delta difftastic tmux fish starship eza coreutils ripgrep xh bat fd fzf zoxide yazi vladkens/tap/macmon
```

## Fish

```
echo $(which fish) | sudo tee -a /etc/shells
chsh -s $(which fish)
reboot!
```

## Sublime Merge
https://www.sublimemerge.com/

## MonitorControl
https://github.com/MonitorControl/MonitorControl

```
# brew install --cask monitorcontrol
brew install MonitorControl
```

# Fonts

## Input Fonts
https://input.djr.com/

# Editors

## Sublime Text
https://www.sublimetext.com/

## Zed
https://zed.dev/

## VSCodium
https://vscodium.com/

## NeoVim

```shell
# brew install neovim
```

## Emacs
https://github.com/d12frosted/homebrew-emacs-plus

```shell
brew install autoconf automake bzip2 cmake dark-mode gcc git giflib gnutls jansson jpeg libgccjit libtiff libvterm libxml2 pkg-config texinfo

brew install emacs-plus@30 --with-xwidgets --with-imagemagick --with-native-comp --with-modern-orange-icon

osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
```

# Apps

## KeepassXC
https://keepassxc.org/

## Anytype
https://anytype.io/

## Obsidian
https://obsidian.md/

## Raycast
https://www.raycast.com/


## WezTerm

```shell
brew install --cask wezterm
```

## Web Browsers

```shell
brew install --cask firefox
brew install --cask tor-browser
brew install --cask eloston-chromium
brew install --cask brave-browser
brew install --cask vivaldi
```

# Developer Tools

## Docker Like Tools

### OrbStack
https://orbstack.dev/

``` shell
brew install orbstack
```

#### docker cli

```
brew install docker
```

## direnv
https://direnv.net/

```
brew install direnv
```

## pkg-config

Manage compile and link flags for libraries

```
brew install pkg-config
```

## MISE
https://mise.jdx.dev/

### Install

```shell
brew install mise
mise ls
```

### Config

change ~/.config/fish/config.fish

```shell
mise direnv activate > ~/.config/direnv/lib/use_mise.sh
```

See available versions with (java example):
```shell
mise ls-remote java
```

See latest version:
```shell
mise latest java
```

### rust

```shell
mise latest rust
mise ls-remote rust
mise use -g --pin rust@1
```

### ruby

https://mise.jdx.dev/lang/ruby.html

```shell
brew install openssl@3 readline libyaml gmp
mise latest ruby
mise use -g --pin ruby@3.3
ruby --yjit -v
```

### python

https://mise.jdx.dev/lang/python.html

```shell
brew install openssl readline sqlite3 xz zlib tcl-tk
mise latest python
mise use -g --pin python@3.12.1
mise resim
python -m tkinter -c "tkinter._text()"
```

### java

https://mise.jdx.dev/lang/java.html

```shell
mise latest java@temurin-21
mise use -g --pin java@temurin-21.0.6+7.0.LTS
mise reshim
```


### clojure

cp deps.edn from https://github.com/practicalli/clojure-cli-config/tree/main

```shell
mise latest clojure
mise use -g --pin clojure@1.12.0.1501
mise reshim
```
