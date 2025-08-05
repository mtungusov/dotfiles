# Homebrew
https://brew.sh

 ``` shell
xcode-select --install
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

# Dotfiles Install

## Chezmoi dotfiles manager
https://www.chezmoi.io/

```shell
brew install git chezmoi
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
brew install git git-delta difftastic tmux fish starship eza coreutils ripgrep xh bat fd fzf zoxide yazi
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
brew install vladkens/tap/macmon
```

## Menubar widget
```
brew install vladkens/tap/macmon
```

# Fonts

## Input Fonts
https://input.djr.com/

## font-symbols-only-nerd-font
```shell
brew install font-symbols-only-nerd-font
```

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

# emacs 30.1 --with-native-comp -- now by default
brew install emacs-plus@30 --with-xwidgets --with-imagemagick --with-modern-orange-icon

# run osascript to add Emacs.app into /Application folder
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

## Git

### LazyGit
https://github.com/jesseduffield/lazygit/

```shell
brew install lazygit
```

## DB

### LazySQL
https://github.com/jorgerojas26/lazysql

```shell
brew install lazysql
```

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

!!!
! --- try not use with mise ---  !
!!!
```shell
# brew install direnv
```

## pkg-config

Manage compile and link flags for libraries

```
brew install pkg-config zlib ncurses
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
mise reshim
```

### go

```shell
mise latest go
mise ls-remote go
mise use -g --pin go@1.24
mise reshim
```

#### linter
```shell
curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/HEAD/install.sh | sh -s -- -b $(go env GOROOT)/bin v1.64.5

golangci-lint --version

## install LSP server for linter from url
## https://github.com/nametake/golangci-lint-langserver/releases/download/v0.0.10/golangci-lint-langserver_Darwin_arm64.tar.gz
## enable run on OSX
sudo xattr -dr com.apple.quarantine /Users/misha/.local/share/mise/installs/go/1.24.0/bin/golangci-lint-langserver

## install Zed extension
https://github.com/joakimen/zed_golangci_lint
```

### ruby

https://mise.jdx.dev/lang/ruby.html

```shell
brew install openssl@3 readline libyaml gmp
mise latest ruby
mise use -g --pin ruby@3.3
mise reshim
ruby --yjit -v
```

mise.toml
```toml
[env]
RUBY_VERSION = "3.3.7"
GEM_HOME = "{{ config_root }}/.rubyenv"
BUNDLE_BIN = "{{ config_root }}/.rubyenv/bin"
_.path = ["{{ config_root }}/.rubyenv/bin"]
```

### node

```shell
mise latest node
mise use -g --pin node@23.7
mise reshim
```

### python

https://mise.jdx.dev/lang/python.html
-or-
UV (https://docs.astral.sh/uv/) ?

```shell
brew install readline sqlite3 xz zlib tcl-tk
mise latest python
mise use -g --pin python@3.12.1
mise reshim
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
