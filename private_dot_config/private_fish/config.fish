set -g fish_greeting

# $PATH:
if type -q /opt/homebrew/bin/brew
    fish_add_path -g /opt/homebrew/bin
end
if type -q /usr/local/bin/brew
    fish_add_path -g /usr/local/bin
end

set -l BREW_PREFIX "$(brew --prefix)"

set -gx TERM "xterm-256color"
set -gx LANG "en_US.UTF-8"
set -gx LC_COLLATE "en_US.UTF-8"
set -gx LC_TYPE "en_US.UTF-8"

# ARCHFLAGS = "-arch arm64" or "-arch x86_64"
set -gx ARCHFLAGS "-arch $(uname -m)"

set -gxa PKG_CONFIG_PATH "$BREW_PREFIX/opt/zlib/lib/pkgconfig"
set -gxa PKG_CONFIG_PATH "$BREW_PREFIX/opt/ncurses/lib/pkgconfig"
set -gxa PKG_CONFIG_PATH "$BREW_PREFIX/opt/readline/lib/pkgconfig"
set -gxa PKG_CONFIG_PATH "$BREW_PREFIX/opt/sqlite/lib/pkgconfig"
set -gxa PKG_CONFIG_PATH "$BREW_PREFIX/opt/openssl/lib/pkgconfig"
set -gxa PKG_CONFIG_PATH "$BREW_PREFIX/opt/tcl-tk/lib/pkgconfig"

set -gxa LDFLAGS "$(pkg-config zlib --libs)"
set -gxa LDFLAGS "$(pkg-config tk --libs)"
set -gxa LDFLAGS "$(pkg-config ncurses --libs)"
set -gxa LDFLAGS "$(pkg-config readline --libs)"
set -gxa LDFLAGS "$(pkg-config openssl --libs)"
set -gxa LDFLAGS "-L$(brew --prefix bzip2)/lib"
set -gxa LDFLAGS "-L$(brew --prefix sqlite)/lib"

set -gxa CPPFLAGS "$(pkg-config zlib --cflags)"
set -gxa CPPFLAGS "$(pkg-config tk --cflags)"
set -gxa CPPFLAGS "$(pkg-config readline --cflags)"
set -gxa CPPFLAGS "$(pkg-config openssl --cflags)"
set -gxa CPPFLAGS "-I$(brew --prefix bzip2)/include"
set -gxa CPPFLAGS "-I$(brew --prefix sqlite)/include"

# TCL and TK for Python
# fish_add_path "$BREW_PREFIX/opt/tcl-tk/bin"
# PYTHON_CONFIGURE_OPTS="--with-tcltk-includes='$(pkg-config tk --cflags)' --with-tcltk-libs='$(pkg-config tk --libs)'"

# Ruby enable YJIT
set -gx RUBY_CONFIGURE_OPTS --enable-yjit

# Editor
set -gx EDITOR "vi"

if type -q emacs
   alias e "emacs -Q -nw -l '~/.config/emacs/nano.el'"
   set -gx EDITOR "e"
end

if status is-interactive
    # Commands to run in interactive sessions can go here
    if type -q bat
        # Use syntax highlight for `cat`:
         alias cat "bat --paging never --decorations never --plain"
    end

    if type -q eza
       alias l   "eza -lg"
       alias ll  "eza -lga --group-directories-first"
       alias lt  "eza --tree --level=3 --git-ignore --ignore-glob='.git|*.log'"
       alias llt "eza --all --tree --level=3 --ignore-glob='.git|*.log'"
    end

    if type -q fzf
       fzf --fish | source
    end

    if type -q zoxide
       zoxide init fish | source
    end
end

function y
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
       builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
end

# alias go-vpn "ssh -t vpn-work 'tmux -CC new -A -s main'"

if type -q git
    alias gst   "git status"

    # `git log` with patches shown with difftastic.
    alias glg "git log -p --ext-diff"

    # Show the most recent commit with difftastic.
    alias gsh "git show --ext-diff"
end

if type -q mise
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end
end

# if type -q direnv
#     direnv hook fish | source
# end

if type -q starship
    starship init fish | source
end

# Added by LM Studio CLI (lms)
if type -q /Users/misha/.lmstudio/bin
    set -gx PATH $PATH /Users/misha/.lmstudio/bin
end
