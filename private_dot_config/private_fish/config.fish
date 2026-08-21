# Homebrew
if type -q /opt/homebrew/bin/brew
    eval "$(/opt/homebrew/bin/brew shellenv fish)"
end

set -g fish_greeting

set -gx ARCHFLAGS "-arch $(uname -m)"

# Editor
set -gx EDITOR "nvim"

if status is-interactive
    # Commands to run in interactive sessions can go here
    if type -q bat
        # Use syntax highlight for `cat`:
         alias cat "bat --paging never --decorations never --plain"
    end

    if type -q eza
       alias l   "eza -lo --group-directories-first --no-permissions --no-user --no-time --icons=always"
       alias ll  "eza -lga --group-directories-first"
       alias lt  "eza --tree --level=2 --git-ignore --ignore-glob='.git|*.log'"
       alias llt "eza --all --tree --level=3 --ignore-glob='.git|*.log'"
    end

    if type -q fzf
       fzf --fish | source
       set -gx FZF_CTRL_T_OPTS "--walker-skip .git,node_modules,target --preview 'bat -n --color=always {}' --bind 'ctrl-/:change-preview-window(down|hidden|)'"
       set -gx FZF_ALT_C_OPTS "--walker-skip .git,node_modules,target --preview 'eza --all --tree --level=2 {}'"
    end

    if type -q zoxide
       zoxide init fish | source
    end

    if type -q git
        alias gst   "git status"

        # `git log` with patches shown with difftastic.
        alias glg "git log -p --ext-diff"

        # Show the most recent commit with difftastic.
        alias gsh "git show --ext-diff"
    end
end

if type -q mise
    if status is-interactive
        mise activate fish | source
    else
        mise activate fish --shims | source
    end
end

if type -q direnv
    direnv hook fish | source
end

if type -q golangci-lint
    golangci-lint completion fish | source
end

if type -q starship
    starship init fish | source
end

# Disable OpenSpec telemetry
set -gx OPENSPEC_TELEMETRY 0
set -gx DO_NOT_TRACK 1
