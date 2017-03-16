#
#  @file .zshrc
#

# emacsライクな操作を行う
bindkey -e

# 自動補完有効
autoload -U compinit
compinit -u

# 色指定有効 
#  : ${fg[色指定]}と${reset_color}で色指定が可能になる
autoload -Uz colors
colors

# プロンプトの設定 
#  : /etc/zshrcで設定しているのでzshenvではなく
#  : zshrcで設定を行う
HOST=`hostname`
PROMPT="[${fg[yellow]}%n@%m${reset_color} ${fg[green]}%c${reset_color}]$ "

# Ctrl+rでヒストリーのインクリメンタルサーチ、Ctrl+sで逆順
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

# コマンドを途中まで入力後、historyから絞り込み
# 例 ls まで打ってCtrl+pでlsコマンドをさかのぼる、Ctrl+bで逆順
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^b" history-beginning-search-forward-end

## cdr
autoload -Uz add-zsh-hock
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook

#
#  setting for peco
#
function peco-history-selection() {
    BUFFER=`history -n 1 | tail -r  | awk '!a[$0]++' | peco`
    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N peco-history-selection
bindkey '^R' peco-history-selection


#
# setting for ls command
#
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

#
#  aliase
#

# for ls
alias ls="ls -FG" 
alias l="ls -t"
alias ll="ls -l"
alias la="ls -a"
alias lst="ls -ltr"
alias lla="ls -la"

# for cd
alias ...="cd ../../"
alias ....="cd ../../../"

# for git
alias ga="git add -p"
alias gcm="git commit"
alias gs="git status"
alias gf="git fetch"
alias gd="git diff"
alias gdc="git diff --cached"

#
#  zplug setting
#
source ~/.zplug/init.zsh

zplug "wbinglee/zsh-wakatime"

# syntax
zplug "chrissicool/zsh-256color"
zplug "Tarrasch/zsh-colors"
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "ascii-soup/zsh-url-highlighter"

# program
zplug "voronkovich/mysql.plugin.zsh"

# tools
zplug "marzocchi/zsh-notify"
zplug "oknowton/zsh-dwim"

# peco
zplug "peco/peco", as:command, from:gh-r

# fzf-tmux の peco バージョン
zplug "b4b4r07/dotfiles", as:command, use:bin/peco-tmux


# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose
