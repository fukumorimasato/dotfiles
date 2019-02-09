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
PROMPT="[%{${fg[yellow]}%}%n@%m%{${reset_color}%} %{${fg[green]}%}%c%{${reset_color}%}]$ "

# コマンドを途中まで入力後、historyから絞り込み
# 例 ls まで打ってCtrl+pでlsコマンドをさかのぼる、Ctrl+bで逆順
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

## cdr
autoload -Uz add-zsh-hock
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook

##
##  for emacs
##
# referenced url:
#  https://qiita.com/regashia/items/6bd9de68d596f6469129
#
function estart() {
  if ! emacsclient -e 0 > /dev/null 2>&1; then
    emacs --daemon
  fi
}

alias ekill='emacsclient -e "(kill-emacs)"'
alias emacs='emacsclient -nw -a ""'

#
# setting for ls command
#
export LSCOLORS=exfxcxdxbxexagabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

#
#  aliases
#

# for less
alias less="less --no-init --quit-if-one-screen --RAW-CONTROL-CHARS -g -j10"
# for validate local aliases when execute sudo cmd
alias sudo="sudo "

# for ls
case ${OSTYPE} in
    darwin*)
	# setting for mac
	alias ls="ls -FG"
	;;
    linux*)
	# setting for linux
	alias ls="ls -F --color=auto"
	;;
esac
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

# for pipe
alias -g T='| tail'
alias -g L='| less'
alias -g F='| fzf'
alias -g S='| sort'
alias -g P='| peco'

# for extract
function extract() {
  case $1 in
    *.tar.gz|*.tgz) tar xzvf $1;;
    *.tar.xz) tar Jxvf $1;;
    *.zip) unzip $1;;
    *.lzh) lha e $1;;
    *.tar.bz2|*.tbz) tar xjvf $1;;
    *.tar.Z) tar zxvf $1;;
    *.gz) gzip -d $1;;
    *.bz2) bzip2 -dc $1;;
    *.Z) uncompress $1;;
    *.tar) tar xvf $1;;
    *.arj) unarj $1;;
  esac
}
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

#
#  for proxy
#
local PROXYFILE=$HOME/.config/zsh/.proxyrc
if [ -e $PROXYFILE ]; then
    source $PROXYFILE
fi

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

# theme
#zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme

# program
#zplug "voronkovich/mysql.plugin.zsh"

# tools
zplug "marzocchi/zsh-notify"

# peco
zplug "peco/peco", as:command, from:gh-r

# fzf-tmux の peco バージョン
zplug "b4b4r07/dotfiles", as:command, use:bin/peco-tmux

# for cd
zplug "b4b4r07/enhancd", use:init.sh

zplug "zsh-users/zsh-completions"

# anyframe
zplug "mollifier/anyframe"

zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf
zplug "junegunn/fzf", as:command, use:bin/fzf-tmux

# for inclimental search
#zplug "zsh-users/zaw"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    echo; zplug install
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose


case ${OSTYPE} in
    darwin*)
	fpath=(/usr/local/share/zsh-completions $fpath)
	;;
esac


function available () {
    local x candidates
    candidates="$1:"
    while [ -n "$candidates" ]
    do
        x=${candidates%%:*}
        candidates=${candidates#*:}
        if type "$x" >/dev/null 2>&1; then
            echo "$x"
            return 0
        else
            continue
        fi
    done
    return 1
}
zle -N available

FILTER="peco:fzf"

#
#  setting for fuzy serch tool
#
function history-selection() {

    case ${OSTYPE} in
        darwin*)
            # setting for mac
            BUFFER=`history -n 1 | tail -r  | awk '!a[$0]++' | \`available ${FILTER}\``
            ;;
	linux*)
            # setting for linux
            BUFFER=`history -n 1 | tac  | awk '!a[$0]++' | \`available ${FILTER}\``
            ;;
    esac

    CURSOR=$#BUFFER
    zle reset-prompt
}

zle -N history-selection
bindkey '^R' history-selection

if [ -x "`which ag`" ]; then
    function peco-ag () {
	# ag $@ | peco --query "$LBUFFER" | awk -F : '{print "+" $2 " " $1}'
	emacs $(ag $@ | peco --query "$LBUFFER" | awk -F : '{print "+" $2 " " $1}')
    }
    zle -N peco-ag

    function file-serch-emacs () {
	if [ $# ] && [ -f $@ ]; then 
	    emacs $@
	elif [ $# ] && [ -d $@ ]; then
	    emacs $(ag -l . $@ | peco)
	else
	    emacs
	fi
    }
    zle -N file-serch-emacs
    alias fsemcs="file-serch-emacs"
fi

#
#  colered man page
#
man() {
        env \
                LESS_TERMCAP_mb=$(printf "\e[38;5;206m") \
                LESS_TERMCAP_md=$(printf "\e[38;5;206m") \
                LESS_TERMCAP_me=$(printf "\e[0m") \
                LESS_TERMCAP_se=$(printf "\e[0m") \
                LESS_TERMCAP_so=$(printf "\e[38;5;190m") \
                LESS_TERMCAP_ue=$(printf "\e[38;5;012m") \
                LESS_TERMCAP_us=$(printf "\e[38;5;012m") \
                man "$@"
}

#
#  for tmux
#
alias t="tmux a || tmux"

# start tmux when zsh
if [ -z $TMUX ]; then

    DATE=$(date +%Y-%m-%d)
    LOG_DIR=$HOME/logs/tmux/$DATE

    if [ ! -d $LOG_DIR ]; then
	mkdir -p $LOG_DIR
	;
    fi

    t
fi

#
#  for pyenv
#
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if [ -x "$(which pyenv)" ]; then
    eval "$(pyenv init -)"
fi

# for go lang
if [ -x "`which go`" ]; then
  export GOPATH=$HOME/.go
  export PATH="$GOPATH/bin:$PATH"
fi

##
##  for ghq
##  http://suzutan.hateblo.jp/entry/2017/12/05/165204
##
# リポジトリにcd
function peco-repo-list () {
  local selected_dir=$(ghq list -p | sort | peco --query "$LBUFFER")
  if [ -n "$selected_dir" ]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N peco-repo-list
bindkey '^[' peco-repo-list

# リポジトリをブラウザで開く
function peco-git-browse () {
    github_repo=$(ghq list | sort | peco)
    GITHUB_HOST=$(echo ${github_repo} | cut -d "/" -f 1) hub browse $(echo ${github_repo} | cut -d "/" -f 2,3)
}
zle -N peco-git-browse
bindkey '^]' peco-git-browse

# el-getで取得したossにcd
function peco-el-get-list () {
    local elget_root=$HOME/.emacs.d/el-get
    local selected=$(find .emacs.d/el-get -depth 1 -type d | sort |peco)
    
    if [ -n "$selected" ]; then
	BUFFER="cd $selected"
	zle accept-line
    fi
    zle clear-screen    
}
zle -N peco-el-get-list
bindkey '^x^[' peco-el-get-list

# el-getで取得したossのrepoをbrose
function peco-el-get-browse () {
    local elget_root=$HOME/.emacs.d/el-get
    local selected=$(find .emacs.d/el-get -depth 1 -type d  | xargs -I{} sh -c "cd {} && git remote -v \
    | cut -d ' ' -f 1 | cut -d '/' -f 4,5 | sed 's/\.git//' | head -n 1&& cd .." | sort |peco)
    
    if [ -n "$selected" ]; then
	BUFFER="hub browse $selected"
	zle accept-line
    fi
    zle clear-screen    
}
zle -N peco-el-get-browse
bindkey '^x^]' peco-el-get-browse

# zplugで取得したossにcd
function peco-zplug-list () {
    local selected=$(find $HOME/.zplug/repos  -depth 1 -type d | sort |peco)
    
    if [ -n "$selected" ]; then
	BUFFER="cd $selected"
	zle accept-line
    fi
    zle clear-screen    
}
zle -N peco-zplug-list
bindkey '^z^[' peco-zplug-list

# zplugで取得したossのrepoをbrose
function peco-zplug-browse () {
    local selected=$(zplug list | cut -d ' ' -f 1 P)
    
    if [ -n "$selected" ]; then
	BUFFER="hub browse $selected"
	zle accept-line
    fi
    zle clear-screen    
}
zle -N peco-zplug-browse
bindkey '^z^]' peco-zplug-browse

##
##  for emacs
##
# referenced url:
#  https://qiita.com/regashia/items/6bd9de68d596f6469129
estart

# .zshrc ends here
