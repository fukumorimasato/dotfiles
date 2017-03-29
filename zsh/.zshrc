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

# コマンドを途中まで入力後、historyから絞り込み
# 例 ls まで打ってCtrl+pでlsコマンドをさかのぼる、Ctrl+bで逆順
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

## cdr
autoload -Uz add-zsh-hock
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook

#
# setting for ls command
#
export LSCOLORS=exfxcxdxbxegedabagacad
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'

#
#  aliases
#

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

# for tmux
alias t="tmux a || tmux"

# for pipe
alias -g T='| tail'
alias -g L='| less'
alias -g F='| fzf'
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

# for cd
zplug "b4b4r07/enhancd", use:init.sh

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    echo; zplug install
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose

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

function diary() {

    dhome="$HOME/diary"
    year=$(date +"%Y")
    month=$(date +"%m")
    day=$(date +"%d")
    
    ddir="${dhome}/${year}/${month}/${day}"
    dfile="${year}${month}${day}_$(whoami).diary"

    if [ ! -d $ddir ]; then
	mkdir -p $ddir
    fi

    if [ ! -e $ddir/$dfile ]; then
	echo -e "#=============================================" >> $ddir/$dfile
	echo -e "# data   : $(date +\"%Y%m%d\")                " >> $ddir/$dfile
	echo -e "# author : $(whoami)                          " >> $ddir/$dfile
	echo -e "#=============================================" >> $ddir/$dfile
	echo -e "                                              " >> $ddir/$dfile
	echo -e "                                              " >> $ddir/$dfile
	echo -e "                                              " >> $ddir/$dfile
	echo -e "#=============================================" >> $ddir/$dfile
	echo -e "# End Of File                                 " >> $ddir/$dfile
	echo -e "#=============================================" >> $ddir/$dfile
    fi

    emacs $ddir/$dfile

}
zle -N diary

#
#  colered man page
#
man() {
        env \
                LESS_TERMCAP_mb=$(printf "\e[1;31m") \
                LESS_TERMCAP_md=$(printf "\e[1;31m") \
                LESS_TERMCAP_me=$(printf "\e[0m") \
                LESS_TERMCAP_se=$(printf "\e[0m") \
                LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
                LESS_TERMCAP_ue=$(printf "\e[0m") \
                LESS_TERMCAP_us=$(printf "\e[1;32m") \
                man "$@"
}
