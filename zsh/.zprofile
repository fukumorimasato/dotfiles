#
#  @file .zprofile
#

# コマンド履歴
HISTFILE=~/.zsh_history
HISTSIZE=6000000
SAVEHIST=6000000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data

export SVN_EDITOR='emacsclient -nw -a ""'
export LANG=ja_JP.UTF-8
export LESSCHARSET=dos
export LC_ALL=en_US.UTF-8

export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/opt/tools/
export PATH=~/bin/:$PATH

export LESS="-g -i -M -R -S -W -z-4 -x4"

# for fzf
export FZF_DEFAULT_OPTS="--ansi --reverse --select-1 --exit-0"

# for enhancd
export ENHANCD_FILTER=peco:fzf

# コマンドミスを修正
setopt correct

# コマンド履歴検索
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# Ctrl+sのロック, Ctrl+qのロック解除を無効にする
setopt no_flow_control

# 補完後、メニュー選択モードになり左右キーで移動が出来る
zstyle ':completion:*:default' menu select=2

# 補完で大文字にもマッチ
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
