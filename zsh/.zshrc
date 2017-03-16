#
#  @file .zshrc
#

source ~/.zplug/init.zsh

zplug "wbinglee/zsh-wakatime"

# syntax
zplug "chrissicool/zsh-256color"
zplug "Tarrasch/zsh-colors"
zplug "zsh-users/zsh-syntax-highlighting"
zplug "ascii-soup/zsh-url-highlighter"

# program
zplug "voronkovich/mysql.plugin.zsh"

# tools
zplug "marzocchi/zsh-notify"
zplug "oknowton/zsh-dwim"

# emacsライクな操作を行う
bindkey -e

# 自動補完有効
autoload -U compinit
compinit

# 色指定有効 
#  : ${fg[色指定]}と${reset_color}で色指定が可能になる
autoload -Uz colors
colors

# プロンプトの設定 
#  : /etc/zshrcで設定しているのでzshenvではなく
#  : zshrcで設定を行う
HOST=`hostname`
PROMPT="[${fg[yellow]}%n@%m${reset_color} ${fg[green]}%c${reset_color}]$ "

# alias 
source ~/dotfiles/zsh/aliases.zsh
