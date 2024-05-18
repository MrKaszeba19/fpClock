#!/bin/zsh



# ======================================================
# ======================|INFO|==========================
# Use: ./installZsh.sh
#   or ./installZsh.sh [flags]
#
# flags:
#    - (none)  : Just install fpclock to your bash $PATH
#
# ======================================================
# =====================|SCRIPT|=========================

appname="fpclock"
appexec="fpclock"

mkdir -p $HOME/bin
if [ $? -ne 0 ] ; then
	echo "Error when creating or finding directory $HOME/bin."
else
	cp ./$appexec $HOME/bin/$appexec
	if [ $? -ne 0 ] ; then
		echo "Error when copying $appname to $HOME/bin."
	else
		if grep "export PATH=${HOME}/bin:\$PATH" ${HOME}/.zshrc ;
		then
			echo "It looks like you've already installed \$PATH on $HOME/bin."
			echo "Installation of $appname to your bash \$PATH has been finished with success!"
			echo "Type '$appexec' in your command prompt to check out the program then."
		else
			echo "# Set up a $appname path" >> $HOME/.zshrc
			echo "export PATH=${HOME}/bin:\$PATH" >> $HOME/.zshrc
			source $HOME/.zshrc 
			if [ $? -eq 0 ] ; then
				echo "Installation of $appname to your zsh \$PATH has been finished with success!"
				echo "Type '$appexec' in your command prompt to check out the program."
			else
				echo "Error when setting up \$PATH."
			fi
		fi	
	
	fi
fi



