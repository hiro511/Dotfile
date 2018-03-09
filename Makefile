.PHONY: install backup

install:
	ansible-playbook -i ansible/hosts ansible/packages.yml

backup:
	cp -rf $HOME/.emacs.d .emacs.d
	cp -rf $HOME/.oh-my-zsh .oh-my-zsh
	cp -f .zshrc .zshrc
