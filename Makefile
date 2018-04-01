.PHONY: install put

install:
	ansible-playbook -i ansible/hosts ansible/packages.yml

put:
	ln -sf ${CURDIR}/.emacs.d ${HOME}/.emacs.d
	ln -sf ${CURDIR}/.zshrc ${HOME}/.zshrc
