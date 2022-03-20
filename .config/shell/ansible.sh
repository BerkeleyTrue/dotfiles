function aroleinit() {
  if ! [ -z $1 ]; then
    echo "Ansible Role : $1 Creating...."
    ansible-galaxy init $1
    tree $1
  else
    echo "Usage : $0 <role name>"
    echo "Example : $0 role1"
  fi
}

# Alias
alias a='ansible '
alias aconf='ansible-config '
alias acon='ansible-console '
alias aver='ansible-version'
alias arinit='ansible-role-init'
alias aplay='ansible-playbook '
alias ainv='ansible-inventory '
alias adoc='ansible-doc '
alias agal='ansible-galaxy '
alias apull='ansible-pull '
alias aval='ansible-vault'
