#! /bin/sh
ansible-playbook main.yml --ask-become -v --skip-tags "update"
