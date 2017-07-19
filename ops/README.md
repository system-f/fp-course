# Vagrant Box

If you'd rather use a pre-configured haskell development environment, then these instructions will
get you up and running in a VirtualBox virtual machine. The machine includes:

 - A Xubuntu desktop environment
 - GHC 8.0.2 installed
 - doctest
 - emacs with haskell-mode
 - vim
 - sublime
 - VS Code

**NOTE**: The VM's default user is `ubuntu` and their password is `ubuntu`

**WARNING**: Building the environment might take a while and download gigabytes of pacakges over the internet.

## Prerequisites

 - Install [VirtualBox](https://www.virtualbox.org/)
 - Install [Vagrant](https://www.vagrantup.com/)
 - Install [ansible](https://www.ansible.com/)

## Make it so

The following will download a VM image of Ubuntu and then provision it to build a desktop
environment for Haskell development. Once it's provisioned, reload the machine, which will log you
straight into a graphical environment.

```
cd fp-course
vagrant up
# go have lunch - this could take a while
vagrant reload
```

You should now see a virtual machine running Xubuntu. The course materials are checked out to
`~/fp-course` and you should have all required binaries on your PATH.
