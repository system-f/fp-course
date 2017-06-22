# Vagrant Box

If you'd rather use a pre-configured haskell development environment, then this will get you up and running in a Virtual Box virtual machine. You'll get:

 - Xubuntu desktop environment
 - GHC 8.0.2 installed
 - doctest
 - emacs with haskell-mode
 - vim
 - sublime
 - VS Code

**WARNING** Building the environment might take a while and download gigabytes of pacakges over the internet.

## Prerequisites

 - Install VirtualBox
 - Install Vagrant
 - Install ansible
 
## Make it so

The following will download a VM image of Ubuntu and then provision it to build a desktop environment for Haskell development. Once it's provisioned, reload the machine, which will log you straight into a graphical environment.

```
cd course
vagrant up
vagrant reload
```

You should now see a virtual machine running Xubuntu. The course materials are checked out to
`~/fp-course` and you should have all required binaries on your PATH.
