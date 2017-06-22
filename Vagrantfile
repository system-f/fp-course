Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/xenial64"

  config.vm.provider 'virtualbox' do |vbox|
    vbox.memory = 4096
    vbox.cpus = 2
    vbox.gui = true
  end

  config.vm.provision 'ansible' do |ansible|
    ansible.playbook = 'ops/ansible.yaml'
  end
end
