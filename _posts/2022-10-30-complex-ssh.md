---
title: "Complex ssh operations"
date: 2022-10-30 7:00:00 +0200
categories: ssh 
tags: ubuntu
math: true
---

Recently I have been trying out complex SSH operations. 

### Scenario 1

One scenario is, say I have two hosts PC1 and PC2, connected within the same subnet. Then I have a VM running in PC2, and I want to build connection from PC1 to this vm.
Here are opertions I need to do given both PC2 and VM are using Ubuntu 20.04 and the VM manager is `virt-manager`:

1. install openssh-server on PC2 with  `sudo apt install openssh-server -y`

2. open a uncommon ssh port (e.g., 44556) on PC2 with `sudo echo "Port 44556" >> /etc/ssh/sshd_config`

3. restart ssh service with `sudo service ssh restart`

4. install `ifconfig` on both PC2 and VM with `sudo apt install net-tools`

5. check PC2's ipv4 address `<PC2-IP>` by displaying `ifconfig` and searching for the address belonging to the subnet `192.168.0.0/24` assuming both PC1 and PC2 are connected under a family WLAN.

6. set 2 port-forwarding rules in PC2's iptables (say we use 55667 port to indicate the intention to connect to the VM instead of PC2):
    1. `sudo iptables -t nat -A PREROUTING  -j DNAT -p tcp --dport 55667 --to <VM-IP>:22`

        This appends a rule in the NAT table, which asks PC2 to forward traffic received from port 55667 to VM's normal SSH port. The <VM-IP> can be found on VM by displaying `ifconfig` and searching for the ipv4 address belonging to the subnet `192.168.0.0/24`. Note that it is not required to open 55667 port on PC2 (unlike how we did for 44556).

    2. `sudo iptables -I FORWARD -d <VM-IP>/255.255.255.0 -m conntrack --ctstate NEW,ESTABLISHED,RELATED -j ACCEPT`

        This inserts a rule that asks PC2 to perform port forwarding for any new or existed traffic destined to the VM subnet, without specifying this rule, any new connection from PC1 will not be forwarded.

After the above operations, one should now be able to ssh to PC2 via PC1 with `ssh -p 44556 <PC2-user>@<PC2-IP>` with password, where `<PC2-user>` is the username of PC2.
To connect to the VM in PC2, one first need to add the public key of PC1 in the VM's known-hosts as I found it is by default not enabled to use password authentication on VM, and then use `ssh -p 55667 <VM-user>@<PC2-IP>` to connect to the VM, where `<VM-user>` refers to the username in VM.

### Scenario 2

Another more complicated scenario is, say I still have two hosts PC1 and PC2, and a VM hosted on PC2, more interestingly, there is also a container in the VM, and I want to connect to the container via PC1. 

I already have a scipt from somewhere such that I can connect to the connect to the container on PC2 with `ssh -p 45678 <container-user>@<VM-IP>`, so I haven't figured out how to do this myself. 
However, given this, there is only one step required to achieve the connection from PC1 to the container, which is to just update the first rule (i.e., the NAT rule) by chaning the port number from `22` to `45678` in the end of rule, and then one can log in with `ssh -p 45678 <container-user>@<PC2-IP>` from PC1.

    
    







