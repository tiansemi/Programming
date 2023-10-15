#!/usr/bin/bash

# Configuration de l'interface wlan0 
# pour accéder au VMs de Ultimate Pentest
# En effet, il accéde aux VMs via la machine server
# qui a les interfaces directement connecter aux réseaux spécifiques
# PentestNet , HiddenNet , RedTeamLAb , Bridge Network
#
#@autor : tiansemi@outlook.com 
defaultroute=$(ip r|grep default|cut -di -f2|awk -c '{print $2}'|head -1)
sudo ifconfig wlan0 192.168.43.99/24

sudo route add -net 192.168.43.0 netmask 255.255.255.0 dev wlan0

sudo route add -net 172.30.1.0 netmask 255.255.255.0 gw 192.168.43.254

sudo route add -net 192.168.42.0 netmask 255.255.255.0 gw 192.168.43.254

sudo ip route add default via "$defaultroute"
