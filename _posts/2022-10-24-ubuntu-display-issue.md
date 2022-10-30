---
title: "Ubuntu external monitors not working"
date: 2022-10-24 9:00:00 +0200
categories: ubuntu
tags: installtion, GPU
---

After reinstalling Ubuntu 20.04, I encountered the same problem as I had in Ubuntu 22.04 when I was trying to connect external monitors.

The default Nividia driver is nouveau, and it didn't work. 
Then I switched to the latest Nvidia driver 515 through "Additional driver", and switched to it multiple times with reboot, and tried it on different monitors, and it seems worse than nouveau.

After switching to Nvidia drivier 515, there is an error in `nvidia-smi` showing it cannot detect GPU.

Then I uninstalled that Nvidia driver with `sudo apt remove --purge nivida* -y` and `sudo apt autoremove -y`, and reinstalled it from command line with `sudo ubuntu-drivers autoinstall`, but after the reboot, it has no improvement at all.

Finally, it is solved by installing an **older** Nividia driver 470, and after the reboot, all monitors work normally. 
