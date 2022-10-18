---
title: "Reinstall Ubuntu"
date: 2022-10-18 9:00:00 +0200
categories: ubuntu
tags: installtion
math: true
---

I was using Ubuntu 22.04, but to be consistent with the environment on the server, I tried to reinstall Ubuntu 20.04.

I first created a bootable USB stick with the Ubuntu built-in app "Startup Disk Creater", then I reboot and entered BIOS.
I am using ThinkPad P1, so the key to enter BIOS is ENTER (this will be prompted during the startup).
However, when I changed boot option, I couldn't enter my USB, there was no responding when I chose different boot options.

The cause was that I turned on the security boot before when I installed somthing, so I needed to turn off it first.
After that, I successfully reinstalled the system.

