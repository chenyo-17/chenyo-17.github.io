---
title: "Scaling issue with Nvidia Ubuntu"
date: 2023-03-03 12:00:00 +0200
categories: os
tags: nvidia ubuntu monitor
math: false
---

### Problem

My laptop has a 4k screen and it has a Nvidia GPU and an integrated GPU, I want to conect to an external 1080p monitor via HDMI.
The external monitor is recognized via `xrandr` and `arandr`, but it is over-scaled (not blurry, just too big). I cannot change the resolution of the second screen with `xrandr --addmode` as it always returns bad parameters.

### Solution

#### Pre-requsite

- softwares

  - `nvidia-prime`

  - `nvidia-settings`

  - `arandr`

- make sure Nivida GPU is selected as prime with `sudo prime-select nvidia`, and check `NVIDIA (Performance Mode)` is selected in `nvidia-settings`.
  Then log-out and log-in again, run `nvidia-settings`, confirm there is a `X Server Display Configuration` option.

#### Procedures

The following procedures are required after every reboot:

1. run `nvidia-settings` and click `X Server Display Configuration`

2. select the external monitor and click `Advanced` in the bottom

3. modify `ViewPortIn` to `3840x2160` and click `Apply`

4. run `arandr` to adjust the layout

#### Important note

Although there is a `Save to X Configuration File` option in `nvidia-settings` (requires `sudo` to write the file), it will only save the external screen information, which means once reboot, only the external screen will be detected and the laptop screen cannot be used.

Therefore, do not save `xorg.conf` until I figure out how to modify the file to support multiple screens.
