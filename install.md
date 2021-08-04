Installation of Required Software
================
Aaron Peikert
05/01/2021

# 1 Why you want to use package managers

Installing software can be a painful experience, especially if you have
to sift through the documentation for multiple platforms. A package
manager helps you to install, update and remove software on your
computer, automating large parts of the process for you. Most
importantly, the installation process is the same for every software,
not unlike the appstore on your phone. Unfortunately though, it is not
as pleasantly designed, but is used through the terminal. If the thought
of using a terminal lets you hesitate, rest assured it is easier than
you might think.

# 2 Windows 10

## 2.1 How to open a terminal on Windows 10?

1.  Press Windows 10 key + X.
2.  Click on “Windows Powershell” or “Windows Powershell (Admin)” (if
    you are using Chocolately)

## 2.2 How to install a package manager for Windows 10?

[Chocolately](https://chocolatey.org/)[1] is a great package manager for
Windows 10. You should follow these installation instructions:
<https://chocolatey.org/docs/installation>, but if you are in a hurry:

1.  Press Windows key + X, click on “Windows Powershell (Admin)”.
2.  Paste:
    `Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))`
3.  Press enter.
4.  Close the PowerShell

## 2.3 How to install R + Rstudio with a package manager?

Since you probably already have R and RStudio you may skip this step.

1.  Open a new terminal (Press Windows key + X, click on “Windows
    Powershell (Admin)”).
2.  Paste `choco install -y r r.studio rtools`
3.  Press enter

## 2.4 How to install Git, Make, and Docker with a package manager?

Please note that Docker does may not work on Windows 10 Home, because
Docker requires a feature called virtualization which you may have to
enable. In that case follow this
[guide](https://docs.docker.com/docker-for-windows/troubleshoot/#virtualization).
This feature is usually enabled in Windows Pro or Education. On Windows
Home, you also require the Windows Subsystem for Linux (WSL) as well as
the WSL Ubuntu extension.

1.  Open a new terminal (Press Windows key + X, click on “Windows
    Powershell (Admin)”).
2.  Paste `choco install -y git make docker`
3.  Press enter.

On Windows Home and systems without Hyper V:

1.  Open a new terminal (Press Windows key + X, click on “Windows
    Powershell (Admin)”).
2.  Paste `choco install -y git make docker wsl`
3.  Press enter.
4.  Restart.
5.  Open a new terminal (Press Windows key + X, click on “Windows
    Powershell (Admin)”).
6.  Paste `choco install -y wsl-ubuntu-2004`
7.  Press enter.

# 3 Mac OS

## 3.1 How to open a terminal on Mac OS?

1.  Press Command + Space.
2.  Type “Terminal”.
3.  Press enter.

## 3.2 How to install a package manager for Mac OS?

[Homebrew](https://brew.sh)[2] is a great package manager for Mac OS. To
install it:

1.  Open a new terminal (Press Command + Space, type “Terminal”, press
    enter).
2.  Paste `xcode-select --install` and press enter to install xcode.
3.  Paste:
    `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`.
4.  Press enter.
5.  Close the terminal.

## 3.3 How to install R + Rstudio with a package manager?

Since you probably already have R and RStudio you may skip this step.

1.  Open a new terminal (Press Command + Space, type “Terminal”, press
    enter).
2.  Paste `brew install r` and press enter to install R.
3.  Paste `brew install --cask rstudio` and press enter to install
    RStudio.

## 3.4 How to install Git, Make, and Docker with a package manager?

1.  Open a new terminal (Press Command + Space, type “Terminal”, press
    enter).
2.  Paste `brew install git make docker`.
3.  Press enter.

[1] <https://chocolatey.org/>

[2] <https://brew.sh>
