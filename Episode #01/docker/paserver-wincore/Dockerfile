FROM mcr.microsoft.com/windows/servercore:ltsc2019

ADD paserver_install.bat /paserver/paserver_install.bat

ADD https://altd.embarcadero.com/releases/studio/21.0/2/PAServer/setup_paserver.exe /paserver

WORKDIR /paserver

RUN paserver_install.bat

EXPOSE 64211

CMD ["/Program Files (x86)/Embarcadero/PAServer/21.0/paserver.exe", "-password=embtdocker", "-scratchdir=/paserver"] 

