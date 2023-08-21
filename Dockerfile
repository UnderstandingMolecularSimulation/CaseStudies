FROM ubuntu:22.04

RUN apt-get update && apt install -y gcc gfortran python3 python3-pip git csh

COPY ./requirements.txt .
RUN pip3 install -r requirements.txt
