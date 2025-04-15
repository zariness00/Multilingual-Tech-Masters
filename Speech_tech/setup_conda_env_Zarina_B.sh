#!/bin/bash

source "$(conda info --base)/etc/profile.d/conda.sh"

conda create --name speech python=3.10 -y

conda activate speech

conda install -c conda-forge pip -y
python -m pip install -r requirements.txt


conda install scipy -y

conda uninstall scipy -y

python --version

conda list --export > installed_packages_Zarina_B.txt
