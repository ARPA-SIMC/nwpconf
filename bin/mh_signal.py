#!/usr/bin/env python3

import sys, os
import requests
import json

from mhauth import *

class My_requests:
    def __init__(self, http):
        self.http = http

    def login(self, data):
        response = self.post("{}/auth/login".format(self.http), data)

        # print(self.http, response)

        try:
            self.token = response.json()
        except:
            self.token = str(int(response.ok))

    def data_ready(self, data):
        print(data)
        return self.post(
            "{}/api/data/ready".format(self.http),
            data,
            headers={"Authorization": "Bearer " + self.token},
        )

    def get(self, url, params):
        pass

    def post(self, url, data={}, headers={}):
        return requests.post(url, data=data, headers=headers, allow_redirects=True)

    def put(self):
        pass

    def delete(self):
        pass

    def head(self):
        pass

    def options(self):
        pass


def main():
    Description = "Make HTTP requests in python"


if __name__ == "__main__":

    https_hub = REQ_DR[CHVERSION]["https_hub"] #"https://meteohub.mistralportal.it/"  #DEV: https://meteohub-dev.hpc.cineca.it/
    my_req = My_requests(https_hub)

    my_data_user = {"username": REQ_DR["username"], "password": REQ_DR["keys"]}

    my_req.login(my_data_user)

    data = {
        "Cluster": os.environ["HPC_SYSTEM"].upper(),
        "Model": sys.argv[1], # this should be the name of the arkimet directory
        "rundate": int(sys.argv[2][:10]) # truncate to hour
    }

    response = my_req.data_ready(data)

    print(
        "STATUS CODE: {} (EXPECTED: 202) ".format(
            response.status_code
        )
    )


# 2xx Success
# L'azione è stata ricevuta con successo, compresa ed accettata.
# 200 OK
# Risposta standard per le richieste HTTP andate a buon fine.
# 201 Created
# La richiesta è stata soddisfatta, restituendo la creazione di una nuova risorsa.
# 202 Accepted
# La richiesta di elaborazione è stata accettata ma non è ancora terminata.

# 4xx Client Error
# La richiesta è sintatticamente scorretta o non può essere soddisfatta.
