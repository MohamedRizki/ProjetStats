import requests
import csv



api_call = "https://bitnodes.io/api/v1/snapshots/"
timestamp = "1737124530"
# timestamp = "latest"
# timestamp = "1736950433"
api_call = api_call + timestamp + "/" 


response = requests.get(api_call)
nodes_data = response.json()["nodes"]
nodes_data = nodes_data.values()
nodes_data_network = [[data[7],data[8],data[9]] for data in nodes_data if data[-1]!='Tor network']

# print(nodes_data_network)

fields = ["country","lat","long"]

file_name = "data/nodes_data_" + timestamp + ".csv"

with open(file_name, 'w') as f:
     
    # using csv.writer method from CSV package
    write = csv.writer(f)
     
    write.writerow(fields)
    write.writerows(nodes_data_network)