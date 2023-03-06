
import requests

url = "http://opendata.cbs.nl/ODataFeed/odata/80590ned/TypedDataSet"

file1 = open('filter_small.txt', 'r')
filter = file1.readline()

query = {"$format" : "json", "$filter" : filter}
print(query)

# get method
response = requests.get(url, params = query)
print(response)
print(response.text)

print("\npost method\n")

# post method
response2 = requests.post(url, data = query)
print(response2)
print(response2.text)

