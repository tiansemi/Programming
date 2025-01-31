# Let's first read the file and extract the URLs to test their validity.
file_path = './blackarch-mirrorlist'

# Open and read the contents of the file
with open(file_path, 'r') as file:
    lines = file.readlines()

# Extract URLs from the file (lines that are not comments or empty)
urls = [line.split(' = ')[1].split('$repo')[0] for line in lines if 'http' in line]

import requests

output_file = 'successful_mirrors.txt'
with open(output_file, 'w') as file:
    # Loop through each URL and test if it is accessible
    for url in urls:
	    
	    try:
	        # Test the URL accessibility using requests
	        response = requests.head(url, allow_redirects=True)
	        
	        # Check the response status
	        if response.status_code == 200:
	            print(f"Success: {url} is reachable.")
	            file.write(f"Server = {url}/$repo/os/$arch\n")
	        else:
	            print(f"Failure: {url} status code {response.status_code}.")
	            # print(f"Failure: {url} returned status code {response.status_code}.")
	    
	    except requests.RequestException as e:
	        # Handle any request-related exceptions (e.g., connection errors)
	        print() #quick
	        # print(f"Failure: {url} is not reachable. Error: {e}") #debug
