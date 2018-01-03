receiving
receive: data
	  | stream results |
	  stream := STONReader on: data readStream. 
	  results := stream next.
	  ^self processReceivedObject: results.