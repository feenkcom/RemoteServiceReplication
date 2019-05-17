receiving
deSerializeStream: aStream
	  | stream results |
	  stream := STONReader on: aStream readStream. 
	  results := stream next.
	  ^results.