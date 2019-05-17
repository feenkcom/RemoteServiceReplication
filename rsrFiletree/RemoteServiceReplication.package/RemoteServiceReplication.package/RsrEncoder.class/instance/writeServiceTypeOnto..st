private-writing
writeServiceTypeOnto: aStream

	self
		writeInteger: self serviceType
		onto: aStream