accessing
sendUsing: aCommandWriter

	self error.
	retainList do: [:each | each sendUsing: aCommandWriter].
	self error