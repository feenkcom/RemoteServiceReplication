accessing
sendUsing: aCommandWriter

	retainList do: [:each | each sendUsing: aCommandWriter].
	aCommandWriter write: encoding