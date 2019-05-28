encoding
writeUsing: aCommandWriter

	retainList do: [:each | each writeUsing: aCommandWriter].
	aCommandWriter write: encoding