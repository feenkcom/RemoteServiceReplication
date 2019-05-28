private-encoding
encodeReleaseObjects: aReleaseObject

	^ByteArray
		streamContents:
			[:stream |
			self
				writeControlWord: self releaseObjectsCommand
				onto: stream.
			self
				writeControlWord: aReleaseObject oids size
				onto: stream.
			aReleaseObject oids
				do:
					[:oid |
					self
						writeControlWord: oid
						onto: stream]]