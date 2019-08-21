run
RsrObject
	subclass: #RsrDateTimeInterface
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrDateTimeInterface

classmethod:
dateTimeClass

    ^DateAndTime
%

classmethod:
fromMicroseconds: anInteger
	"DateTime is encoded in offset +00:00"

    ^self dateTimeClass
        posixSeconds: (anInteger / 1000000)
        offset: Duration zero
%

classmethod:
microsecondsSince: aDateAndTime

    ^((aDateAndTime asSeconds - self posixEpoch asSeconds) * 1000000) rounded
%

classmethod:
now

    ^self dateTimeClass now
%

classmethod:
fromString: aString

    ^self dateTimeClass fromString: aString
%

classMethod:
posixEpoch

    ^self dateTimeClass
        posixSeconds: 0
        offset: Duration zero
%