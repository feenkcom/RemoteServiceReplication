run
RsrObject
	subclass: #RsrEphemeron
	instVarNames: #(key mournAction)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: UserGlobals
%

set class RsrEphemeron

classMethod:
on: anObject
mournAction: aBlock

    ^self new
        key: anObject;
        mournAction: aBlock;
        beEphemeron: true;
        yourself
%

method:
key

    ^key
%

method:
key: anObject

    key := anObject
%

method:
mournAction: aBlock

    mournAction := aBlock
%

method:
mourn

    mournAction value.
    key := mournAction := nil
%