accessing
newTestRemoteInterfaceUsing: aForwarderClass

	^RsrMockRemoteInterface connection: (RsrMockConnection forwarderClass: aForwarderClass)