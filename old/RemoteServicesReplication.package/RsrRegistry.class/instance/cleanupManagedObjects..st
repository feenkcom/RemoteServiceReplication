accessing
cleanupManagedObjects: orphanedKeys
    orphanedKeys do: [ :entry |
        (managedObjects at: entry )
            ifNil: [ managedObjects removeKey: entry].
        managedObjects removeKey: entry.
     ].
