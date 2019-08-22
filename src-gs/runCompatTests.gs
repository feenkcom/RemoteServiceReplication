input RemoteServiceReplication-Base.gs
input loadRsrCompat.gs
input compat-test/tests.gs

exec
    | result report |
    result := RsrTestCase run.
    report := String
        streamContents:
            [:stream |
            stream nextPutAll: 'Passes:'.
            result passed
                do:
                    [:each |
                    stream lf; tab.
                    each printOn: stream].
            stream
                lf; lf;
                nextPutAll: 'Failures:'.
            result failures
                do:
                    [:each |
                    stream lf; tab.
                    each printOn: stream].
            stream
                lf; lf;
                nextPutAll: 'Errors:'.
            result errors
                do:
                    [:each |
                    stream lf; tab.
                    each printOn: stream]].
    GsFile gciLogClient: report
%