input loadRsrCompat.gs
input loadRSR.topaz
input RsrEvaluationService.gs
input compat-test/tests.gs
input RemoteServiceReplication-Test.gs

PRINTIT
    | result |
    result := RsrTestCase run.
    String
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
                    each printOn: stream]]
%