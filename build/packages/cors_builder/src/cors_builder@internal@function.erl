-module(cors_builder@internal@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/cors_builder/internal/function.gleam").
-export([tap/2, identity/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/cors_builder/internal/function.gleam", 1).
?DOC(false).
-spec tap(RPI, fun((RPI) -> any())) -> RPI.
tap(A, Next) ->
    Next(A),
    A.

-file("src/cors_builder/internal/function.gleam", 6).
?DOC(false).
-spec identity(RPK) -> RPK.
identity(A) ->
    A.
