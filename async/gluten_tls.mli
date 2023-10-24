open Async

val create_default : ?alpn_protocols:string list ->
                     certfile:string ->
                     keyfile:string ->
                     ([< Socket.Address.t ] as 'a) ->
                     ([ `Active ], 'a) Socket.t ->
                     (Reader.t * Writer.t * unit Deferred.t) Deferred.t