-record(connection, {id=0, jid=unknown, status=connecting, socket, connected_at}).

-record(server, {name, module}).

-define(HOST, "localhost").
-define(PORT, 9876).
-define(CONN_TABLE, connection_table).
