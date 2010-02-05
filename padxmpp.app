{application, padxmpp,
 [{description, "People Are Ducks XMPP Server"},
  {vsn, "0.0"},
  {mod, {padxmpp_app, []}},
  {applications, [kernel, stdlib]},
  {modules, [gen_listener_tcp, padxmpp, padxmpp_auth_fsm,
	     padxmpp_client_sup, padxmpp_conn_listener, 
	     padxmpp_conn_table, padxmpp_xml_scan,
	     pevent]}
 ]}.
