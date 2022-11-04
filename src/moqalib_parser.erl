%%=================================================================================================================%%
%%
%%   Copyright (c) 2022 , Hachemaoui Sidi Mohammed <hachemaouisidimohammed@gmail.com> 
%%
%%    This program is under GNU General Public License v3, you may check the full license's
%%    terms and conditions in LICENSE file for more details.
%%
%%    Rights granted for use, distribution, modification, patent use, and private use of the software.
%%
%%    Requirements to disclose the source code, display the license and copyright notices, 
%%    state all changes, and name the libraries under the same license
%%
%%    You may copy, distribute and modify the software as long as you track changes/dates in source files. 
%%
%%    Any modifications to or software including (via compiler) GPL-licensed code must also be made available 
%%    under the GPLv3 along with build & install instructions.
%%
%%    Users can change or rework the code, but if they distribute these changes/modifications in binary form, 
%%    they’re also required to release these updates in source code form under the GPLv3 license.
%%
%%    As long as these modifications are also released under the GPLv3 license, they can be distributed to others.
%%
%%=================================================================================================================%%


-module(moqalib_parser).


-export([parse/1]).
-export([decode_updates/1]).


-include("../include/moqa_data.hrl").


-spec parse(nonempty_binary()) -> moqalib_checker:moqa_data().
parse(Bin) ->
	case moqalib_checker:check_binary_data(Bin) of
		true ->
			parse_ok(Bin);
		_ ->
			exit('wrong binary data')
	end.


parse_ok(Bin) ->
	decode_fixed_header(Bin).


decode_fixed_header(Bin) ->
	decode_control_packet(Bin).


decode_control_packet(<<1:8>>) ->
	#state{};


decode_control_packet(<<0:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , updates);


decode_control_packet(<<1:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , signup);


decode_control_packet(<<2:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , signack);


decode_control_packet(<<3:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , deactivate);


decode_control_packet(<<4:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , deactivack);


decode_control_packet(<<5:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , connect);


decode_control_packet(<<6:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , connack);


decode_control_packet(<<7:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , disconnect);


decode_control_packet(<<8:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , disconnack);


decode_control_packet(<<9:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , publish);


decode_control_packet(<<10:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , puback);


decode_control_packet(<<11:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , pubcomp);


decode_control_packet(<<12:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , pubcompack);


decode_control_packet(<<13:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , subscribe);


decode_control_packet(<<14:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , suback);


decode_control_packet(<<15:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , subcnl);


decode_control_packet(<<16:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , subcnlack);


decode_control_packet(<<17:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , subresp);


decode_control_packet(<<18:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , subrespack);


decode_control_packet(<<19:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , unsubscribe);


decode_control_packet(<<20:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , unsuback);


decode_control_packet(<<21:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , block);


decode_control_packet(<<22:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , blockack);


decode_control_packet(<<23:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , unblock);


decode_control_packet(<<24:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , unblockack);


decode_control_packet(<<25:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , ping);


decode_control_packet(<<26:8 , Rest/binary>>) ->
	try_decode_remaining_length(Rest , pingack);


decode_control_packet( _ ) ->
	exit('wrong control packet data').


try_decode_remaining_length(Bin , Type) ->
	case decode_remaining_length(Bin) of
		{RemainingLength , Rest} ->
			case moqalib_checker:check_remaining_length(Rest , RemainingLength) of
				true ->
					continue_parse(Rest , Type);
				false ->
					exit('malformed remaining length')
			end;
		undefined ->
			exit('malformed remaining length')
	end.


continue_parse(Bin , updates) ->
	Updates =#{},
	parse_updates_packet(Bin , Updates);


continue_parse(Bin , signup) ->
	Signup =#signup{},
	parse_signup_packet(Bin , Signup);


continue_parse(Bin , signack) ->
	Signack =#signack{},
	parse_signack_packet(Bin , Signack);


continue_parse(Bin , deactivate) ->
	Deactivate =#deactivate{},
	parse_deactivate_packet(Bin , Deactivate);


continue_parse(Bin , deactivack) ->
	Deactivack =#deactivack{},
	parse_deactivack_packet(Bin , Deactivack);
	

continue_parse(Bin , connect) ->
	Connect =#connect{},
	parse_connect_packet(Bin , Connect);


continue_parse(Bin , connack) ->
	Connack =#connack{},
	parse_connack_packet(Bin , Connack);


continue_parse(Bin , disconnect) ->
	Disconnect =#disconnect{},
	parse_disconnect_packet(Bin , Disconnect);


continue_parse(Bin , disconnack) ->
	Disconnack =#disconnack{},
	parse_disconnack_packet(Bin , Disconnack);


continue_parse(Bin , publish) ->
	Publish =#publish{},
	parse_publish_packet(Bin , Publish);


continue_parse(Bin , puback) ->
	Puback =#puback{},
	parse_puback_packet(Bin , Puback);


continue_parse(Bin , pubcomp) ->
	Pubcomp =#pubcomp{},
	parse_pubcomp_packet(Bin , Pubcomp);


continue_parse(Bin , pubcompack) ->
	Pubcompack =#pubcompack{},
	parse_pubcompack_packet(Bin , Pubcompack);


continue_parse(Bin , subscribe) ->
	Subscribe =#subscribe{},
	parse_subscribe_packet(Bin , Subscribe);


continue_parse(Bin , suback) ->
	Suback =#suback{},
	parse_suback_packet(Bin , Suback);


continue_parse(Bin , subcnl) ->
	Subcnl =#subcnl{},
	parse_subcnl_packet(Bin , Subcnl);


continue_parse(Bin , subcnlack) ->
	Subcnlack =#subcnlack{},
	parse_subcnlack_packet(Bin , Subcnlack);


continue_parse(Bin , subresp) ->
	Subresp =#subresp{},
	parse_subresp_packet(Bin , Subresp);


continue_parse(Bin , subrespack) ->
	Subrespack =#subrespack{},
	parse_subrespack_packet(Bin , Subrespack);


continue_parse(Bin , unsubscribe) ->
	Unsubscribe =#unsubscribe{},
	parse_unsubscribe_packet(Bin , Unsubscribe);


continue_parse(Bin , unsuback) ->
	Unsuback =#unsuback{},
	parse_unsuback_packet(Bin , Unsuback);


continue_parse(Bin , block) ->
	Block =#block{},
	parse_block_packet(Bin , Block);


continue_parse(Bin , blockack) ->
	Blockack =#blockack{},
	parse_blockack_packet(Bin , Blockack);


continue_parse(Bin , unblock) ->
	Unblock =#unblock{},
	parse_unblock_packet(Bin , Unblock);


continue_parse(Bin , unblockack) ->
	Unblockack =#unblockack{},
	parse_unblockack_packet(Bin , Unblockack);


continue_parse(Bin , ping) -> 
	Ping =#ping{},
	parse_ping_packet(Bin , Ping);


continue_parse(Bin , pingack) ->
	Pingack =#pingack{},
	parse_pingack_packet(Bin , Pingack).



%%===================================================================================================================================================%%

%%===================================================================================================================================================%%


-spec parse_updates_packet(nonempty_binary() , #{}) -> #roster{} | #subscriptions{}.
parse_updates_packet(<<1:8 , Rest/binary>> , Updates) ->
	parse_roster_updates_packet(Rest , Updates);


parse_updates_packet(<<2:8 , Rest/binary>> , Updates) ->
	parse_subscriptions_updates_packet(Rest , Updates).


parse_roster_updates_packet(Bin , Updates) ->
	try decode_updates(Bin , Updates) of
		NewUpdates ->
			#roster{updates = NewUpdates}
	catch
		error:Error ->
			exit(Error);
		exit:Exit ->
			exit(Exit)
	end.


parse_subscriptions_updates_packet(Bin , Updates) ->
	try decode_updates(Bin , Updates) of
		NewUpdates ->
			#subscriptions{updates = NewUpdates}
	catch
		error:Error ->
			exit(Error);
		exit:Exit ->
			exit(Exit)
	end.

	
decode_updates(Bin , Updates) ->
	try_decode_number_of_updates(Bin , Updates).


try_decode_number_of_updates(Bin , Updates) ->
	case decode_number_of_updates(Bin) of
		{NumberOfUpdates , Rest} ->
			decode_updates(Rest , Updates , NumberOfUpdates);
		undefined ->
			exit('wrong number of updates')
	end.


decode_updates(<<>> , Updates , 0) ->
	Updates;


decode_updates(_Bin , _Updates , 0) ->
	exit('unexpected additional data');


decode_updates(Bin , Updates , RemainingUpdates) ->
	case decode_prefixed_size_data(Bin) of
		{Key , Rest} ->
			case decode_prefixed_size_data(Rest) of
				{Value , RestOfRest} ->
					NewValue = case byte_size(Value) of
							7 ->
								<< Year:16 , Month:8 , Day:8 , Hour:8 , Minute:8 , Second:8 >> = Value,
								{ {Year , Month , Day} , {Hour , Minute , Second} };
							_ ->
								Value
						   end,
					NewUpdates = Updates#{Key => NewValue},
					decode_updates(RestOfRest , NewUpdates , RemainingUpdates - 1);
				undefined ->
					exit('wrong updates value data')
			end;
		undefined ->
			exit('wrong updates key data')
	end.


%%=====================================================================================================================================================%%

%%=====================================================================================================================================================%%


parse_signup_packet(Bin , Signup) ->
	decode_signup_variable_header(Bin , Signup).


decode_signup_variable_header(Bin , Signup) ->
	try_decode_signup_packet_identifier(Bin , Signup).


try_decode_signup_packet_identifier(Bin , Signup) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSignup = Signup#signup{packet_identifier = PacketIdentifier},
			try_decode_signup_fixed_data(Rest , NewSignup);
		undefined ->
			exit('wrong signup packet identifier data')
	end.


try_decode_signup_fixed_data(Bin , Signup) ->
	case moqalib_checker:check_remaining_length(Bin , 8) of
		true ->
			decode_signup_fixed_data(Bin , Signup);
		_ ->
			exit('wrong signup packet fixed data')
	end.


decode_signup_fixed_data(<<5:16 , "MOQA" , Rest/binary>> , Signup) ->
	decode_signup_keep_alive(Rest , Signup);


decode_signup_fixed_data(_Bin , _Signup) ->
	exit('wrong signup packet fixed data').


decode_signup_keep_alive(<<KeepAlive:16 , Rest/binary>> , Signup) ->
	NewSignup = Signup#signup{keep_alive = KeepAlive},
	decode_signup_payload(Rest , NewSignup).


decode_signup_payload(Bin , Signup) ->
	try_decode_signup_username(Bin , Signup).


try_decode_signup_username(Bin , Signup) ->
	case decode_prefixed_size_data(Bin) of
		{Username , Rest} ->
			NewSignup = Signup#signup{username = Username},
			try_decode_signup_password(Rest , NewSignup);
		undefined ->
			exit('wrong signup username data')
	end.


try_decode_signup_password(Bin , Signup) ->
	case decode_prefixed_size_data(Bin) of
		{Password , Rest} ->
			NewSignup = Signup#signup{password = Password},
			end_decode_packet(Rest , NewSignup);
		undefined ->
			exit('wrong signup password data')
	end.



%%======================================================================================================================================================%%

%%======================================================================================================================================================%%


parse_signack_packet(Bin , Signack) ->
	decode_signack_variable_header(Bin , Signack).


decode_signack_variable_header(Bin , Signack) ->
	try_decode_signack_packet_identifier(Bin , Signack).


try_decode_signack_packet_identifier(Bin , Signack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSignack = Signack#signack{packet_identifier = PacketIdentifier},
			decode_signack_payload(Rest , NewSignack);
		undefined ->
			exit('wrong signack packet identifier data')
	end.


decode_signack_payload(Bin , Signack) ->
	try_decode_signack_response(Bin , Signack).


try_decode_signack_response(Bin , Signack) ->
	case decode_response(Bin) of
		{Response , Rest} ->
			NewSignack = Signack#signack{response = Response},
			end_decode_packet(Rest , NewSignack);
		undefined ->
			exit('wrong signack response data')
	end.


%%=======================================================================================================================================================%%

%%=======================================================================================================================================================%%


parse_deactivate_packet(Bin , Deactivate) ->
	decode_deactivate_variable_header(Bin , Deactivate).


decode_deactivate_variable_header(Bin , Deactivate) ->
	try_decode_deactivate_packet_identifier(Bin , Deactivate).


try_decode_deactivate_packet_identifier(Bin , Deactivate) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewDeactivate = Deactivate#deactivate{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewDeactivate);
		undefined ->
			exit('wrong deactivate packet identifier data')
	end.


%%=======================================================================================================================================================%%

%%=======================================================================================================================================================%%


parse_deactivack_packet(Bin , Deactivack) ->
	decode_deactivack_variable_header(Bin , Deactivack).


decode_deactivack_variable_header(Bin , Deactivack) ->
	try_decode_deactivack_packet_identifier(Bin , Deactivack).


try_decode_deactivack_packet_identifier(Bin , Deactivack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewDeactivack = Deactivack#deactivack{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewDeactivack);
		undefined ->
			exit('wrong deactivack packet identifier data')
	end.


%%=======================================================================================================================================================%%

%%=======================================================================================================================================================%%


parse_connect_packet(Bin , Connect) ->
	decode_connect_variable_header(Bin , Connect).


decode_connect_variable_header(Bin , Connect) ->
	try_decode_connect_packet_identifier(Bin , Connect).


try_decode_connect_packet_identifier(Bin , Connect) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewConnect = Connect#connect{packet_identifier = PacketIdentifier},
			try_decode_connect_fixed_data(Rest , NewConnect);
		undefined ->
			exit('wrong connect packet identifier data')
	end.


try_decode_connect_fixed_data(Bin , Connect) ->
	case moqalib_checker:check_remaining_length(Bin , 10) of
		true -> 
			decode_connect_fixed_data(Bin , Connect);
		_ ->
			exit('wrong connect packet fixed data')
	end.


decode_connect_fixed_data(<<4:16 , "MOQA" , 4:8 , Rest/binary>> , Connect) ->
	decode_connect_flags(Rest , Connect);


decode_connect_fixed_data(_Bin , _Connect) ->
	exit('wrong connect packet fixed data').


decode_connect_flags(<<1:1 , 1:1 , 0:1 , 0:2 , 0:1 , 0:1 , 0:1 , Rest/binary>> , Connect) ->
	decode_connect_keep_alive(Rest , Connect);

				     		
decode_connect_flags(_Bin , _Connect) ->
	exit('wrong connect flags data').


decode_connect_keep_alive(<<KeepAlive:16 , Rest/binary>> , Connect) ->
	NewConnect = Connect#connect{keep_alive = KeepAlive},	
	decode_connect_payload(Rest , NewConnect).


decode_connect_payload(Bin , Connect) ->
	try_decode_connect_username(Bin , Connect).


try_decode_connect_username(Bin , Connect) ->
	case decode_prefixed_size_data(Bin) of
		{Username , Rest} ->
			NewConnect = Connect#connect{username = Username},
			try_decode_connect_password(Rest , NewConnect);
		undefined ->
			exit('wrong connect username data')
	end.


try_decode_connect_password(Bin , Connect) ->
	case decode_prefixed_size_data(Bin) of
		{Password , Rest} ->
			NewConnect = Connect#connect{password = Password},
			end_decode_packet(Rest , NewConnect);
		undefined ->
			exit('wrong connect password data')
	end.


%%========================================================================================================================================================%%

%%========================================================================================================================================================%%


parse_connack_packet(Bin , Connack) ->
	decode_connack_variable_header(Bin , Connack).


decode_connack_variable_header(Bin , Connack) ->
	try_decode_connack_packet_identifier(Bin , Connack).


try_decode_connack_packet_identifier(Bin , Connack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewConnack = Connack#connack{packet_identifier = PacketIdentifier},
			decode_connack_payload(Rest , NewConnack);
		undefined ->
			exit('wrong connack packet identifier data')
	end.


decode_connack_payload(Bin , Connack) ->
	try_decode_connack_response(Bin , Connack).


try_decode_connack_response(Bin , Connack) ->
	case decode_response(Bin) of
		{Response , Rest} ->
			NewConnack = Connack#connack{response = Response},
			end_decode_packet(Rest , NewConnack);
		undefined ->
			exit('wrong connack response data')
	end.


%%===========================================================================================================================================================%%

%%===========================================================================================================================================================%%


parse_disconnect_packet(Bin , Disconnect) ->
	decode_disconnect_variable_header(Bin , Disconnect).


decode_disconnect_variable_header(Bin , Disconnect) ->
	try_decode_disconnect_packet_identifier(Bin , Disconnect).


try_decode_disconnect_packet_identifier(Bin , Disconnect) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewDisconnect = Disconnect#disconnect{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewDisconnect);
		undefined ->
			exit('wrong disconnect packet identifier data')
	end.


%%============================================================================================================================================================%%

%%============================================================================================================================================================%%


parse_disconnack_packet(Bin , Disconnack) ->
	decode_disconnack_variable_header(Bin , Disconnack).


decode_disconnack_variable_header(Bin , Disconnack) ->
	try_decode_disconnack_packet_identifier(Bin , Disconnack).


try_decode_disconnack_packet_identifier(Bin , Disconnack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewDisconnack = Disconnack#disconnack{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewDisconnack);
		undefined ->
			exit('wrong disconnack packet identifier data')
	end.


%%============================================================================================================================================================%%

%%============================================================================================================================================================%%


parse_publish_packet(Bin , Publish) ->
	decode_publish_variable_header(Bin , Publish).


decode_publish_variable_header(Bin , Publish) ->
	try_decode_publish_packet_identifier(Bin , Publish).


try_decode_publish_packet_identifier(Bin , Publish) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewPublish = Publish#publish{packet_identifier = PacketIdentifier},
			decode_publish_payload(Rest , NewPublish);
		undefined ->
			exit('wrong publish packet identifier data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%

	
decode_publish_payload(Bin , Publish) ->
	try_decode_publish_source(Bin , Publish).


try_decode_publish_source(Bin , Publish) ->
	case decode_prefixed_size_data(Bin) of
		{Source , Rest} ->
			NewPublish = Publish#publish{source = Source},
			try_decode_publish_destination(Rest , NewPublish);
		undefined ->
			exit('wrong publish source data')
	end.


try_decode_publish_destination(Bin , Publish) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewPublish = Publish#publish{destination = Destination},
			try_decode_publish_message(Rest , NewPublish);
		undefined ->
			exit('wrong publish destination data')
	end.


try_decode_publish_message(Bin , Publish) ->
	case decode_prefixed_size_data(Bin) of
		{Message , Rest} ->
			NewPublish = Publish#publish{message = Message},
			end_decode_packet(Rest , NewPublish);
		undefined ->
			exit('wrong publish message data')
	end.


%%=============================================================================================================================================================%%


%%=============================================================================================================================================================%%


parse_puback_packet(Bin , Puback) ->
	decode_puback_variable_header(Bin , Puback).


decode_puback_variable_header(Bin , Puback) ->
	try_decode_puback_packet_identifier(Bin , Puback).


try_decode_puback_packet_identifier(Bin , Puback) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewPuback = Puback#puback{packet_identifier = PacketIdentifier},
			decode_puback_payload(Rest , NewPuback);
		undefined ->
			exit('wrong puback packet identifier data')
	end.


decode_puback_payload(Bin , Puback) ->
	try_decode_puback_destination(Bin , Puback).


try_decode_puback_destination(Bin , Puback) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewPuback = Puback#puback{destination = Destination},
			end_decode_packet(Rest , NewPuback);
		undefined ->
			exit('wrong puback destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_pubcomp_packet(Bin , Pubcomp) ->
	decode_pubcomp_variable_header(Bin , Pubcomp).


decode_pubcomp_variable_header(Bin , Pubcomp) ->
	try_decode_pubcomp_packet_identifier(Bin , Pubcomp).


try_decode_pubcomp_packet_identifier(Bin , Pubcomp) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewPubcomp = Pubcomp#pubcomp{packet_identifier = PacketIdentifier},
			decode_pubcomp_payload(Rest , NewPubcomp);
		undefined ->
			exit('wrong pubcomp packet identifier data')
	end.


decode_pubcomp_payload(Bin , Pubcomp) ->
	try_decode_pubcomp_source(Bin , Pubcomp).


try_decode_pubcomp_source(Bin , Pubcomp) ->
	case decode_prefixed_size_data(Bin) of
		{Source , Rest} ->
			NewPubcomp = Pubcomp#pubcomp{source = Source},
			try_decode_pubcomp_destination(Rest , NewPubcomp);
		undefined ->
			exit('wrong pubcomp source data')
	end.


try_decode_pubcomp_destination(Bin , Pubcomp) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewPubcomp = Pubcomp#pubcomp{destination = Destination},
			end_decode_packet(Rest , NewPubcomp);
		undefined ->
			exit('wrong pubcomp destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_pubcompack_packet(Bin , Pubcompack) ->
	decode_pubcompack_variable_header(Bin , Pubcompack).


decode_pubcompack_variable_header(Bin , Pubcompack) ->
	try_decode_pubcompack_packet_identifier(Bin , Pubcompack).


try_decode_pubcompack_packet_identifier(Bin , Pubcompack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewPubcompack = Pubcompack#pubcompack{packet_identifier = PacketIdentifier},
			decode_pubcompack_payload(Rest , NewPubcompack);
		undefined ->
			exit('wrong pubcompack packet identifier data')
	end.


decode_pubcompack_payload(Bin , Pubcompack) ->
	try_decode_pubcompack_destination(Bin , Pubcompack).


try_decode_pubcompack_destination(Bin , Pubcompack) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewPubcompack = Pubcompack#pubcompack{destination = Destination},
			end_decode_packet(Rest , NewPubcompack);
		undefined ->
			exit('wrong pubcompack destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_subscribe_packet(Bin , Subscribe) ->
	decode_subscribe_variable_header(Bin , Subscribe).


decode_subscribe_variable_header(Bin , Subscribe) ->
	try_decode_subscribe_packet_identifier(Bin , Subscribe).


try_decode_subscribe_packet_identifier(Bin , Subscribe) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSubscribe = Subscribe#subscribe{packet_identifier = PacketIdentifier},
			decode_subscribe_payload(Rest , NewSubscribe);
		undefined ->
			exit('wrong subscribe packet identifier data')
	end.


decode_subscribe_payload(Bin , Subscribe) ->
	try_decode_subscribe_source(Bin , Subscribe).


try_decode_subscribe_source(Bin , Subscribe) ->
	case decode_prefixed_size_data(Bin) of
		{Source , Rest} ->
			NewSubscribe = Subscribe#subscribe{source = Source},
			try_decode_subscribe_destination(Rest , NewSubscribe);
		undefined ->
			exit('wrong subscribe source data')
	end.


try_decode_subscribe_destination(Bin , Subscribe) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewSubscribe = Subscribe#subscribe{destination = Destination},
			end_decode_packet(Rest , NewSubscribe);
		undefined ->
			exit('wrong subscribe destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_suback_packet(Bin , Suback) ->
	decode_suback_variable_header(Bin , Suback).


decode_suback_variable_header(Bin , Suback) ->
	try_decode_suback_packet_identifier(Bin , Suback).


try_decode_suback_packet_identifier(Bin , Suback) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSuback = Suback#suback{packet_identifier = PacketIdentifier},
			decode_suback_payload(Rest , NewSuback);
		undefined ->
			exit('wrong suback packet identifier data')
	end.


decode_suback_payload(Bin , Suback) ->
	try_decode_suback_destination(Bin , Suback).


try_decode_suback_destination(Bin , Suback) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewSuback = Suback#suback{destination = Destination},
			end_decode_packet(Rest , NewSuback);
		undefined ->
			exit('wrong suback destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_subcnl_packet(Bin , Subcnl) ->
	decode_subcnl_variable_header(Bin , Subcnl).


decode_subcnl_variable_header(Bin , Subcnl) ->
	try_decode_subcnl_packet_identifier(Bin , Subcnl).


try_decode_subcnl_packet_identifier(Bin , Subcnl) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSubcnl = Subcnl#subcnl{packet_identifier = PacketIdentifier},
			decode_subcnl_payload(Rest , NewSubcnl);
		undefined ->
			exit('wrong subcnl packet identifier data')
	end.


decode_subcnl_payload(Bin , Subcnl) ->
	try_decode_subcnl_destination(Bin , Subcnl).


try_decode_subcnl_destination(Bin , Subcnl) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewSubcnl = Subcnl#subcnl{destination = Destination},
			end_decode_packet(Rest , NewSubcnl);
		undefined ->
			exit('wrong subcnl destination data')

	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_subcnlack_packet(Bin , Subcnlack) ->
	decode_subcnlack_variable_header(Bin , Subcnlack).


decode_subcnlack_variable_header(Bin , Subcnlack) ->
	try_decode_subcnlack_packet_identifier(Bin , Subcnlack).


try_decode_subcnlack_packet_identifier(Bin , Subcnlack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSubcnlack = Subcnlack#subcnlack{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewSubcnlack);
		undefined ->
			exit('wrong subcnlack packet identifier data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_subresp_packet(Bin , Subresp) ->
	decode_subresp_variable_header(Bin , Subresp).


decode_subresp_variable_header(Bin , Subresp) ->
	try_decode_subresp_packet_identifier(Bin , Subresp).


try_decode_subresp_packet_identifier(Bin , Subresp) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSubresp = Subresp#subresp{packet_identifier = PacketIdentifier},
			decode_subresp_payload(Rest , NewSubresp);
		undefined ->
			exit('wrong subresp packet identifier data')
	end.


decode_subresp_payload(Bin , Subresp) ->
	try_decode_subresp_source(Bin , Subresp).


try_decode_subresp_source(Bin , Subresp) ->
	case decode_prefixed_size_data(Bin) of
		{Source , Rest} ->
			NewSubresp = Subresp#subresp{source = Source},
			try_decode_subresp_destination(Rest , NewSubresp);
		undefined ->
			exit('wrong subresp source data')
	end.


try_decode_subresp_destination(Bin , Subresp) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewSubresp = Subresp#subresp{destination = Destination},
			try_decode_subresp_response(Rest , NewSubresp);
		undefined ->
			exit('wrong subresp destination data')
	end.


try_decode_subresp_response(Bin , Subresp) ->
	case decode_response(Bin) of
		{Response , Rest} ->
			NewSubresp = Subresp#subresp{response = Response},
			end_decode_packet(Rest , NewSubresp);
		undefined ->
			exit('wrong subresp response data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_subrespack_packet(Bin , Subrespack) ->
	decode_subrespack_variable_header(Bin , Subrespack).


decode_subrespack_variable_header(Bin , Subrespack) ->
	try_decode_subrespack_packet_identifier(Bin , Subrespack).


try_decode_subrespack_packet_identifier(Bin , Subrespack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewSubrespack = Subrespack#subrespack{packet_identifier = PacketIdentifier},
			decode_subrespack_payload(Rest , NewSubrespack);
		undefined ->
			exit('wrong subrespack packet identifier data')
	end.


decode_subrespack_payload(Bin , Subrespack) ->
	try_decode_subrespack_destination(Bin , Subrespack).


try_decode_subrespack_destination(Bin , Subrespack) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewSubrespack = Subrespack#subrespack{destination = Destination},
			end_decode_packet(Rest , NewSubrespack);
		undefined ->
			exit('wrong subrespack destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_unsubscribe_packet(Bin , Unsubscribe) ->
	decode_unsubscribe_variable_header(Bin , Unsubscribe).


decode_unsubscribe_variable_header(Bin , Unsubscribe) ->
	try_decode_unsubscribe_packet_identifier(Bin , Unsubscribe).


try_decode_unsubscribe_packet_identifier(Bin , Unsubscribe) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewUnsubscribe = Unsubscribe#unsubscribe{packet_identifier = PacketIdentifier},
			decode_unsubscribe_payload(Rest , NewUnsubscribe);
		undefined ->
			exit('wrong unsubscribe packet identifier data')
	end.


decode_unsubscribe_payload(Bin , Unsubscribe) ->
	try_decode_unsubscribe_destination(Bin , Unsubscribe).


try_decode_unsubscribe_destination(Bin , Unsubscribe) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewUnsubscribe = Unsubscribe#unsubscribe{destination = Destination},
			end_decode_packet(Rest , NewUnsubscribe);
		undefined ->
			exit('wrong unsubscribe destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_unsuback_packet(Bin , Unsuback) ->
	decode_unsuback_variable_header(Bin , Unsuback).


decode_unsuback_variable_header(Bin , Unsuback) ->
	try_decode_unsuback_packet_identifier(Bin , Unsuback).


try_decode_unsuback_packet_identifier(Bin , Unsuback) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewUnsuback = Unsuback#unsuback{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewUnsuback);
		undefined ->
			exit('wrong unsuback packet identifier data')
	end.



%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_block_packet(Bin , Block) ->
	decode_block_variable_header(Bin , Block).


decode_block_variable_header(Bin , Block) ->
	try_decode_block_packet_identifier(Bin , Block).


try_decode_block_packet_identifier(Bin , Block) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewBlock = Block#block{packet_identifier = PacketIdentifier},
			decode_block_payload(Rest , NewBlock);
		undefined ->
			exit('wrong block packet identifier data')
	end.


decode_block_payload(Bin , Block) ->
	try_decode_block_destination(Bin , Block).


try_decode_block_destination(Bin , Block) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewBlock = Block#block{destination = Destination},
			end_decode_packet(Rest , NewBlock);
		undefined ->
			exit('wrong block destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_blockack_packet(Bin , Blockack) ->
	decode_blockack_variable_header(Bin , Blockack).


decode_blockack_variable_header(Bin , Blockack) ->
	try_decode_blockack_packet_identifier(Bin , Blockack).


try_decode_blockack_packet_identifier(Bin , Blockack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewBlockack = Blockack#blockack{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewBlockack);
		undefined ->
			exit('wrong blockack packet identifier data')
	end.	


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_unblock_packet(Bin , Unblock) ->
	decode_unblock_variable_header(Bin , Unblock).


decode_unblock_variable_header(Bin , Unblock) ->
	try_decode_unblock_packet_identifier(Bin , Unblock).


try_decode_unblock_packet_identifier(Bin , Unblock) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewUnblock = Unblock#unblock{packet_identifier = PacketIdentifier},
			decode_unblock_payload(Rest , NewUnblock);
		undefined ->
			exit('wrong unblock packet identifier data')
	end.


decode_unblock_payload(Bin , Unblock) ->
	try_decode_unblock_destination(Bin , Unblock).


try_decode_unblock_destination(Bin , Unblock) ->
	case decode_prefixed_size_data(Bin) of
		{Destination , Rest} ->
			NewUnblock = Unblock#unblock{destination = Destination},
			end_decode_packet(Rest , NewUnblock);
		undefined ->
			exit('wrong unblock destination data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_unblockack_packet(Bin , Unblockack) ->
	decode_unblockack_variable_header(Bin , Unblockack).


decode_unblockack_variable_header(Bin , Unblockack) ->
	try_decode_unblockack_packet_identifier(Bin , Unblockack).


try_decode_unblockack_packet_identifier(Bin , Unblockack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewUnblockack = Unblockack#unblockack{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewUnblockack);
		undefined ->
			exit('wrong unblockack packet identifier data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_ping_packet(Bin , Ping) ->
	decode_ping_variable_header(Bin , Ping).


decode_ping_variable_header(Bin , Ping) ->
	try_decode_ping_packet_identifier(Bin , Ping).


try_decode_ping_packet_identifier(Bin , Ping) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewPing = Ping#ping{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewPing);
		undefined ->
			exit('wrong ping packet identifier data')
	end.


%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


parse_pingack_packet(Bin , Pingack) ->
	decode_pingack_variable_header(Bin , Pingack).


decode_pingack_variable_header(Bin , Pingack) ->
	try_decode_pingack_packet_identifier(Bin , Pingack).


try_decode_pingack_packet_identifier(Bin , Pingack) ->
	case decode_packet_identifier(Bin) of
		{PacketIdentifier , Rest} ->
			NewPingack = Pingack#pingack{packet_identifier = PacketIdentifier},
			end_decode_packet(Rest , NewPingack);
		undefined ->
			exit('wrong pingack packet identifier data')
	end.



%%=============================================================================================================================================================%%

%%=============================================================================================================================================================%%


-spec decode_prefixed_size_data(binary()) -> {nonempty_binary() , binary()} | undefined.
decode_prefixed_size_data(Bin) ->
	try_decode_prefixed_size_data_length(Bin).


try_decode_prefixed_size_data_length(Bin) ->
	case moqalib_checker:check_remaining_length(Bin , 2) of
		true ->
			try_decode_prefixed_size_data(Bin);
		_ ->
			undefined
	end.


try_decode_prefixed_size_data(<<DataLength:16 , Rest/binary>>) ->
	case moqalib_checker:check_remaining_length(Rest , DataLength) of
		true ->
			decode_prefixed_size_data_ok(Rest , DataLength);
		_ ->
			undefined
	end.


decode_prefixed_size_data_ok(Bin , DataLength) ->
	<<Data:DataLength/binary , Rest/binary>> = Bin,
	{Data , Rest}.


-spec end_decode_packet(binary() , moqalib_checker:moqa_data()) -> moqalib_checker:moqa_data().
end_decode_packet(<<>> , MoqaData) ->
	MoqaData;


end_decode_packet(_Bin , _Data) ->
	exit('unexpected additional data').


-spec decode_packet_identifier(binary()) -> {pos_integer() , binary()} | undefined.
decode_packet_identifier(Bin) ->
	case moqalib_checker:check_remaining_length(Bin , 2) of
		true ->
			decode_packet_identifier_ok(Bin);
		_ ->
			undefined
	end.


decode_packet_identifier_ok(<<PacketIdentifier:16 , Rest/binary>>) ->
	{PacketIdentifier , Rest}.


-spec decode_number_of_updates(binary()) -> {pos_integer() , binary()} | undefined.
decode_number_of_updates(Bin) ->
	case moqalib_checker:check_remaining_length(Bin , 2) of
		true ->
			decode_number_of_updates_ok(Bin);
		_ ->
			undefined
	end.


decode_number_of_updates_ok(<<NumberOfUpdates:16 , Rest/binary>>) ->
	{NumberOfUpdates , Rest}.


-spec decode_response(binary()) -> { 0 | 1 , binary()} | undefined.
decode_response(Bin) ->
	case moqalib_checker:check_remaining_length(Bin , 1) of
		true ->
			decode_response_ok(Bin);
		_ ->
			undefined
	end.


decode_response_ok(<<Response:8 , Rest/binary>>) ->
	{Response , Rest}.


-spec decode_remaining_length(binary()) -> {pos_integer() , binary} | undefined.
decode_remaining_length(Data) ->
	case moqalib_checker:check_remaining_length(Data , 1) of
		true ->
			get_remaining_length(Data , 1 , 0);
		_ ->
			undefined
	end.


get_remaining_length(<<Byte:8 , Rest/binary>> , Multiplier , Value) ->
	NewValue = Value + ((Byte band 127) * Multiplier),
	NewMultiplier = Multiplier * 128,
	case (NewMultiplier > 128 * 128 * 128) of
		true ->
			undefined;
		_ ->
			case (Byte band 128) of
				0 ->
					{NewValue , Rest};
				_ ->
					case moqalib_checker:check_remaining_length(Rest , 1) of
						true ->
							get_remaining_length(Rest , NewMultiplier , NewValue);
						_ ->
							undefined
					end
			end
	end.	


-spec decode_updates(map()) -> map().
decode_updates(Updates) when is_map(Updates) ->
	Fun = fun(Key , Value , Acc) ->
		{NewKey , NewValue} = case Value of
					{ _ , _ } ->
						{Key , decode_date(Value)};
					_ ->
						{Key , Value}
				      end,
		Acc#{NewKey => NewValue}
	      end,
	maps:fold(Fun , #{} , Updates).


decode_date(Date) ->
	NewDate = calendar:universal_time_to_local_time(Date),
	{ { _ , Month , Day } , { Hour , Minute , _ } } = NewDate,
	StrMonth = integer_to_list(Month),
	StrDay = integer_to_list(Day),
	StrHour = integer_to_list(Hour),
	StrMinute = integer_to_list(Minute),
	"last seen : " ++ StrDay ++ "/" ++ StrMonth ++ " at " ++ StrHour ++ "h" ++ StrMinute.

			




