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


-module(moqalib_encoder).


-export([encode_packet/1]).
-export([encode_ack/1]).
-export([encode_ack/2]).


-include("../include/moqa_data.hrl").


-spec encode_packet(moqalib_checker:moqa_data()) -> nonempty_binary().
encode_packet(#roster{updates = Updates}) ->
	NumberOfUpdates = maps:size(Updates),	
	case NumberOfUpdates of
		0 ->
			RemainingLength = 1 + 2,
			{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),
			<< 0:8 , 
			   BinRemainingLength:BinRemainingLengthLength/binary ,
			   1:8 , 
			   NumberOfUpdates:16
			>> ;
		_ ->
			{BinUpdates , BinUpdatesLength} = encode_updates(Updates),
			NumberOfUpdates	= maps:size(Updates),
			RemainingLength = 1 + 2 + BinUpdatesLength,
			{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

			<< 0:8 ,
			   BinRemainingLength:BinRemainingLengthLength/binary , 
			   1:8 , 
			   NumberOfUpdates:16 ,
			   BinUpdates:BinUpdatesLength/binary
			>> 
	end;


encode_packet(#subscriptions{updates = Updates}) ->
	NumberOfUpdates = maps:size(Updates),
	case NumberOfUpdates of
		0 ->
			NumberOfUpdates = 0,
			RemainingLength = 1 + 2,
			{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

			<< 0:8 ,
			   BinRemainingLength:BinRemainingLengthLength/binary , 
			   2:8 , 
			   NumberOfUpdates:16
			>> ;
		_ ->
			{BinUpdates , BinUpdatesLength} = encode_updates(Updates),
			NumberOfUpdates = maps:size(Updates),
			RemainingLength = 1 + 2 + BinUpdatesLength,
			{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

			<< 0:8 ,
			   BinRemainingLength:BinRemainingLengthLength/binary ,
			   2:8 ,
			   NumberOfUpdates:16 ,
			   BinUpdates:BinUpdatesLength/binary
			>> 
	end;


encode_packet(#signup{
		packet_identifier = PacketIdentifier , 
		keep_alive = KeepAlive , 
		username = Username , 
		password = Password
		})
	when is_integer(PacketIdentifier) , 
	     is_integer(KeepAlive),
	     is_binary(Username),
	     is_binary(Password) 
		->
	UsernameLength = byte_size(Username),
	PasswordLength = byte_size(Password),
	RemainingLength = 2 + 8 + (2 + UsernameLength) + (2 + PasswordLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 1:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16,
	   5:16 , 
	   "MOQA", 
	   KeepAlive:16 , 
	   UsernameLength:16 , Username:UsernameLength/binary , 
	   PasswordLength:16 , Password:PasswordLength/binary 
	>> ;


encode_packet(#signack{
		packet_identifier = PacketIdentifier , 
		response = Response
		}) 
	when is_integer(PacketIdentifier),
	     is_integer(Response) 
		->
	RemainingLength = 2 + 1,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 2:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16,
	   Response:8
	>> ;


encode_packet(#deactivate{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 3:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16
	>> ;


encode_packet(#deactivack{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 4:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16
	>> ;


encode_packet(#connect{
		packet_identifier = PacketIdentifier , 
		keep_alive = KeepAlive , 
		username = Username , 
		password = Password
		}) 
	when is_integer(PacketIdentifier),
	     is_integer(KeepAlive),
	     is_binary(Username),
	     is_binary(Password) 
		->
	UsernameLength = byte_size(Username),
	PasswordLength = byte_size(Password),
	RemainingLength = 2 + 10 + (2 + UsernameLength) + (2 + PasswordLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 5:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16,
	   4:16 , 
	   "MOQA" , 
	   4:8 ,
	   1:1 , 1:1 , 0:1 , 0:2 , 0:1 , 0:1 , 0:1 ,
	   KeepAlive:16 , 
	   UsernameLength:16 , Username:UsernameLength/binary , 
	   PasswordLength:16 , Password:PasswordLength/binary
	>> ;


encode_packet(#connack{
		packet_identifier = PacketIdentifier , 
		response = Response
		}) 
	when is_integer(PacketIdentifier),
	     is_integer(Response) 
		-> 
	RemainingLength = 2 + 1,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 6:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   Response:8
	>> ;


encode_packet(#disconnect{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 7:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16
	>> ;


encode_packet(#disconnack{
		packet_identifier = PacketIdentifier
		}) 
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 8:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16
	>> ;


encode_packet(#publish{
		packet_identifier = PacketIdentifier , 
		source = Source , 
		destination = Destination , 
		message = Message
		})
	when is_integer(PacketIdentifier),
	     is_binary(Source),
	     is_binary(Destination),
	     is_binary(Message) 
		->
	SourceLength = byte_size(Source),
	DestinationLength = byte_size(Destination),
	MessageLength = byte_size(Message),
	RemainingLength = 2 + (2 + SourceLength) + (2 + DestinationLength) + (2 + MessageLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 9:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   SourceLength:16 , Source:SourceLength/binary ,
	   DestinationLength:16 , Destination:DestinationLength/binary ,
	   MessageLength:16 , Message:MessageLength/binary
	>> ;


encode_packet(#puback{
		packet_identifier = PacketIdentifier ,  
		destination = Destination
		})
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 10:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#pubcomp{
		packet_identifier = PacketIdentifier , 
		source = Source , 
		destination = Destination
		}) 
	when is_integer(PacketIdentifier),
	     is_binary(Source),
	     is_binary(Destination) 
		->
	SourceLength = byte_size(Source),
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + SourceLength) + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 11:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   SourceLength:16 , Source:SourceLength/binary ,
	   DestinationLength:16 , Destination:DestinationLength/binary 
	>> ;


encode_packet(#pubcompack{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		})
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),
	<< 12:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#subscribe{
		packet_identifier = PacketIdentifier , 
		source = Source , 
		destination = Destination
		})
	when is_integer(PacketIdentifier),
	     is_binary(Source),
	     is_binary(Destination) 
		->
	SourceLength = byte_size(Source),
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + SourceLength) + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 13:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16 ,
	   SourceLength:16 , Source:SourceLength/binary ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#suback{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		})
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 14:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16 ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#subcnl{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		})
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 15:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16 , 
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#subcnlack{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<<16:8 , 
	  BinRemainingLength:BinRemainingLengthLength/binary , 
	  PacketIdentifier:16
	>> ; 
	   	

encode_packet(#subresp{
		packet_identifier = PacketIdentifier , 
		source = Source , 
		destination = Destination , 
		response = Response
		}) 
	when is_integer(PacketIdentifier),
	     is_binary(Source),
	     is_binary(Destination),
	     is_integer(Response) 
		->
	SourceLength = byte_size(Source),
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + SourceLength) + (2 + DestinationLength) + 1,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 17:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   SourceLength:16 , Source:SourceLength/binary ,
	   DestinationLength:16 , Destination:DestinationLength/binary , 
	   Response:8
	>> ;


encode_packet(#subrespack{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		}) 
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 18:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16 ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#unsubscribe{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		}) 
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 19:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16 , 
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#unsuback{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2 ,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 20:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary,
	   PacketIdentifier:16
	>> ;


encode_packet(#block{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		})		
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 21:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16 ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#blockack{
		packet_identifier = PacketIdentifier
		}) 
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 22:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 
	>> ;


encode_packet(#unblock{
		packet_identifier = PacketIdentifier , 
		destination = Destination
		})
	when is_integer(PacketIdentifier),
	     is_binary(Destination) 
		->
	DestinationLength = byte_size(Destination),
	RemainingLength = 2 + (2 + DestinationLength),
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 23:8 , 
	   BinRemainingLength:BinRemainingLengthLength/binary ,
	   PacketIdentifier:16 ,
	   DestinationLength:16 , Destination:DestinationLength/binary
	>> ;


encode_packet(#unblockack{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 24:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16
	>> ;


encode_packet(#ping{
		packet_identifier = PacketIdentifier
		})
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<<25:8 , 
	  BinRemainingLength:BinRemainingLengthLength/binary , 
	  PacketIdentifier:16
	>> ;


encode_packet(#pingack{
		packet_identifier = PacketIdentifier
		}) 
	when is_integer(PacketIdentifier) 
		->
	RemainingLength = 2,
	{BinRemainingLength , BinRemainingLengthLength} = encode_remaining_length(RemainingLength),

	<< 26:8 ,
	   BinRemainingLength:BinRemainingLengthLength/binary , 
	   PacketIdentifier:16
	>> ;


encode_packet(_Packet) ->
	exit('wrong moqa data entry').


-spec encode_remaining_length(integer()) -> nonempty_binary().
encode_remaining_length(RemainingLength) ->
	encode_remaining_length(RemainingLength , <<>> , 0).


encode_remaining_length(0 , BinRemainingLength , BinRemainingLengthLength ) ->
	{BinRemainingLength , BinRemainingLengthLength};


encode_remaining_length(X , Acc , NumberOfBytes) ->
	N = X rem 128,
	Y = X div 128,
	EncodedByte = case (Y > 0) of
				true -> 
					N bor 128;
				_ -> 
					N
		      end,
	encode_remaining_length(Y , <<Acc/binary , EncodedByte:8>> , NumberOfBytes + 1).


-spec encode_ack(moqalib_checker:moqa_data()) -> moqalib_checker:moqa_data().
encode_ack(#signup{packet_identifier = PacketIdentifier} , Response) ->
	encode_packet(#signack{
			packet_identifier = PacketIdentifier ,
			response = Response
			});


encode_ack(#connect{packet_identifier = PacketIdentifier} , Response) ->
	encode_packet(#connack{
			packet_identifier = PacketIdentifier ,
			response = Response
			}).


encode_ack(#deactivate{packet_identifier = PacketIdentifier}) ->
	encode_packet(#deactivack{
			packet_identifier = PacketIdentifier 
			});


encode_ack(#disconnect{packet_identifier = PacketIdentifier}) ->
	encode_packet(#disconnack{
			packet_identifier = PacketIdentifier  
			});


encode_ack(#publish{packet_identifier = PacketIdentifier , source = Source}) ->
	encode_packet(#puback{
			packet_identifier = PacketIdentifier ,
			destination = Source
			});


encode_ack(#pubcomp{packet_identifier = PacketIdentifier , source = Source}) ->
	encode_packet(#pubcompack{
			packet_identifier = PacketIdentifier ,
			destination = Source
			});


encode_ack(#subscribe{packet_identifier = PacketIdentifier , source = Source}) ->
	encode_packet(#suback{
			packet_identifier = PacketIdentifier ,
			destination = Source
			});


encode_ack(#subcnl{packet_identifier = PacketIdentifier}) ->
	encode_packet(#subcnlack{
			packet_identifier = PacketIdentifier 
			});


encode_ack(#subresp{packet_identifier = PacketIdentifier , source = Source}) ->
	encode_packet(#subrespack{
			packet_identifier = PacketIdentifier ,
			destination = Source
			});


encode_ack(#unsubscribe{packet_identifier = PacketIdentifier}) ->
	encode_packet(#unsuback{
			packet_identifier = PacketIdentifier 
			});


encode_ack(#block{packet_identifier = PacketIdentifier}) ->
	encode_packet(#blockack{
			packet_identifier = PacketIdentifier 
			});


encode_ack(#unblock{packet_identifier = PacketIdentifier}) ->
	encode_packet(#unblockack{
			packet_identifier = PacketIdentifier 
			});


encode_ack(#ping{packet_identifier = PacketIdentifier}) ->
	encode_packet(#pingack{
			packet_identifier = PacketIdentifier 
			}).


-spec encode_updates(map()) -> {nonempty_binary() , pos_integer()}.
encode_updates(Updates) ->
	Fun = fun(Key , Value , {Acc , Length}) ->
		BinValue = case Value of
				{ { Year , Month , Day } , { Hour , Minute , Second } } ->
					<<Year:16 , Month:8 , Day:8 , Hour:8 , Minute:8 , Second:8>>;
				_ ->
					Value
			    end,
		KeyLength = byte_size(Key),
		BinValueLength = byte_size(BinValue),

		NewAcc = << Acc/binary , 
			    KeyLength:16 , Key:KeyLength/binary , 
			    BinValueLength:16 , BinValue:BinValueLength/binary
			 >> ,

		NewLength = Length + (2 + KeyLength) + (2 + BinValueLength),
		{NewAcc , NewLength}
	     end,
	maps:fold(Fun , { <<>> , 0 } , Updates).


