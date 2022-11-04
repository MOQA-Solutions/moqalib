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


-module(test_subscriptions_packet).


-export([init/0]).


-include("../include/moqa_data.hrl").
-include("../include/client_info_data.hrl").


init() ->
	file:set_cwd("../src"),
	Updates =#{
		list_to_binary("user1") => ?BININ,
		list_to_binary("user2") => ?BINOUT,
		list_to_binary("user3") => ?BINUNDEFINED
		},
	Subscriptions =#subscriptions{
				updates = Updates
		},
	EncodedData = moqalib_encoder:encode_packet(Subscriptions),
	DecodedData = moqalib_parser:parse(EncodedData),
	file:set_cwd("../test"),
	DecodedData.







