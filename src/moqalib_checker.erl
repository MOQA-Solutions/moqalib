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


-module(moqalib_checker).


-export([check_binary_data/1]).
-export([check_remaining_length/2]).
-export([check_waiting_ack/2]).


-include("../include/moqa_data.hrl").


-type moqa_data() :: #state{} |

		     #roster{} |

		     #subscriptions{} |

		     #signup{} |

		     #signack{} |

		     #deactivate{} |

		     #deactivack{} |

		     #connect{} |

  		     #connack{} |

		     #disconnect{} |

		     #disconnack{} |

		     #publish{} |

		     #puback{} |

		     #pubcomp{} |

		     #pubcompack{} |

		     #subscribe{} |

		     #suback{} |

		     #subcnl{} |

		     #subcnlack{} |

		     #subresp{} |

		     #subrespack{} |

	      	     #unsubscribe{} |

		     #unsuback{} |

		     #block{} |

		     #blockack{} |

		     #unblock{} |

		     #unblockack{} |

		     #ping{} |

		     #pingack{}.


-spec check_binary_data(bitstring()) -> boolean().
check_binary_data(Data) when is_binary(Data) ->
	true;


check_binary_data(_Data) ->
	false.


-spec check_remaining_length(binary() , integer()) -> boolean().
check_remaining_length(Data , Length) ->
	DataSize = byte_size(Data),
	DataSize >= Length.


-spec check_waiting_ack(moqa_data() , moqa_data()) -> boolean().
check_waiting_ack(#signack{packet_identifier = PacketIdentifier},
		  #signup{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#deactivack{packet_identifier = PacketIdentifier},
		  #deactivate{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#connack{packet_identifier = PacketIdentifier},
		  #connect{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#disconnack{packet_identifier = PacketIdentifier},
		  #disconnect{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#puback{packet_identifier = PacketIdentifier},
		  #publish{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#pubcompack{packet_identifier = PacketIdentifier},
		  #pubcomp{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#suback{packet_identifier = PacketIdentifier},
		  #subscribe{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#subcnlack{packet_identifier = PacketIdentifier},
		  #subcnl{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#subrespack{packet_identifier = PacketIdentifier},
		  #subresp{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#unsuback{packet_identifier = PacketIdentifier},
		  #unsubscribe{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#blockack{packet_identifier = PacketIdentifier},
		  #block{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#unblockack{packet_identifier = PacketIdentifier},
		  #unblock{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack(#pingack{packet_identifier = PacketIdentifier},
		  #ping{packet_identifier = PacketIdentifier}) ->
	true;


check_waiting_ack( _ , _ ) ->
	false.


