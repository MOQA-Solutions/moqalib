{ application , moqalib ,
 
	[

		{description, "encoding and decoding library for moqa application"} ,

		{vsn, "0.1.0"} ,
  
		{applications ,  [kernel , stdlib]} ,

		{modules, ['moqalib_encoder' , 'moqalib_parser' , 'moqalib_checker']}

 	]

}.
