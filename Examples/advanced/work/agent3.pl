
:-use_module(library(system)).

:-dynamic left/1.

:-dynamic right/1.

:-dynamic myid/1.

eve(init(var_Id)):-print('Agent init->'),print('Id:'),print(var_Id),nl,assert(myid(var_Id)),var_L is(var_Id-1)mod 4,assert(left(var_L)),var_R is(var_Id+1)mod 4,assert(right(var_R)),print('dx:'),print(var_R),print(' sx:'),print(var_L),nl,start(var_Id).

start(var_Id):-right(var_RightAgent),var_X=agent,getAgentName(var_X,var_RightAgent,var_Result),format(user_output,'election(~w,~w,~w,~w)',[var_Id,var_Id,0,1]),nl,a(message(var_Result,send_message(election([var_Id,var_Id,0,1]),var_Me))).
