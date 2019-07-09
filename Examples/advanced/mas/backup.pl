/* This type of agent partecipate to the leader election algorithm. */
/* BASATA SU BACKTRACKING MA NON FUNZIONA. */
:- use_module(library(random)).
:- use_module(library(system)).

/* left and right are dynamic clauses used to remember which agents are to the left and right of the current. */
:- dynamic left/1.
:- dynamic right/1.
:- dynamic myid/1.

:- dynamic replyLeft/1.
:- dynamic replyRight/1.

/* This method is invoked when the master node send an init message. */
initE(Id) :> print('Agent init->'), 
            print('Id:'), print(Id), nl,
            assert(myid(Id)),
            L is Id-1, assert(left(L)),
            R is Id+1, assert(right(R)),
            start(Id).

/* This method is called only one during the initialization, each each sleeps for a random time to simulate asynchronous wake-up. */
start(Id) :> random(1,16,WaitTime), 
          sleep(WaitTime),
          print('awake'),
          left(LeftAgent),
          X=agent, getAgentName(X,LeftAgent,Result),
          messageA(Result,send_message(election(Id, Id, 0, 1), Me)),
          right(RightAgent),
          X=agent, getAgentName(X,RightAgent,Result), 
          messageA(Result,send_message(election(Id, Id, 0, 1), Me)). 

/* This method is used to get the user agent name from left() and right() that is used for send_message primitive. */
getAgentName(AtomicPrefix ,NumericSuffix ,Concatenation) :-
          name(AtomicPrefix, Pfx),
          name(NumericSuffix, Sfx),
          append(Pfx,Sfx,Codes),
          name(Concatenation,Codes).


/* React to election msg, if the id is greater than this node's id and this node is in the boundary, forward the msg. */
electionE(Id, SenderId, Phase, Distance) :> 
          myid(MyId),
          Id > MyId,
          Distance < 2**Phase,
          NewDist is Distance + 1,          
          forwardElection(Id, MyId, Phase, NewDist).

/* React to election msg, if the id is greater than this node's id and this node is the boundary, send reply msg. */
electionE(Id, SenderId, Phase, Distance) :> 
          myid(MyId),
          Id > MyId,
          Distance == 2**Phase,
          sendReply(Id, MyId, Phase).

/* If the election msg contains the current id, this means that this node is the leader. */
electionE(Id, SenderId, Phase, Distance) :> 
          myid(MyId),
          Id == MyId,
          messageA(agent1, send_message(leader(MyId), Me)).

/* If the election comes from the node to the left, forward it to the node to the right. */
forwardElection(Id, SenderId, Phase, NewDist) :-
          left(LeftAgent),
          SenderId == LeftAgent,
          right(RightAgent),
          X=agent, getAgentName(X, RightAgent, Result), 
          myid(MyId),
          messageA(Result,send_message(election(Id, MyId, Phase, NewDist), Me)). 

/* If the election comes from the node to the right, forward it to the node to the left. */
forwardElection(Id, SenderId, Phase, NewDist) :-
          right(RightAgent),
          SenderId == RightAgent,
          left(LeftAgent),
          X=agent, getAgentName(X, LeftAgent, Result), 
          myid(MyId),
          messageA(Result,send_message(election(Id, MyId, Phase, NewDist), Me)). 

sendReply(Id, SenderId, Phase) :- 
          left(LeftAgent),
          SenderId == LeftAgent,
          X=agent, getAgentName(X, LeftAgent, Result), 
          myid(MyId),
          messageA(Result,send_message(reply(Id, MyId, Phase), Me)).

sendReply(Id, SenderId, Phase) :- 
          right(RightAgent),
          SenderId == RightAgent,
          X=agent, getAgentName(X, RightAgent, Result), 
          myid(MyId),
          messageA(Result,send_message(reply(Id, MyId, Phase), Me)).

replyE(Id, SenderId, Phase) :> 
          myid(MyId),
          Id =\= MyId,
          forwardReply(Id, SenderId, Phase).

replyE(Id, SenderId, Phase) :> 
          myid(MyId),
          Id == MyId,
          left(LeftAgent),
          LeftAgent == SenderId,
          assert(replyLeft(LeftAgent)),
          right(RightAgent),
          replyRight(ReplyRight),
          RightAgent == ReplyRight,
          goNextPhase(Phase).

replyE(Id, SenderId, Phase) :> 
          myid(MyId),
          Id == MyId,
          right(RightAgent),
          RightAgent == SenderId,
          assert(replyRight(RightAgent)),
          left(LeftAgent),
          replyLeft(ReplyLeft),
          LeftAgent == ReplyLeft,
          goNextPhase(Phase).

/* If the reply comes from the node to the left, forward it to the node to the right. */
forwardReply(Id, SenderId, Phase) :- 
          left(LeftAgent),
          SenderId == LeftAgent,
          right(RightAgent),
          X=agent, getAgentName(X,RightAgent,Result), 
          myid(MyId),
          messageA(Result,send_message(reply(Id, MyId, Phase),Me)). 

/* If the reply comes from the node to the right, forward it to the node to the left. */
forwardReply(Id, SenderId, Phase) :- 
          right(RightAgent),
          SenderId == RightAgent,
          left(LeftAgent),
          X=agent, getAgentName(X,LeftAgent,Result), 
          myid(MyId),
          messageA(Result,send_message(reply(Id, MyId, Phase),Me)). 

/* When an agent recevies both replies, it can proceed to the next phase. */
goNextPhase(Phase) :-
          retract(replyLeft(_)),
          retract(replyRight(_)),
          NextPhase is Phase + 1,
          left(LeftAgent), right(RightAgent), myid(MyId),
          X=agent, getAgentName(X,LeftAgent,Result),
          messageA(Result,send_message(election(MyId, MyId, NextPhase, 1),Me)),
          right(RightAgent),
          X=agent, getAgentName(X,RightAgent,Result), 
          messageA(Result,send_message(election(MyId, MyId, NextPhase, 1),Me)). 

/*
* Election and Reply message structure
electionE(Id, Phase, Distance) :- 
          Id > MyId,
          Distance < 2**Phase,
          NewDist is Distance + 1,
          send_message(election(Id, Phase, NewDist).

electionE(Id, Phase, Distance) :- 
          Id > MyId,
          Distance is 2**Phase,
          send_message(reply(Id, Phase)).

electionE(Id, Phase, Distance) :- 
          MyId is Id,
          annuncia leader (magari messaggio broadcast dall'agent1?)

replyE(Id, Phase) :-
          MyId =\= Id,
          send_message(reply(Id, Phase)).

replyE(Id, Phase) :-
          if già ricevuto reply,
          NextPhase is Phase +1,
          send_message(election(Id, NextPhase, 1)).
*/