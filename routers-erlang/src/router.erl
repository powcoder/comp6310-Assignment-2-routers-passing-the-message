https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-module(router).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

% Add anything else you'd like to keep track of between calls to this record.
-record(state, {neighbours = [] :: [pid()]}).

-record(envelope, {dest     :: pid(),
                   hops = 0 :: non_neg_integer(),
                   message  :: term()}).

%% API

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({neighbours, Neighbours}, _From, State) ->
    {reply, ok, State#state{neighbours=Neighbours}};
handle_call({test, Dest}, From, State) ->
    Envelope = #envelope{dest = Dest, hops = 0, message = {test, From}},
    {reply, ok, State1} = handle_call(Envelope, From, State),
    {noreply, State1};
handle_call(E, _From, State) when is_record(E, envelope) ->
    Self = self(),
    case E#envelope.dest of
        Self ->
            case E#envelope.message of
                {test, From} ->
                    gen_server:reply(From, {ok, E#envelope.hops}),
                    {reply, ok, State}
                % Deal with other kinds of message by adding cases here
            end;
        Dest ->
            % Deal with forwarding a message to another node here
            %forward(Next, E),
            {reply, ok, State}
    end.

% You can ignore this - "casts" are gen_server's asynchronous calls.
% For this assignment, passing messages between different routers with
% asynchronous casts is cheating - stick to synchronous calls (when you're
% communicating between routers, that is. For worker tasks or similar that you
% create, go nuts.)
handle_cast(_Request, State) ->
    {noreply, State}.

%% internal functions

-spec forward(pid(), #envelope{}) -> term().
forward(Next, Envelope) ->
    gen_server:call(Next, Envelope#envelope{hops = Envelope#envelope.hops + 1}).