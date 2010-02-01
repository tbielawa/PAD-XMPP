%%% File    : banker.erl
%%% Author  : Tim Bielawa <timbielawa@gmail.com>
%%% Description : This is a bank teller.
%%% Created : 31 Jan 2010 by Tim Bielawa <timbielawa@gmail.com>

-module(banker).
-export([init/0, deposit/2, withdraw/2, handle/2, balance/1, funds/0]).
-import(my_bank, [rpc/2]).
-import(simple_logger, [announce/2]).

init() -> 0.

% Client Routines
deposit(Account, Amount) -> 
    announce("Bank Teller", "Depositing"),
    rpc(banker, {deposit, Account, Amount}).

withdraw(Account, Amount) -> 
    announce("Bank Teller", "Withdrawing"),
    rpc(banker, {withdraw, Account, Amount}).

balance(Account) -> 
    announce("Bank Teller", "Checking Balance"),
    rpc(banker, {balance, Account}).

funds() ->
    announce("Bank Teller", "Checking Bank Funds"),
    rpc(banker, funds).

%% Message Handlers
handle({deposit, Customer, Amount}, Money) -> 
    announce("Bank Handler", "Deposited"),
    {{ok, Amount}, Money+Amount};

handle({withdraw, Customer, Amount}, Money) -> 
    announce("Bank Handler", "Withdrew"),
    {{ok, Amount}, Money-Amount};

handle({balance, Account}, Money) -> 
    announce("Bank Handler", "Balance Checked"),
    {{ok, 0}, Money};

handle(funds, Money) ->
    announce("Bank Handler", "Bank Funds Checked"),
    {{ok, Money}, Money}.
