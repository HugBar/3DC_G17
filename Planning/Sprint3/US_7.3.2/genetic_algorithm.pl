:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.

% task(Id,PreparationTime,SurgeryTime,CleaningTime,Priority).
task(s1, 30, 120, 30, 3).
task(s2, 45, 180, 45, 5).
task(s3, 20, 60, 20, 1).
task(s4, 35, 150, 35, 4).
task(s5, 25, 90, 25, 2).

% tasks(NTasks).
tasks(5).

% parameters initialization
initialize:-
     write('Select stopping condition (1-Generations, 2-Time): '), read(Option),
    (Option = 1 -> 
        write('Number of generations: '), read(NG),
        (retract(generations(_));true), asserta(generations(NG)),
        (retract(stop_condition(_));true), asserta(stop_condition(generations))
    ;   
        write('Time limit (seconds): '), read(TimeLimit),
        (retract(time_limit(_));true), asserta(time_limit(TimeLimit)),
        (retract(stop_condition(_));true), asserta(stop_condition(time))
    ),
	write('Population size: '),read(PS),
	(retract(population(_));true), asserta(population(PS)),
	write('Probability of crossover (%):'), read(P1),
	PC is P1/100, 
	(retract(prob_crossover(_));true), 	asserta(prob_crossover(PC)),
	write('Probability of mutation (%):'), read(P2),
	PM is P2/100, 
	(retract(prob_mutation(_));true), asserta(prob_mutation(PM)).

generate:-
    initialize,
    generate_population(Pop),
    evaluate_population(Pop,PopValue),
    order_population(PopValue,PopOrd),
    get_time(StartTime),
    stop_condition(Condition),
    (Condition = generations -> 
        generations(G),
        generate_generation(0, G, PopOrd)
    ;   
        time_limit(TimeLimit),
        generate_generation_time(0, StartTime, TimeLimit, PopOrd)
    ).

generate_population(Pop):-
    population(PopSize),
    tasks(NumT),
    findall(Task,task(Task,_,_,_,_),TasksList),
    generate_population(PopSize,TasksList,NumT,Pop).

generate_population(0,_,_,[]):-!.
generate_population(PopSize,TasksList,NumT,[Ind|Rest]):-
    PopSize1 is PopSize-1,
    generate_population(PopSize1,TasksList,NumT,Rest),
    generate_individual(TasksList,NumT,Ind),
    not(member(Ind,Rest)).
generate_population(PopSize,TasksList,NumT,L):-
    generate_population(PopSize,TasksList,NumT,L).

generate_individual([G],1,[G]):-!.

generate_individual(TasksList,NumT,[G|Rest]):-
    NumTemp is NumT + 1, % to use with random
    random(1,NumTemp,N),
    remove(N,TasksList,G,NewList),
    NumT1 is NumT-1,
    generate_individual(NewList,NumT1,Rest).

remove(1,[G|Rest],G,Rest).
remove(N,[G1|Rest],G,[G1|Rest1]):- N1 is N-1,
            remove(N1,Rest,G,Rest1).


evaluate_population([],[]).
evaluate_population([Ind|Rest],[Ind*V|Rest1]):-
    evaluate(Ind,V),
    evaluate_population(Rest,Rest1).

evaluate(Seq,V):- evaluate(Seq,0,V).

evaluate([ ],_,0).
evaluate([S|Rest], StartTime, V):-
    task(S, Prep, Surg, Clean, _),
    ProcedureTime is Prep + Surg + Clean,
    FinishTime is StartTime + ProcedureTime,
    evaluate(Rest, FinishTime, VRest),
    V is max(FinishTime, VRest). % Only care about last operation finish time

order_population(PopValue,PopValueOrd):-
    bsort(PopValue,PopValueOrd).

bsort([X],[X]):-!.
bsort([X|Xs],Ys):-
    bsort(Xs,Zs),
    bchange([X|Zs],Ys).


bchange([X],[X]):-!.

bchange([X*VX,Y*VY|L1],[Y*VY|L2]):-
    VX>VY,!,
    bchange([X*VX|L1],L2).

bchange([X|L1],[X|L2]):-bchange(L1,L2).

% Time-based generation
generate_generation_time(_,StartTime,TimeLimit,Pop):-
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,
    ElapsedTime >= TimeLimit,!,
    write('Time limit reached. Best solution:'),nl,
    Pop = [Best*Value|_],
    write(Best), write(' with value '), write(Value),nl.

generate_generation_time(N,StartTime,TimeLimit,Pop):-
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,
    ElapsedTime < TimeLimit,
    write('Generation '), write(N), write(':'), nl, write(Pop), nl,
    % Get best individual from current population
    crossover(Pop,NPop1),
    mutation(NPop1,NPop),
    evaluate_population(NPop,NPopValue),
    append(Pop,NPopValue,AllPop),
    
    remove_duplicates(AllPop,AllPop1),

    order_population(AllPop1,TempPopOrd),

    select_top_p(TempPopOrd, TopP, Remaining),

    add_random_weights(Remaining, Randomized),
    sort_by_randomized(Randomized, Sorted),

    length(TopP, P),
    population(PopSize),
    
    NP is PopSize - P,

    take_n(NP, Sorted, NewPop),
    append(TopP, NewPop, NPopOrd),
    
    order_population(NPopOrd, NPopOrdSorted),

    N1 is N+1,
    generate_generation_time(N1,StartTime,TimeLimit,NPopOrdSorted).
    
generate_generation(G,G,Pop):-!,
	write('Generation '), write(G), write(':'), nl, write(Pop), nl.
generate_generation(N,G,Pop):-
    write('Generation '), write(N), write(':'), nl, write(Pop), nl,
    % Get best individual from current population
    crossover(Pop,NPop1),
    mutation(NPop1,NPop),
    evaluate_population(NPop,NPopValue),
    append(Pop,NPopValue,AllPop),
    
    remove_duplicates(AllPop,AllPop1),

    order_population(AllPop1,TempPopOrd),

    select_top_p(TempPopOrd, TopP, Remaining),

    add_random_weights(Remaining, Randomized),
    sort_by_randomized(Randomized, Sorted),

    length(TopP, P),
    population(PopSize),
    
    NP is PopSize - P,

    take_n(NP, Sorted, NewPop),
    append(TopP, NewPop, NPopOrd),
    
    order_population(NPopOrd, NPopOrdSorted),

    N1 is N+1,
    generate_generation(N1,G,NPopOrdSorted).

select_top_p(SortedList, TopP, Remaining):-
    SortedList = [BestCurrent*_|_],
    population(PopSize),
    TopCount is round(PopSize * 0.2),
    take_p(TopCount, SortedList, TempTopP, TempRemaining),
    (member(BestCurrent*_, TempTopP) ->
        TopP = TempTopP,
        Remaining = TempRemaining
    ;
        TopP = [BestCurrent|TempTopP],
        delete(TempRemaining, BestCurrent, Remaining)
    ).

take_p(0, List, [], List). % Base case: if TopCount is 0, take nothing, remainder is the whole list.
take_p(N, [H|T], [H|TopP], Remaining) :-
    N > 0,
    N1 is N - 1,
    take_p(N1, T, TopP, Remaining).

take_n(0, _, []). % Base case: if N is 0, take nothing.
take_n(_, [], []). % Base case: if list is empty, take nothing.
take_n(N, [H|T], [H|Rest]) :- % Take N elements from the list.
    N > 0,
    N1 is N - 1,
    take_n(N1, T, Rest).

remove_duplicates([], []).
remove_duplicates([H|T], [H|Result]) :-
    \+ member(H, T), % Check if H is not already in the tail
    remove_duplicates(T, Result).
remove_duplicates([H|T], Result) :-
    member(H, T), % If H is in the tail, skip it
    remove_duplicates(T, Result).


randomize_evaluation(List, Randomized):- 
    maplist(randomize_individual, List, Randomized).
randomize_individual(Ind*Val, Ind*RandVal):- 
    random(0.0, 1.0, Rand), 
    RandVal is Val * Rand.

add_random_weights([], []).
add_random_weights([Ind*Val|Rest], [Ind*Val*RandVal|WeightedRest]) :-
    random(0.0, 1.0, R),
    RandVal is Val * R,
    add_random_weights(Rest, WeightedRest).

sort_by_randomized(List, Sorted) :-
    sort(2, @=<, List, SortedByRandom),
    strip_random_values(SortedByRandom, Sorted).

strip_random_values([], []).
strip_random_values([Ind*Val*_|Rest], [Ind*Val|StrippedRest]) :-
    strip_random_values(Rest, StrippedRest).

generate_crossover_points(P1,P2):- generate_crossover_points1(P1,P2).

generate_crossover_points1(P1,P2):-
	tasks(N),
	NTemp is N+1,
	random(1,NTemp,P11),
	random(1,NTemp,P21),
	P11\==P21,!,
	((P11<P21,!,P1=P11,P2=P21);P1=P21,P2=P11).
generate_crossover_points1(P1,P2):-
	generate_crossover_points1(P1,P2).




crossover([], []).
crossover([Ind*_], [Ind]). % If only one individual, it remains unchanged.
crossover(Population, Result) :-
    random_permutation(Population, ShuffledPopulation), % Shuffle the population to randomize pairings.
    crossover_pairs(ShuffledPopulation, Result).

crossover_pairs([], []).
crossover_pairs([Ind*_], [Ind]). % Handle odd number of individuals.
crossover_pairs([Ind1*_,Ind2*_|Rest], [NInd1, NInd2 | Rest1]) :-
    generate_crossover_points(P1, P2),
    prob_crossover(Pcruz),
    random(0.0, 1.0, Pc),
    (   (Pc =< Pcruz,!,
        cross(Ind1, Ind2, P1, P2, NInd1),
        cross(Ind2, Ind1, P1, P2, NInd2))
    ;   (NInd1=Ind1,NInd2=Ind2)
    ),
    crossover_pairs(Rest, Rest1).

fillh([ ],[ ]).

fillh([_|R1],[h|R2]):-
	fillh(R1,R2).

sublist(L1,I1,I2,L):-I1 < I2,!,
    sublist1(L1,I1,I2,L).

sublist(L1,I1,I2,L):-sublist1(L1,I2,I1,L).

sublist1([X|R1],1,1,[X|H]):-!, fillh(R1,H).

sublist1([X|R1],1,N2,[X|R2]):-!,N3 is N2 - 1,
	sublist1(R1,1,N3,R2).

sublist1([_|R1],N1,N2,[h|R2]):-N3 is N1 - 1,
		N4 is N2 - 1,
		sublist1(R1,N3,N4,R2).

rotate_right(L,K,L1):- tasks(N),
	T is N - K,
	rr(T,L,L1).

rr(0,L,L):-!.

rr(N,[X|R],R2):- N1 is N - 1,
	append(R,[X],R1),
	rr(N1,R1,R2).

remove([],_,[]):-!.

remove([X|R1],L,[X|R2]):- not(member(X,L)),!,
        remove(R1,L,R2).

remove([_|R1],L,R2):-
    remove(R1,L,R2).

insert([],L,_,L):-!.
insert([X|R],L,N,L2):-
    tasks(T),
    ((N>T,!,N1 is N mod T);N1 = N),
    insert1(X,N1,L,L1),
    N2 is N + 1,
    insert(R,L1,N2,L2).


insert1(X,1,L,[X|L]):-!.
insert1(X,N,[Y|L],[Y|L1]):-
    N1 is N-1,
    insert1(X,N1,L,L1).

cross(Ind1,Ind2,P1,P2,NInd11):-
    sublist(Ind1,P1,P2,Sub1),
    tasks(NumT),
    R is NumT-P2,
    rotate_right(Ind2,R,Ind21),
    remove(Ind21,Sub1,Sub2),
    P3 is P2 + 1,
    insert(Sub2,Sub1,P3,NInd1),
    removeh(NInd1,NInd11).


removeh([],[]).

removeh([h|R1],R2):-!,
    removeh(R1,R2).

removeh([X|R1],[X|R2]):-
    removeh(R1,R2).

mutation([],[]).
mutation([Ind|Rest],[NInd|Rest1]):-
	prob_mutation(Pmut),
	random(0.0,1.0,Pm),
	((Pm < Pmut,!,mutacao1(Ind,NInd));NInd = Ind),
	mutation(Rest,Rest1).

mutacao1(Ind,NInd):-
	generate_crossover_points(P1,P2),
	mutacao22(Ind,P1,P2,NInd).

mutacao22([G1|Ind],1,P2,[G2|NInd]):-
	!, P21 is P2-1,
	mutacao23(G1,P21,Ind,G2,NInd).
mutacao22([G|Ind],P1,P2,[G|NInd]):-
	P11 is P1-1, P21 is P2-1,
	mutacao22(Ind,P11,P21,NInd).

mutacao23(G1,1,[G2|Ind],G2,[G1|Ind]):-!.
mutacao23(G1,P,[G|Ind],G2,[G|NInd]):-
	P1 is P-1,
	mutacao23(G1,P1,Ind,G2,NInd).
