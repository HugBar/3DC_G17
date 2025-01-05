:-dynamic generations/1.
:-dynamic population/1.
:-dynamic prob_crossover/1.
:-dynamic prob_mutation/1.
:-dynamic genetic_solution/2.  % genetic_solution(Solution, Value)
:-dynamic best_solution/2. 
:-dynamic tasks/1.
:-dynamic time_limit/1.
:-dynamic stop_condition/1.
:- consult('schedule.pl').

% room(id, surgeries).
% Define rooms and their surgeries
room(or1, [so100001, so100002, so100004,so100003,so100005]).
% room(or2, [so100006, so100007, so100008,so100009,so100010]).

% task(Id,PreparationTime,SurgeryTime,CleaningTime,Priority).
task(so100001, 45, 60, 45, 3).
task(so100002, 45, 60, 30, 5).
task(so100003, 45,50,30, 1).
task(so100004, 45, 60, 45, 4).
task(so100005, 45,50,30, 2).
task(so100006, 45, 60, 45, 3).
task(so100007, 45, 60, 30, 5).
task(so100008, 45,50,30, 1).
task(so100009, 45, 60, 45, 4).
task(so100010, 45,50,30, 2).

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
    retractall(genetic_solution(_,_)),  % Clear previous solutions
    retractall(best_solution(_,_)),     % Clear previous best
    initialize,
    generate_population(Pop),
    evaluate_population(Pop,PopValue),
    order_population(PopValue,PopOrd),
    store_initial_solutions(PopOrd),    % Store initial solutions
    get_time(StartTime),
    stop_condition(Condition),
    (Condition = generations -> 
        generations(G),
        generate_generation(0, G, PopOrd)
    ;   
        time_limit(TimeLimit),
        generate_generation_time(0, StartTime, TimeLimit, PopOrd)
    ).

% Helper to clean up state between rooms
cleanup_room_state :-
    retractall(scheduled_surgery(_)),
    retractall(failed_surgery(_)),
    % Keep agenda_staff1 to maintain staff availability between rooms
    retractall(agenda_operation_room1(_, _, _)),
    retractall(better_sol(_, _, _, _, _)).

% 3. Add solution storage predicates
store_initial_solutions([]).
store_initial_solutions([Solution*Value|Rest]):-
    assertz(genetic_solution(Solution, Value)),
    (best_solution(_,BestValue) ->
        (Value < BestValue ->
            retract(best_solution(_,_)),
            assertz(best_solution(Solution,Value))
        ; true)
    ;
        assertz(best_solution(Solution,Value))
    ),
    store_initial_solutions(Rest).

% Modified population generation for specific room surgeries
generate_population_for_room(Pop, Surgeries) :-
    population(PopSize),
    write('Generating population for room...'), nl,
    length(Surgeries, NumT),
    write('Room surgeries: '), write(Surgeries), nl,
    write('Number of tasks: '), write(NumT), nl,
    generate_population(PopSize, Surgeries, NumT, Pop).

generate_population(Pop):-
    population(PopSize),
    write('Generating population...'), nl,
    tasks(NumT),
    write('Number of tasks: '), write(NumT), nl,
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

% Modified evaluation for specific room
evaluate_population_for_room([], _, []).
evaluate_population_for_room([Ind|Rest], Room, [Ind*V|Rest1]) :-
    evaluate_for_room(Ind, Room, V),
    evaluate_population_for_room(Rest, Room, Rest1).

evaluate_for_room(Seq, Room, V) :-   
    % Try to schedule the sequence for this specific room
    schedule_surgeries_list(Seq, Room, 20241028),
    calculate_priority_score(Seq, PriorityScore),
    calculate_schedule_score(ScheduleScore),
    calculate_makespan_score(MakespanScore),
    V is PriorityScore + ScheduleScore + MakespanScore.


evaluate_population([],[]).
evaluate_population([Ind|Rest],[Ind*V|Rest1]):-
    evaluate(Ind,V),
    evaluate_population(Rest,Rest1).

evaluate(Seq, V):-   
    % Try to schedule the sequence
    schedule_surgeries_list(Seq, Room, 20241028),
    
    % Calculate components
    calculate_priority_score(Seq, PriorityScore),
    calculate_schedule_score(ScheduleScore),
    calculate_makespan_score(MakespanScore),
    
    % Combine scores (lower is better)
    V is PriorityScore + ScheduleScore + MakespanScore.

% Priority score based on scheduled/failed surgeries
calculate_priority_score([], 0).
calculate_priority_score([Surgery|Rest], Score) :-
    calculate_priority_score(Rest, RestScore),
    task(Surgery, _, _, _, Priority),
    (scheduled_surgery(Surgery) -> 
        % Lower score (better) for scheduled high priority
        Score is RestScore + 0
    ;   
        % Higher score (worse) for failed high priority
        Score is RestScore + Priority * 100
    ).

% Schedule success/failure score
calculate_schedule_score(Score) :-
    findall(1, scheduled_surgery(_), Successes),
    findall(1, failed_surgery(_), Failures),
    length(Successes, NumSuccess),
    length(Failures, NumFailures),
    Score is NumFailures * 200 - NumSuccess * 10.

% Makespan score
calculate_makespan_score(Score) :-
    findall(EndTime, (
        agenda_operation_room1(_, _, Agenda),
        member((_, EndTime, _), Agenda)
    ), EndTimes),
    (EndTimes = [] -> 
        Score = 1000  % Penalty if nothing scheduled
    ;   
        max_list(EndTimes, MaxEnd),
        Score is MaxEnd
    ).

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



% 5. Add helper predicate to store new solutions
store_new_solutions([]).
store_new_solutions([Solution*Value|Rest]):-
    assertz(genetic_solution(Solution, Value)),
    (best_solution(_,BestValue) ->
        (Value < BestValue ->
            retract(best_solution(_,_)),
            assertz(best_solution(Solution,Value))
        ; true)
    ;
        assertz(best_solution(Solution,Value))
    ),
    store_new_solutions(Rest).

get_all_solutions(Solutions) :-
    findall(Solution-Value, genetic_solution(Solution, Value), Solutions).

get_best_solution(Solution, Value) :-
    best_solution(Solution, Value).

get_top_n_solutions(N, Solutions) :-
    findall(Solution-Value, genetic_solution(Solution, Value), AllSolutions),
    sort(2, @=<, AllSolutions, SortedSolutions),
    take_n(N, SortedSolutions, Solutions).

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

format_time(Minutes, TimeStr) :-
    Hours is Minutes div 60,
    Mins is Minutes mod 60,
    format(atom(TimeStr), '~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Mins]).

% Display complete schedule
display_final_schedule(BestSolution, Room) :-
    writeln('\n====== FINAL SCHEDULE ======\n'),
    
    % Schedule best solution
    schedule_surgeries_list(BestSolution, Room, 20241028),
    
    % Show surgeries status
    writeln('=== SURGERIES STATUS ==='),
    writeln('\nSuccessfully Scheduled:'),
    forall(scheduled_surgery(S), 
           (task(S, P, Su, C, Pr),
            Total is P + Su + C,
            format('  ~w (Priority: ~w, Duration: ~w min)\n', [S, Pr, Total]))),
    
    writeln('\nFailed to Schedule:'),
    forall(failed_surgery(F), 
           (task(F, _, _, _, Pr),
            format('  ~w (Priority: ~w)\n', [F, Pr]))),
    
    % Show room schedule
    writeln('\n=== OPERATION ROOM SCHEDULE ==='),
    agenda_operation_room1(or1, 20241028, RoomAgenda),
    format_room_schedule(RoomAgenda),
    
    % Show staff schedules
    writeln('\n=== STAFF SCHEDULES ==='),
    forall(agenda_staff1(Staff, 20241028, Agenda),
           (format('\n~w:\n', [Staff]),
            format_staff_schedule(Agenda))).

% Format room schedule
format_room_schedule([]).
format_room_schedule([(Start, End, Op)|Rest]) :-
    format_time(Start, StartTime),
    format_time(End, EndTime),
    format('  ~w - ~w : ~w\n', [StartTime, EndTime, Op]),
    format_room_schedule(Rest).

% Format staff schedule
format_staff_schedule([]).
format_staff_schedule([(Start, End, Op)|Rest]) :-
    format_time(Start, StartTime),
    format_time(End, EndTime),
    format('  ~w - ~w : ~w\n', [StartTime, EndTime, Op]),
    format_staff_schedule(Rest).

% Main predicate to run genetic algorithm for all rooms
schedule_all_rooms :-
    retractall(time_limit(_)),
    retractall(stop_condition(_)),
    % Get all rooms and their surgeries
    initialize,
    findall(Room-Surgeries, room(Room, Surgeries), RoomSurgeries),
    write('Rooms and surgeries: '), write(RoomSurgeries), nl,
    % Process each room sequentially
    process_rooms(RoomSurgeries).

% Base case - no more rooms to process
process_rooms([]):-!.
% Recursive case - process one room at a time
process_rooms([Room-Surgeries|Rest]) :-
    format('~nProcessing room ~w~n', [Room]),
    % Initialize genetic algorithm parameters for this room
    retractall(tasks(_)),
    length(Surgeries, NumTasks),
    assertz(tasks(NumTasks)),
    % Run genetic algorithm for current room
    generate_for_room(Room, Surgeries),
    % Continue with next room
    !,
    process_rooms(Rest).

% Run genetic algorithm for a specific room
generate_for_room(Room, Surgeries) :-
    cleanup_room_state,
    retractall(genetic_solution(_, _)),
    retractall(best_solution(_, _)),
    % Generate initial population based on rooms surgeries
    generate_population_for_room(Pop, Surgeries),
    write('Initial population: '), write(Pop), nl,
    evaluate_population_for_room(Pop, Room, PopValue),
    write('Initial population with values: '), write(PopValue), nl,
    order_population(PopValue, PopOrd),
    store_initial_solutions(PopOrd),
    get_time(StartTime),
    stop_condition(Condition),
    (Condition = generations -> 
        generations(G),
        generate_generation_for_room(0, G, PopOrd, Room)
    ;   
        time_limit(TimeLimit),
        generate_generation_time_for_room(0, StartTime, TimeLimit, PopOrd, Room)
    ).

% filepath: /c:/Users/User/Universidade/3º Ano Licenciatura/Projeto/3dc_17/3DC_G17/Planning/Sprint3/US_7.3.2/genetic_algorithm.pl

% Step 3: Add generation predicates
generate_generation_for_room(G, G, Pop, Room):-!,
    write('Room '), write(Room), write(' - Final Generation '), write(G), write(':'), nl,
    Pop = [Best*Value|_],
    format('Best solution for ~w: ~w with value ~w~n', [Room, Best, Value]),
    schedule_surgeries_list(Best, Room, 20241028).

generate_generation_for_room(N, G, Pop, Room):-
    write('Room '), write(Room), write(' - Generation '), write(N), write(':'), write(Pop), nl,
    crossover(Pop, NPop1),
    mutation(NPop1, NPop),
    evaluate_population_for_room(NPop, Room, NPopValue),
    store_new_solutions(NPopValue),
    append(Pop, NPopValue, AllPop),
    remove_duplicates(AllPop, AllPop1),
    order_population(AllPop1, TempPopOrd),
    select_top_p(TempPopOrd, TopP, Remaining),
    add_random_weights(Remaining, Randomized),
    sort_by_randomized(Randomized, Sorted),
    population(PopSize),
    length(TopP, P),
    NP is PopSize - P,
    take_n(NP, Sorted, NewPop),
    append(TopP, NewPop, NPopOrd),
    order_population(NPopOrd, NPopOrdSorted),
    N1 is N+1,
    generate_generation_for_room(N1, G, NPopOrdSorted, Room).

% Step 4: Add time-based generation
generate_generation_time_for_room(_, StartTime, TimeLimit, Pop, Room):-
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,
    ElapsedTime >= TimeLimit,!,
    Pop = [Best*Value|_],
    format('Time limit reached for ~w. Best: ~w Value: ~w~n', [Room, Best, Value]),
    schedule_surgeries_list(Best, Room, 20241028),
    display_final_schedule(Best, Room).

generate_generation_time_for_room(N, StartTime, TimeLimit, Pop, Room):-
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,
    ElapsedTime < TimeLimit,
    write('Room '), write(Room), write(' - Generation '), write(N), write(':'), write(Pop), nl,nl,
    crossover(Pop, NPop1),
    mutation(NPop1, NPop),
    evaluate_population_for_room(NPop, Room, NPopValue),
    store_new_solutions(NPopValue),
    append(Pop, NPopValue, AllPop),
    remove_duplicates(AllPop, AllPop1),
    order_population(AllPop1, TempPopOrd),
    select_top_p(TempPopOrd, TopP, Remaining),
    add_random_weights(Remaining, Randomized),
    sort_by_randomized(Randomized, Sorted),
    population(PopSize),
    length(TopP, P),
    NP is PopSize - P,
    take_n(NP, Sorted, NewPop),
    append(TopP, NewPop, NPopOrd),
    order_population(NPopOrd, NPopOrdSorted),
    N1 is N+1,
    generate_generation_time_for_room(N1, StartTime, TimeLimit, NPopOrdSorted, Room).