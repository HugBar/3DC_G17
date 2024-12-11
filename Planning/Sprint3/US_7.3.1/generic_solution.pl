:- dynamic generations/1.
:- dynamic population/1.
:- dynamic prob_crossover/1.
:- dynamic prob_mutation/1.

% Keep the original room and surgery definitions
:- consult('Final.pl').

% Initialize parameters with check for existing values
initialize:-
    (generations(NG) -> true ; (write('Number of generations: '), read(NG))),
    (retract(generations(_)); true), asserta(generations(NG)),
    
    (population(PS) -> true ; (write('Population size: '), read(PS))),
    (retract(population(_)); true), asserta(population(PS)),
    
    (prob_crossover(PC) -> true ; 
        (write('Probability of crossover (%): '), read(P1), PC is P1/100)),
    (retract(prob_crossover(_)); true), asserta(prob_crossover(PC)),
    
    (prob_mutation(PM) -> true ; 
        (write('Probability of mutation (%): '), read(P2), PM is P2/100)),
    (retract(prob_mutation(_)); true), asserta(prob_mutation(PM)).

% Main generation predicate
generate :-
    initialize,
    generate_initial_population(Pop),
    write('Initial population: '), write(Pop), nl,
    evaluate_population(Pop, PopValue),
    write('Population values: '), write(PopValue), nl,
    order_population(PopValue, PopOrd),
    generations(NG),
    generate_generation(0, NG, PopOrd).

% Generate initial population
generate_initial_population(Pop) :-
    population(PopSize),
    findall(S, surgery(S, _, _, _), Surgeries),
    length(Surgeries, NumSurgeries),
    generate_population(PopSize, Surgeries, NumSurgeries, Pop).

generate_population(0, _, _, []) :- !.
generate_population(PopSize, Surgeries, NumSurgeries, [Ind|Rest]) :-
    PopSize1 is PopSize - 1,
    generate_population(PopSize1, Surgeries, NumSurgeries, Rest),
    generate_individual(Surgeries, NumSurgeries, Ind),
    \+ member(Ind, Rest).

% Generate a single individual (surgery assignment)
generate_individual(Surgeries, NumSurgeries, Assignment) :-
    random_permutation(Surgeries, RandomSurgeries),
    findall(Room, room(Room, _, _), Rooms),
    assign_surgeries(RandomSurgeries, Rooms, TempAssignment),
    validate_solution(TempAssignment),
    Assignment = TempAssignment.

% Assign surgeries to rooms
assign_surgeries(Surgeries, Rooms, Assignment) :-
    assign_surgeries(Surgeries, Rooms, [], Assignment).

assign_surgeries([], _, Acc, Acc).
assign_surgeries([Surgery|Rest], Rooms, Acc, Final) :-
    member(Room, Rooms),
    room(Room, Capacity, Equipment),
    surgery(Surgery, Duration, _, RequiredEquip),
    has_required_equipment(RequiredEquip, Equipment),
    can_fit_surgery(Room, Duration, Acc),
    add_to_assignment(Room, Surgery, Acc, NewAcc),
    assign_surgeries(Rest, Rooms, NewAcc, Final).

% Evaluate population fitness
evaluate_population([], []).
evaluate_population([Ind|Rest], [Ind*Value|Rest1]) :-
    evaluate_assignment(Ind, Value),
    evaluate_population(Rest, Rest1).

% Evaluate single assignment
evaluate_assignment(Assignment, Value) :-
    calculate_capacity_usage(Assignment, CapacityScore),
    calculate_equipment_usage(Assignment, EquipmentScore),
    Value is (0.6 * CapacityScore) + (0.4 * EquipmentScore).

% Calculate capacity usage score
calculate_capacity_usage(Assignment, Score) :-
    findall(RoomScore,
            (member(Room-Surgeries, Assignment),
             room(Room, Capacity, _),
             total_duration(Surgeries, Used),
             RoomScore is Used / Capacity),
            Scores),
    sum_list(Scores, Sum),
    length(Scores, Len),
    Score is Sum / Len * 100.

% Calculate equipment usage score
calculate_equipment_usage(Assignment, Score) :-
    findall(EquipScore,
            (member(Room-Surgeries, Assignment),
             room(Room, _, RoomEquip),
             calculate_room_equipment_score(Surgeries, RoomEquip, EquipScore)),
            Scores),
    sum_list(Scores, TotalScore),
    length(Scores, NumRooms),
    Score is TotalScore / NumRooms.

% Calculate equipment score for a single room
calculate_room_equipment_score(Surgeries, RoomEquip, Score) :-
    findall(MatchScore,
            (member(Surgery, Surgeries),
             surgery(Surgery, _, _, RequiredEquip),
             count_matching_equipment(RequiredEquip, RoomEquip, MatchScore)),
            Scores),
    sum_list(Scores, TotalScore),
    length(Scores, NumSurgeries),
    (NumSurgeries > 0 -> Score is TotalScore / NumSurgeries ; Score = 0).

% Count how many required equipment items are in the room
count_matching_equipment(Required, Available, Score) :-
    findall(1, (member(Equip, Required), member(Equip, Available)), Matches),
    length(Matches, MatchCount),
    length(Required, RequiredCount),
    Score is MatchCount / RequiredCount.

% Crossover operation
crossover([], []).
crossover([Ind*_], [Ind]).
crossover([Ind1*_, Ind2*_|Rest], [NInd1, NInd2|Rest1]) :-
    prob_crossover(Pcruz),
    random(0.0, 1.0, Pc),
    (Pc =< Pcruz ->
        cross_assignments(Ind1, Ind2, TempNInd1, TempNInd2),
        (validate_solution(TempNInd1) -> NInd1 = TempNInd1 ; NInd1 = Ind1),
        (validate_solution(TempNInd2) -> NInd2 = TempNInd2 ; NInd2 = Ind2)
    ;
        NInd1 = Ind1, NInd2 = Ind2),
    crossover(Rest, Rest1).

% Mutation operation
mutation([], []).
mutation([Ind|Rest], [NInd|Rest1]) :-
    prob_mutation(Pmut),
    random(0.0, 1.0, Pm),
    (Pm < Pmut ->
        mutate_assignment(Ind, NInd)
    ;
        NInd = Ind),
    mutation(Rest, Rest1).

% Generate new generation with diversity check
generate_generation(N, G, Pop) :-
    N < G,  % Not final generation
    write('Generation '), write(N), write(':'), nl,
    write(Pop), nl,
    % Perform genetic operations
    crossover(Pop, NPop1),
    mutation(NPop1, NPop),
    evaluate_population(NPop, NPopValue),
    % Sort and ensure diversity
    sort(NPopValue, Sorted),  % Remove duplicates
    ensure_population_diversity(Sorted, DiversePop),
    order_population(DiversePop, NPopOrd),
    % Continue only if not reached max generations
    (N =:= G - 1 ->
        % Last generation, just display final result
        write('Final Generation:'), nl,
        write(NPopOrd), nl
    ;
        % Continue if population changed or force continue
        (Pop \= NPopOrd ->
            N1 is N + 1,
            generate_generation(N1, G, NPopOrd)
        ;
            % Force continue even if population hasn't changed
            N1 is N + 1,
            write('Population stabilized at generation '), write(N), nl,
            generate_generation(N1, G, NPopOrd)
        )
    ).

% Helper predicate to take N elements from list
take(N, List, Taken) :-
    length(Prefix, N),
    append(Prefix, _, List),
    Prefix = Taken.

% Ensure population diversity by modifying similar solutions
ensure_population_diversity(Pop, DiversePop) :-
    population_size(Size),
    length(Pop, CurrentSize),
    (CurrentSize >= Size ->
        % Take the best N solutions
        sort(Pop, SortedPop),  % Sort by fitness
        take(Size, SortedPop, DiversePop)
    ;
        % Generate additional solutions if needed
        generate_diverse_solutions(Pop, Size, DiversePop)
    ).

% Generate additional diverse solutions if needed
generate_diverse_solutions(Pop, TargetSize, DiversePop) :-
    length(Pop, CurrentSize),
    NumNeeded is TargetSize - CurrentSize,
    % Generate new solutions through mutation
    generate_new_solutions(Pop, NumNeeded, NewSolutions),
    % Combine original and new solutions
    append(Pop, NewSolutions, DiversePop).

% Generate N new solutions through mutation
generate_new_solutions(_, 0, []) :- !.
generate_new_solutions(Pop, N, [NewSol*Score|Rest]) :-
    N > 0,
    % Select random solution and mutate it
    random_member(Sol*_, Pop),
    mutate_assignment(Sol, NewSol),
    % Evaluate new solution
    evaluate_assignment(NewSol, Score),
    % Continue generating rest
    N1 is N - 1,
    generate_new_solutions(Pop, N1, Rest).

% Helper predicates
can_fit_surgery(Room, Duration, Assignment) :-
    (member(Room-Surgeries, Assignment) ->
        total_duration(Surgeries, CurrentDuration),
        room(Room, Capacity, _),
        CurrentDuration + Duration =< Capacity
    ;
        true).

add_to_assignment(Room, Surgery, [], [Room-[Surgery]]) :- !.
add_to_assignment(Room, Surgery, [Room-Surgeries|Rest], [Room-[Surgery|Surgeries]|Rest]) :- !.
add_to_assignment(Room, Surgery, [Other|Rest], [Other|NewRest]) :-
    add_to_assignment(Room, Surgery, Rest, NewRest).

% Test predicate
test_genetic_assignment :-
    retractall(generations(_)),
    retractall(population_size(_)),
    retractall(prob_crossover(_)),
    retractall(prob_mutation(_)),
    asserta(generations(50)),
    asserta(population_size(20)),
    asserta(prob_crossover(0.7)),
    asserta(prob_mutation(0.1)),
    generate.

% Order population by fitness (ascending order)
order_population(PopValue, PopValueOrd) :-
    bsort(PopValue, PopValueOrd).

% Bubble sort implementation
bsort([X], [X]) :- !.
bsort([X|Xs], Ys) :-
    bsort(Xs, Zs),
    bchange([X|Zs], Ys).

bchange([X], [X]) :- !.
bchange([X*VX, Y*VY|L1], [Y*VY|L2]) :-
    VX > VY, !,
    bchange([X*VX|L1], L2).
bchange([X|L1], [X|L2]) :-
    bchange(L1, L2).

% Crossover operation for room assignments
cross_assignments(Ind1, Ind2, NInd1, NInd2) :-
    % Get all surgeries from both assignments
    findall(S, (member(_-Surgeries, Ind1), member(S, Surgeries)), Surgeries1),
    findall(S, (member(_-Surgeries, Ind2), member(S, Surgeries)), Surgeries2),
    % Generate crossover points
    length(Surgeries1, L),
    P1 is L // 3,
    P2 is 2 * L // 3,
    % Perform crossover
    perform_crossover(Surgeries1, Surgeries2, P1, P2, NewSurgeries1, NewSurgeries2),
    % Create new assignments
    findall(Room, room(Room, _, _), Rooms),
    assign_surgeries(NewSurgeries1, Rooms, NInd1),
    assign_surgeries(NewSurgeries2, Rooms, NInd2).

% Perform crossover between two surgery lists
perform_crossover(Surgeries1, Surgeries2, P1, P2, NewSurgeries1, NewSurgeries2) :-
    % Split lists at crossover points
    split_at(Surgeries1, P1, Start1, Mid1, End1),
    split_at(Surgeries2, P1, Start2, Mid2, End2),
    % Exchange middle segments
    append(Start1, Mid2, Temp1),
    append(Temp1, End1, NewSurgeries1),
    append(Start2, Mid1, Temp2),
    append(Temp2, End2, NewSurgeries2).

% Helper predicate to split a list at given position
split_at(List, N, Start, Mid, End) :-
    length(Start, N),
    append(Start, Rest, List),
    length(Mid, N),
    append(Mid, End, Rest).

% Mutation operation
mutate_assignment(Ind, NInd) :-
    % Get all surgeries from assignment
    findall(S, (member(_-Surgeries, Ind), member(S, Surgeries)), Surgeries),
    % Randomly swap two surgeries
    length(Surgeries, L),
    random(0, L, P1),
    random(0, L, P2),
    P1 \= P2,
    nth0(P1, Surgeries, S1),
    nth0(P2, Surgeries, S2),
    swap_elements(Surgeries, P1, P2, NewSurgeries),
    % Create new assignment
    findall(Room, room(Room, _, _), Rooms),
    assign_surgeries(NewSurgeries, Rooms, NInd).

% Helper predicate to swap elements in a list
swap_elements(List, P1, P2, NewList) :-
    nth0(P1, List, E1),
    nth0(P2, List, E2),
    replace_nth0(P1, List, E2, TempList),
    replace_nth0(P2, TempList, E1, NewList).

% Helper predicate to replace element at index
replace_nth0(N, List, E, NewList) :-
    length(Prefix, N),
    append(Prefix, [_|Suffix], List),
    append(Prefix, [E|Suffix], NewList).

% Validate individual solution
validate_solution(Assignment) :-
    % Get all surgeries in the assignment
    findall(Surgery, 
            (member(Room-Surgeries, Assignment),
             member(Surgery, Surgeries)),
        AllScheduledSurgeries),
    % Check for duplicates
    sort(AllScheduledSurgeries, NoDuplicates),
    length(AllScheduledSurgeries, L1),
    length(NoDuplicates, L2),
    L1 = L2,
    % Check if all surgeries are scheduled
    findall(S, surgery(S,_,_,_), AllPossibleSurgeries),
    sort(AllPossibleSurgeries, SortedPossible),
    NoDuplicates = SortedPossible.





