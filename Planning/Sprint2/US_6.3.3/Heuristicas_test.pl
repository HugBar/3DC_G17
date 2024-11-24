:- consult('6.3.1.pl').  

% Unified test for both heuristics
test_heuristics(OpCodeList, Room, Day, Result) :-
    % Heuristic 1: Earliest Available (availability_all_surgeries/3)
    initialize_availabilities(Day), % Reset states
    availability_all_surgeries(OpCodeList, Room, Day),
    agenda_operation_room1(Room, Day, Schedule1),
    write('Heuristic 1 (Earliest Available) Schedule: '), nl,
    write(Schedule1), nl,
    evaluate_schedule(Schedule1, Room, Day, EndTime1),

    % Reset state for next heuristic
    initialize_availabilities(Day),

    % Heuristic 2: Most Occupied Slot (obtain_better_sol/5)
    obtain_better_sol(Room, Day, Schedule2, _, EndTime2),
    write('Heuristic 2 (Most Occupied) Schedule: '), nl,
    write(Schedule2), nl,


    % Output comparison
    write('Comparison of Heuristics:'), nl,
    format('Heuristic 1: End Time = ~w, Schedule = ~w~n', [EndTime1, Schedule1]),
    format('Heuristic 2: End Time = ~w, Schedule = ~w~n', [EndTime2, Schedule2]).

% Evaluate a schedule's end time
evaluate_schedule(Schedule, _, _, EndTime) :-
    findall(End, member((_, End, _), Schedule), EndTimes),
    max_list(EndTimes, EndTime).
