:- consult('test3.pl').  % Load existing code

% Test both heuristics and compare results
test_heuristics(OpCodeList, Room, Day, BestSchedule) :-
    % Heuristic 1: Earliest available slot
    schedule_all_surgeries(Room, Day),
    agenda_operation_room1(Room, Day, Schedule1),  
    evaluate_schedule(Schedule1, Room, Day, EndTime1),

    % Reset state for next heuristic
    initialize_availabilities(Day),

    % Heuristic 2: Most occupied slot
    obtain_better_sol(Room, Day, Schedule2, _, _),
    evaluate_schedule(Schedule2, Room, Day, EndTime2),

    % Compare and select the better heuristic
    (   EndTime1 =< EndTime2
    ->  BestSchedule = Schedule1,
        SelectedHeuristic = 'Heuristic 1 (Earliest Available)'
    ;   BestSchedule = Schedule2,
        SelectedHeuristic = 'Heuristic 2 (Most Occupied)'
    ),

    % Clean Output
    write('Comparison of Heuristics:'), nl,
    format('Heuristic 1: End Time = ~w, Schedule = ~w~n', [EndTime1, Schedule1]),
    format('Heuristic 2: End Time = ~w, Schedule = ~w~n', [EndTime2, Schedule2]),
    format('Selected Schedule: ~w~n', [SelectedHeuristic]).

% Evaluate a schedule to determine its maximum end time
evaluate_schedule(Schedule, _, _, EndTime) :-
    findall(End, member((_, End, _), Schedule), EndTimes),
    max_list(EndTimes, EndTime).

% Test case with more surgeries and updated setup
test_small_input :-
    OpCodeList = [so100001, so100002, so100003, so100004, so100005,so100006],  
    Room = or1,
    Day = 20241028, 
    test_heuristics(OpCodeList, Room, Day, BestSchedule),
    format('Best Schedule: ~w~n', [BestSchedule]).
