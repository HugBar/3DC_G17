% Define operation rooms (RoomId, Capacity, SpecialEquipment).
room(r1, 8, [xray, ventilator]).
room(r2, 6, [ventilator]).
room(r3, 10, [xray, ventilator, cardiac_monitor]).

% Define surgeries (SurgeryId, Duration, Type, RequiredEquipment).
surgery(s1, 3, general, [ventilator]).
surgery(s2, 5, orthopedic, [xray]).
surgery(s3, 2, pediatric, [ventilator]).
surgery(s4, 4, general, [ventilator, cardiac_monitor]).
surgery(s5, 6, orthopedic, [xray, ventilator]).

% Basic assignment predicate
assign_surgeries_to_rooms(Surgeries, Rooms, Assignment) :-
    assign(Surgeries, Rooms, [], Assignment).

% Base case: no surgeries left to assign
assign([], _, Assignment, Assignment).

% Recursive case: assign surgery to best fitting room
assign([CurrentSurgery|RestSurgeries], Rooms, PartialAssignment, FinalAssignment) :-
    surgery(CurrentSurgery, Duration, _, RequiredEquip),
    find_best_room(CurrentSurgery, Duration, RequiredEquip, PartialAssignment, Rooms, BestRoom),
    format('Assigning surgery ~w to room ~w (capacity: ~w)~n', 
           [CurrentSurgery, BestRoom, Duration]),
    update_assignment(BestRoom, CurrentSurgery, PartialAssignment, NewAssignment),
    assign(RestSurgeries, Rooms, NewAssignment, FinalAssignment).

% Find best room considering current assignments and available capacity
find_best_room(Surgery, Duration, RequiredEquip, CurrentAssignment, Rooms, BestRoom) :-
    findall(Room-Available, 
            (member(Room-_, Rooms),
             room(Room, Capacity, RoomEquip),
             has_required_equipment(RequiredEquip, RoomEquip),
             get_room_available_capacity(Room, Capacity, CurrentAssignment, Available),
             Available >= Duration),
            ValidRooms),
    ValidRooms \= [], % Ensure there is at least one valid room
    sort(2, @>=, ValidRooms, [BestRoom-_|_]). % Sort by available capacity (descending)

% Get available capacity for a room considering current assignments
get_room_available_capacity(Room, MaxCapacity, Assignment, Available) :-
    (member(Room-Surgeries, Assignment) ->
        total_duration(Surgeries, UsedCapacity),
        Available is MaxCapacity - UsedCapacity
    ;
        Available is MaxCapacity).

% Check if room has required equipment
has_required_equipment([], _).
has_required_equipment([Equip|Rest], RoomEquip) :-
    member(Equip, RoomEquip),
    has_required_equipment(Rest, RoomEquip).

% Update assignment with new surgery
update_assignment(Room, Surgery, [], [Room-[Surgery]]) :- !.
update_assignment(Room, Surgery, [Room-Surgeries|Rest], [Room-[Surgery|Surgeries]|Rest]) :-
    surgery(Surgery, Duration, _, _),
    total_duration([Surgery|Surgeries], TotalDuration),
    room(Room, Capacity, _),
    TotalDuration =< Capacity,
    !.
update_assignment(Room, Surgery, [OtherRoom-OtherSurgeries|Rest], [OtherRoom-OtherSurgeries|NewRest]) :-
    OtherRoom \= Room,
    update_assignment(Room, Surgery, Rest, NewRest).

% Calculate total duration of surgeries
total_duration([], 0).
total_duration([S|Rest], Total) :-
    surgery(S, Duration, _, _),
    total_duration(Rest, RestTotal),
    Total is Duration + RestTotal.

% Test predicate
test_basic_assignment :-
    findall(S, surgery(S, _, _, _), Surgeries),
    sort_surgeries_by_duration(Surgeries, SortedSurgeries),
    findall(Room-Cap, room(Room, Cap, _), Rooms),
    assign_surgeries_to_rooms(SortedSurgeries, Rooms, Assignment),
    format('~nBasic Assignment Result:~n'),
    print_assignment(Assignment),
    verify_assignment(Assignment).

% Helper predicate to print assignment
print_assignment([]).
print_assignment([Room-Surgeries|Rest]) :-
    room(Room, Capacity, _),
    total_duration(Surgeries, UsedCapacity),
    format('Room ~w (Capacity: ~w):~n', [Room, Capacity]),
    format('  Surgeries: ~w~n', [Surgeries]),
    format('  Used Capacity: ~w/~w~n', [UsedCapacity, Capacity]),
    print_assignment(Rest).

% Verify assignment is valid
verify_assignment([]).
verify_assignment([Room-Surgeries|Rest]) :-
    room(Room, Capacity, _),
    total_duration(Surgeries, TotalDuration),
    TotalDuration =< Capacity,
    verify_assignment(Rest).

% Sort surgeries by duration (descending) to handle longer surgeries first
sort_surgeries_by_duration(Surgeries, SortedSurgeries) :-
    findall(Duration-Surgery, 
            (member(Surgery, Surgeries), 
             surgery(Surgery, Duration, _, _)), 
            Pairs),
    sort(1, @>=, Pairs, SortedPairs),
    findall(Surgery, member(_-Surgery, SortedPairs), SortedSurgeries).










%Room r3 (Capacity: 10)
%s4 (4 hours) + s5 (6 hours) = 10/10 hours
%100% capacity utilization
%Has all required equipment (xray, ventilator, cardiac_monitor)
%Room r1 (Capacity: 8)
%s3 (2 hours) + s2 (5 hours) = 7/8 hours
%87.5% capacity utilization
%Has required equipment (xray, ventilator)
%Room r2 (Capacity: 6)
%s1 (3 hours) = 3/6 hours
%50% capacity utilization
%Has required equipment (ventilator)
%The solution is optimal because:
%All surgeries are assigned
%Room capacities are respected
%Equipment requirements are met
%Larger surgeries (s5, s2) were assigned first
%Good capacity utilization (overall ~83%)