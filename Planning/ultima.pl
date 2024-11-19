:- dynamic availability/3.
:- dynamic agenda_staff1/3.
:- dynamic room_availability/3.

% Updated doctor availability
agenda_staff(d001, 20241028, [(720, 1020, m01), (1080, 1200, c01)]).
agenda_staff(d002, 20241028, [(850, 1000, m02), (1380, 1440, c02)]).
agenda_staff(d003, 20241028, [(720, 960, m03)]).

% Staff definitions
staff(d001, doctor, orthopaedist, [so2, so3, so4]).
staff(d002, doctor, orthopaedist, [so2, so3, so4]).
staff(d003, doctor, orthopaedist, [so2, so3, so4]).

% Surgery definitions
surgery(so2, 45, 60, 45).  % Total = 150
surgery(so3, 45, 90, 45).  % Total = 180
surgery(so4, 45, 75, 45).  % Total = 165

% Surgery ID mapping to types
surgery_id(so100001, so2).
surgery_id(so100002, so3).
surgery_id(so100003, so4).
surgery_id(so100004, so2).
surgery_id(so100005, so4).

% Assignment of surgeries to doctors
assignment_surgery(so100001, d001).
assignment_surgery(so100002, d002).
assignment_surgery(so100003, d003).
assignment_surgery(so100004, d001).
assignment_surgery(so100005, d003).

% Main Scheduling Predicate
schedule_with_heuristic(HeuristicType, FormattedSchedule) :-
    Room = or1,
    Date = 20241028,
    retractall(availability(_, _, _)),
    retractall(room_availability(_, _, _)),
    initialize_agendas(Date),
    initialize_room_availability(Room, Date, 720, 1440), % Initialize room availability for full day
    findall(OpCode, surgery_id(OpCode, _), Operations),
    (HeuristicType = earliest_available ->
        schedule_by_earliest_available(Room, Date, Operations, Schedule)
    ;
        schedule_by_occupation_rate(Room, Date, Operations, Schedule)
    ),
    sort(2, @=<, Schedule, SortedSchedule), % Sort by StartTime
    format_schedule(SortedSchedule, FormattedSchedule).

% Initialize agendas and availability
initialize_agendas(Date) :-
    findall(_, (
        agenda_staff(Doctor, Date, Agenda),
        maplist(convert_slot, Agenda, FreeSlots),
        assertz(availability(Doctor, Date, FreeSlots)),
        writeln(initial_availability(Doctor, FreeSlots)) % Debug initial availability
    ), _).

initialize_room_availability(Room, Date, StartTime, EndTime) :-
    assertz(room_availability(Room, Date, [(StartTime, EndTime)])),
    writeln(initial_room_availability(Room, [(StartTime, EndTime)])).

convert_slot((Start, End, _), (Start, End)).

% Schedule operations using earliest available heuristic
schedule_by_earliest_available(Room, Date, Operations, Schedule) :-
    schedule_operations(Room, Operations, Date, [], Schedule).

schedule_operations(_, [], _, Schedule, Schedule). % Base case: no more operations.
schedule_operations(Room, [Op|Ops], Date, CurrentSchedule, FinalSchedule) :-
    surgery_id(Op, OpType),
    surgery(OpType, TAnesthesia, TSurgery, TCleaning),
    Duration is TAnesthesia + TSurgery + TCleaning, % Calculate total duration
    assignment_surgery(Op, Doctor),
    writeln(trying_to_fit(Op, Doctor, Room, Duration)), % Debug
    (   % Try to find a slot for the surgery
        find_earliest_slot(Doctor, Room, Date, Duration, StartTime) ->
        EndTime is StartTime + Duration - 1,
        writeln(scheduled(Op, Doctor, Room, StartTime, EndTime)), % Debug
        update_availability(Doctor, Date, StartTime, EndTime),
        update_room_availability(Room, Date, StartTime, EndTime),
        schedule_operations(Room, Ops, Date, [(Op, StartTime, EndTime)|CurrentSchedule], FinalSchedule)
    ;   % If no slot is found, skip the surgery
        writeln(cannot_schedule(Op, Doctor, Room)), % Debug
        schedule_operations(Room, Ops, Date, CurrentSchedule, FinalSchedule)
    ).

find_earliest_slot(Doctor, Room, Date, Duration, StartTime) :-
    availability(Doctor, Date, DoctorSlots),
    room_availability(Room, Date, RoomSlots),
    writeln(trying_to_fit_slot(Doctor, Duration, DoctorSlots)), % Debug doctor slots
    writeln(trying_to_fit_slot(Room, Duration, RoomSlots)),    % Debug room slots
    member((Start, End), DoctorSlots),
    member((RoomStart, RoomEnd), RoomSlots),
    StartTime is max(Start, RoomStart),  % Find overlapping slot
    EndTime is StartTime + Duration - 1,
    EndTime =< min(End, RoomEnd),        % Ensure slot fits
    !.                                   % Stop after finding the first valid slot

update_availability(Doctor, Date, StartTime, EndTime) :-
    availability(Doctor, Date, Slots),
    writeln(before_update(Doctor, Slots)), % Debug
    subtract_slot(Slots, (StartTime, EndTime), UpdatedSlots),
    writeln(after_update(Doctor, UpdatedSlots)), % Debug
    retractall(availability(Doctor, Date, _)),
    assertz(availability(Doctor, Date, UpdatedSlots)).

update_room_availability(Room, Date, StartTime, EndTime) :-
    room_availability(Room, Date, Slots),
    writeln(before_update_room(Room, Slots)), % Debug
    subtract_slot(Slots, (StartTime, EndTime), UpdatedSlots),
    writeln(after_update_room(Room, UpdatedSlots)), % Debug
    retractall(room_availability(Room, Date, _)),
    assertz(room_availability(Room, Date, UpdatedSlots)).

% Subtract a time slot
subtract_slot([], _, []).
subtract_slot([(Start, End)|Rest], (StartTime, EndTime), Updated) :-
    (End < StartTime ; Start > EndTime),
    subtract_slot(Rest, (StartTime, EndTime), Remaining),
    Updated = [(Start, End)|Remaining].
subtract_slot([(Start, End)|Rest], (StartTime, EndTime), Updated) :-
    subtract_slot(Rest, (StartTime, EndTime), Remaining),
    (
        Start < StartTime, End > EndTime ->
            BeforeEnd is StartTime - 1,
            AfterStart is EndTime + 1,
            Updated = [(Start, BeforeEnd), (AfterStart, End) | Remaining];
        Start < StartTime ->
            BeforeEnd is StartTime - 1,
            Updated = [(Start, BeforeEnd) | Remaining];
        End > EndTime ->
            AfterStart is EndTime + 1,
            Updated = [(AfterStart, End) | Remaining]
    ).

% Format schedule times to HH:MM
minutes_to_time(Minutes, Hours:FormattedMinutes) :-
    Hours is Minutes // 60,
    MinutesRemainder is Minutes mod 60,
    (MinutesRemainder < 10 ->
        FormattedMinutes = 0 + MinutesRemainder;
        FormattedMinutes = MinutesRemainder).

format_schedule([], []).
format_schedule([(Op, Start, End)|Rest], [(Op, StartTime, EndTime)|FormattedRest]) :-
    minutes_to_time(Start, StartTime),
    minutes_to_time(End, EndTime),
    format_schedule(Rest, FormattedRest).
