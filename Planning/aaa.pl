:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:- dynamic agenda_operation_room/3.
:- dynamic agenda_operation_room1/3.

% Mock Data
% ---------

% Room availability
room(or1, [(480, 840), (900, 1200)]).  % Room available from 8:00 to 14:00, with a break from 14:00

% Staff availability
agenda_staff(d001, 20241028, [(720, 790, m01), (1080, 1140, c01)]).
agenda_staff(d002, 20241028, [(850, 900, m02), (901, 960, m02), (1380, 1440, c02)]).
agenda_staff(d003, 20241028, [(720, 790, m01), (910, 980, m02)]).  % Doctor d003 with availability that can be scheduled

% Timetables (full available working hours)
timetable(d001, 20241028, (480, 1200)).
timetable(d002, 20241028, (500, 1440)).
timetable(d003, 20241028, (520, 1320)).

% Staff definitions
staff(d001, doctor, orthopaedist, [so2, so3, so4]).
staff(d002, doctor, orthopaedist, [so2, so3, so4]).
staff(d003, doctor, orthopaedist, [so2, so3, so4]).

% Surgery definitions
% surgery(SurgeryType, TAnesthesia, TSurgery, TCleaning).
surgery(so2, 45, 60, 45).
surgery(so3, 45, 90, 45).
surgery(so4, 45, 75, 45).

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

% Existing operation room agenda
agenda_operation_room(or1, 20241028, [(520, 579, so100000), (1000, 1059, so099999)]).

% Scheduling Logic
% ----------------

% Generate a "good" schedule (non-optimal but efficient) for the specified room and date.
generate_good_schedule(Room, Date, Schedule) :-
    % Clear dynamic facts to start fresh
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(availability(_, _, _)),

    % Initialize agendas for the room and staff
    initialize_agendas(Room, Date),

    % Collect all operation codes and schedule them in a greedy manner
    findall(OpCode, surgery_id(OpCode, _), OperationCodes),
    schedule_operations(Room, Date, OperationCodes, Schedule).

% Initialize room and staff agendas
initialize_agendas(Room, Date) :-
    % Assert dynamic agenda facts for staff
    findall(_, (
        agenda_staff(Doctor, Date, Agenda),
        assertz(agenda_staff1(Doctor, Date, Agenda))
    ), _),
    
    % Initialize the room agenda
    agenda_operation_room(Room, Date, RoomAgenda),
    assertz(agenda_operation_room1(Room, Date, RoomAgenda)),

    % Calculate availability for each doctor based on their timetable
    findall(_, (
        agenda_staff1(Doctor, Date, BusySlots),
        free_agenda0(BusySlots, FreeSlots),
        adapt_timetable(Doctor, Date, FreeSlots, AdjustedSlots),
        assertz(availability(Doctor, Date, AdjustedSlots))
    ), _).

% Schedule operations in a greedy, first-fit manner
schedule_operations(_, _, [], []). % Base case: no more operations to schedule

schedule_operations(Room, Date, [OpCode | RestOperations], [(OpCode, Start, End) | Scheduled]) :-
    % Get the surgery type and required duration
    surgery_id(OpCode, OpType),
    surgery(OpType, _, TSurgery, _),

    % Find doctors assigned to the surgery
    findall(Doctor, assignment_surgery(OpCode, Doctor), AssignedDoctors),

    % Find the earliest possible slot for the surgery
    find_earliest_available_slot(Room, Date, TSurgery, AssignedDoctors, (Start, End)),

    % Update the room and doctor agendas with the scheduled time slot
    update_room_and_doctor_agendas(Room, Date, (Start, End), OpCode, AssignedDoctors),

    % Recurse to schedule the remaining operations
    schedule_operations(Room, Date, RestOperations, Scheduled).

% Find the earliest available slot in both room and doctors' agendas
find_earliest_available_slot(Room, Date, Duration, Doctors, (Start, End)) :-
    % Intersect all agendas of assigned doctors
    intersect_all_agendas(Doctors, Date, DoctorAvailability),

    % Get free slots in the room agenda
    agenda_operation_room1(Room, Date, RoomAgenda),
    free_agenda0(RoomAgenda, RoomFreeSlots),

    % Find common free intervals between room and doctor availability
    intersect_2_agendas(DoctorAvailability, RoomFreeSlots, CommonFreeSlots),

    % Select the first interval that fits the surgery duration
    select_first_fit(CommonFreeSlots, Duration, (Start, End)).

% Update the room and doctor agendas after scheduling
update_room_and_doctor_agendas(Room, Date, (Start, End), OpCode, Doctors) :-
    % Update the room agenda
    retract(agenda_operation_room1(Room, Date, RoomAgenda)),
    insert_agenda((Start, End, OpCode), RoomAgenda, UpdatedRoomAgenda),
    assertz(agenda_operation_room1(Room, Date, UpdatedRoomAgenda)),

    % Update each doctor's agenda
    update_doctor_agendas(Date, (Start, End), OpCode, Doctors).

% Update agendas for each doctor
update_doctor_agendas(_, _, _, []).
update_doctor_agendas(Date, (Start, End), OpCode, [Doctor | RestDoctors]) :-
    retract(agenda_staff1(Doctor, Date, DoctorAgenda)),
    insert_agenda((Start, End, OpCode), DoctorAgenda, UpdatedDoctorAgenda),
    assertz(agenda_staff1(Doctor, Date, UpdatedDoctorAgenda)),
    update_doctor_agendas(Date, (Start, End), OpCode, RestDoctors).

% Select the first interval that fits the surgery duration
select_first_fit([(Start, End) | _], Duration, (Start, FitEnd)) :-
    End - Start + 1 >= Duration,
    FitEnd is Start + Duration - 1.

select_first_fit([_ | Rest], Duration, FitInterval) :-
    select_first_fit(Rest, Duration, FitInterval).

% Helper predicates for handling free intervals and agenda adjustments
free_agenda0([], [(0, 1440)]).
free_agenda0([(0, Tfin, _) | LT], LT1) :- !, free_agenda1([(0, Tfin, _) | LT], LT1).
free_agenda0([(Tin, Tfin, _) | LT], [(0, T1) | LT1]) :-
    T1 is Tin - 1,
    free_agenda1([(Tin, Tfin, _) | LT], LT1).

free_agenda1([(_, Tfin, _)], [(T1, 1440)]) :- Tfin \== 1440, !, T1 is Tfin + 1.
free_agenda1([(_, _, _)], []).
free_agenda1([(_, T, _), (T1, Tfin2, _) | LT], LT1) :- Tx is T + 1, T1 == Tx, !, free_agenda1([(T1, Tfin2, _) | LT], LT1).
free_agenda1([(_, Tfin1, _), (Tin2, Tfin2, _) | LT], [(T1, T2) | LT1]) :-
    T1 is Tfin1 + 1, T2 is Tin2 - 1,
    free_agenda1([(Tin2, Tfin2, _) | LT], LT1).

adapt_timetable(D, Date, LFA, LFA2) :-
    timetable(D, Date, (InTime, FinTime)),
    treatin(InTime, LFA, LFA1),
    treatfin(FinTime, LFA1, LFA2).

treatin(InTime, [(In, Fin) | LFA], [(In, Fin) | LFA]) :- InTime =< In, !.
treatin(InTime, [(_, Fin) | LFA], LFA1) :- InTime > Fin, !, treatin(InTime, LFA, LFA1).
treatin(InTime, [(_, Fin) | LFA], [(InTime, Fin) | LFA]).
treatin(_, [], []).

treatfin(FinTime, [(In, Fin) | LFA], [(In, Fin) | LFA1]) :- FinTime >= Fin, !, treatfin(FinTime, LFA, LFA1).
treatfin(FinTime, [(In, _) | _], []) :- FinTime =< In, !.
treatfin(FinTime, [(In, _) | _], [(In, FinTime)]).
treatfin(_, [], []).

intersect_all_agendas([Name], Date, LA) :- !, availability(Name, Date, LA).
intersect_all_agendas([Name | LNames], Date, LI) :-
    availability(Name, Date, LA),
    intersect_all_agendas(LNames, Date, LI1),
    intersect_2_agendas(LA, LI1, LI).

intersect_2_agendas([], _, []).
intersect_2_agendas([D | LD], LA, LIT) :-
    intersect_availability(D, LA, LI, LA1),
    intersect_2_agendas(LD, LA1, LID),
    append(LI, LID, LIT).

intersect_availability((_, _), [], [], []).
intersect_availability((_, Fim), [(Ini1, Fim1) | LD], [], [(Ini1, Fim1) | LD]) :- Fim < Ini1, !.
intersect_availability((Ini, Fim), [(_, Fim1) | LD], LI, LA) :- Ini > Fim1, !, intersect_availability((Ini, Fim), LD, LI, LA).
intersect_availability((Ini, Fim), [(Ini1, Fim1) | LD], [(Imax, Fmin)], [(Fim, Fim1) | LD]) :-
    Fim1 > Fim, !,
    min_max(Ini, Ini1, _, Imax),
    min_max(Fim, Fim1, Fmin, _).

intersect_availability((Ini, Fim), [(Ini1, Fim1) | LD], [(Imax, Fmin) | LI], LA) :-
    Fim >= Fim1, !,
    min_max(Ini, Ini1, _, Imax),
    min_max(Fim, Fim1, Fmin, _),
    intersect_availability((Fim1, Fim), LD, LI, LA).

min_max(I, I1, I, I1) :- I < I1, !.
min_max(I, I1, I1, I).

insert_agenda((TinS, TfinS, OpCode), [], [(TinS, TfinS, OpCode)]).
insert_agenda((TinS, TfinS, OpCode), [(Tin, Tfin, OpCode1) | LA], [(TinS, TfinS, OpCode), (Tin, Tfin, OpCode1) | LA]) :-
    TfinS < Tin, !.
insert_agenda((TinS, TfinS, OpCode), [(Tin, Tfin, OpCode1) | LA], [(Tin, Tfin, OpCode1) | LA1]) :-
    insert_agenda((TinS, TfinS, OpCode), LA, LA1).

% Test the code
% -------------
% To run the scheduling, use the following query:
% ?- generate_good_schedule(or1, 20241028, Schedule).
