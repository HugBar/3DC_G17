:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.
:- dynamic scheduled_surgery/1.
:- dynamic failed_surgery/1.

agenda_staff(d001,20241028,[]).
agenda_staff(d002,20241028,[]).
agenda_staff(d003,20241028,[]).
agenda_staff(d005,20241028,[]).
agenda_staff(d004,20241028,[]).
agenda_staff(n004,20241028,[(510,600,m01)]).
agenda_staff(m001,20241028,[]).
agenda_staff(a001,20241028,[(480,1100,m01)]).
agenda_staff(n001,20241028,[]).
agenda_staff(n002,20241028,[]).
agenda_staff(n003,20241028,[(820,910,m01)]).

timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).
timetable(d005,20241028,(520,1320)).
timetable(d004,20241028,(510,1310)).
timetable(n004,20241028,(510,1310)).
timetable(m001,20241028,(480,1200)).
timetable(a001,20241028,(480,1200)).
timetable(n001,20241028,(480,1200)).
timetable(n002,20241028,(480,1200)).
timetable(n003,20241028,(480,1200)).


% surgery_requirements(SurgeryType, StaffRequirements)
% StaffRequirements = [Phase1Requirements, Phase2Requirements, Phase3Requirements]
% Each PhaseRequirements = [Role-Number, Role-Number, ...]
surgery_requirements(so2, [
    % Phase 1 - Anesthesia/preparation
    [doctor-anaesthetist-1, nurse-anaesthetist-1],
    % Phase 2 - Surgery
    [doctor-orthopaedist-3, nurse-instrumentation-1, nurse-circulating-1, doctor-anaesthetist-1, nurse-anaesthetist-1],
    % Phase 3 - Cleaning
    [medical-assistant-1]
]).

surgery_requirements(so3, [
    % Phase 1 - Anesthesia/preparation
    [doctor-anaesthetist-1, nurse-anaesthetist-1],
    % Phase 2 - Surgery
    [doctor-orthopaedist-3, doctor-anaesthetist-1, nurse-anaesthetist-1, nurse-circulating-1, nurse-instrumentation-1],
    % Phase 3 - Cleaning
    [medical-assistant-1]
]).

surgery_requirements(so4, [
    % Phase 1 - Anesthesia/preparation
    [doctor-anaesthetist-1, nurse-anaesthetist-1],
    % Phase 2 - Surgery
    [doctor-orthopaedist-3, doctor-anaesthetist-1, nurse-anaesthetist-1, nurse-circulating-1, nurse-instrumentation-1],
    % Phase 3 - Cleaning
    [medical-assistant-1]
]).



% staff(StaffID, Role, Speciality, Operations)
staff(d001, doctor, orthopaedist, [so2,so3,so4]).
staff(d002, doctor, orthopaedist, [so2,so3,so4]).
staff(d003, doctor, orthopaedist, [so2,so3,so4]).
staff(d005, doctor, orthopaedist, [so2,so3,so4]).
staff(d004, doctor, anaesthetist, [so2,so3,so4]).
staff(a001, nurse, anaesthetist, [so2,so3,so4]).
staff(n001, nurse, instrumentation, [so2,so3,so4]).
staff(n002, nurse, circulating, [so2,so3,so4]).
staff(n003, nurse, anaesthetist, [so2,so3,so4]).
staff(m001, medical, assistant, [so2,so3,so4]).
staff(n004, nurse, anaesthetist, [so2,so3,so4]).



% surgery(SurgeryType, TPrep, TSurgery, TCleaning)
surgery(so2, 45, 60, 45).
surgery(so3, 45, 60, 30).
surgery(so4, 45,50,30).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so4).
surgery_id(so100006,so2).  % Joelho
surgery_id(so100007,so3).  % Ombro
surgery_id(so100008,so4).  % Quadril
surgery_id(so100009,so2).  % Joelho
surgery_id(so100010,so3).  % Ombro

agenda_operation_room(or1,20241028,[(520,579,so100000), (1000,1059,so099999)]).
agenda_operation_room(or2,20241028,[]).


cleanup_all :-
    retractall(scheduled_surgery(_)),
    retractall(failed_surgery(_)), 
    retractall(availability(_, _, _)),
    retractall(agenda_staff1(_, _, _)),
    retractall(agenda_operation_room1(_, _, _)),
    retractall(better_sol(_, _, _, _, _)).

% Setup predicate
setup_test(Room) :-
    % Copy initial agendas to agenda_staff1
    findall(_, (
        agenda_staff(Staff, Day, Agenda),
        assertz(agenda_staff1(Staff, Day, Agenda))
    ), _),
    % Initialize room agenda
    agenda_operation_room(Room, 20241028, RoomAgenda),
    assertz(agenda_operation_room1(Room, 20241028, RoomAgenda)),
    % Initialize availabilities
    findall(_, (
        agenda_staff(D, 20241028, Agenda),
        free_agenda0(Agenda, LFA),
        adapt_timetable(D, 20241028, LFA, LFA2),
        assertz(availability(D, 20241028, LFA2))
    ), _).

% Test predicate
test_availability_operation(OpCode) :-
    cleanup_all,
    setup_test,
    availability_operation(OpCode, or1, 20241028, LPossibilities, StaffByPhase),
    write('Possibilities: '), write(LPossibilities), nl,
    write('Staff by phase: '), write(StaffByPhase), nl.

free_agenda0([],[(0,1440)]).
free_agenda0([(0,Tfin,_)|LT],LT1):-!,free_agenda1([(0,Tfin,_)|LT],LT1).
free_agenda0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda1([(Tin,Tfin,_)|LT],LT1).

free_agenda1([(_,Tfin,_)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda1([(_,_,_)],[]).
free_agenda1([(_,T,_),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda1([(T1,Tfin2,_)|LT],LT1).
free_agenda1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda1([(Tin2,Tfin2,_)|LT],LT1).


take_n([], _, _, []).
take_n([Role-Specialty-Number|RestReqs], AllStaff, Day, SelectedStaff) :-
    % Filter available staff for the specific role and specialty
    % write('Selecting staff for role '), write(Role), write(' and specialty '), write(Specialty), nl,
    findall(Staff-AvailableTime, (
        member(Staff, AllStaff),
        staff(Staff, Role, Specialty, _),
        availability(Staff, Day, StaffAgenda),
        \+ StaffAgenda = [], % Ensure the staff has available agenda
        % Calculate total available time
        calculate_total_available_time(StaffAgenda, AvailableTime)
    ), StaffWithTime),
    
    % Sort staff by available time (descending order)
    sort_staff_by_time(StaffWithTime, SortedStaffWithTime),
    
    % Extract just the staff IDs from sorted pairs
    extract_staff_ids(SortedStaffWithTime, RoleStaff),
    
    % Limit the number of staff based on the requirement
    length(RoleStaff, L),
    (L >= Number ->
        length(LimitedRoleStaff, Number),
        append(LimitedRoleStaff, _, RoleStaff)
    ;
        LimitedRoleStaff = RoleStaff
    ),
    
    % Continue with the rest of the requirements
    take_n(RestReqs, AllStaff, Day, RestSelectedStaff),
    
    % Combine the selected staff
    append(LimitedRoleStaff, RestSelectedStaff, SelectedStaff).

% Calculate total available time from agenda slots
calculate_total_available_time([], 0).
calculate_total_available_time([(Start, End)|Rest], TotalTime) :-
    calculate_total_available_time(Rest, RestTime),
    Time is End - Start,
    TotalTime is Time + RestTime.

% Sort staff by available time (descending)
sort_staff_by_time(StaffWithTime, SortedStaffWithTime) :-
    sort(2, @>=, StaffWithTime, SortedStaffWithTime).

% Extract staff IDs from Staff-Time pairs
extract_staff_ids([], []).
extract_staff_ids([Staff-_|Rest], [Staff|StaffIds]) :-
    extract_staff_ids(Rest, StaffIds).

intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),
					intersect_2_agendas(LD,LA1,LID),
					append(LI,LID,LIT).

intersect_availability((_,_),[],[],[]).

intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
		Fim<Ini1,!.

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
		Ini>Fim1,!,
		intersect_availability((Ini,Fim),LD,LI,LA).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
		Fim1>Fim,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
		Fim>=Fim1,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_),
		intersect_availability((Fim1,Fim),LD,LI,LA).


min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).

remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).

% New predicate for inserting staff agenda for a specific phase
insert_agenda_staff_phase(_,_,[]).
insert_agenda_staff_phase((TinS,TfinS,OpCode),Day,[Staff|RestStaff]):-
    (agenda_staff1(Staff,Day,CurrentAgenda) ->
        % Check if this exact time slot is already scheduled
        (member((TinS,TfinS,OpCode), CurrentAgenda) ->
            % If already scheduled, skip
            true
        ;
            % If not scheduled, update agenda
            retract(agenda_staff1(Staff,Day,CurrentAgenda)),
            insert_agenda((TinS,TfinS,OpCode),CurrentAgenda,NewAgenda),
            assertz(agenda_staff1(Staff,Day,NewAgenda))
        )
    ;
        % If no agenda exists, create new one
        assertz(agenda_staff1(Staff,Day,[(TinS,TfinS,OpCode)]))
    ),
    insert_agenda_staff_phase((TinS,TfinS,OpCode),Day,RestStaff).

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

% Main predicate to schedule a list of surgeries
schedule_surgeries_list(List, Room, Day) :-
    cleanup_all,
    setup_test(Room),
               
    % Process each surgery in the list
    process_surgeries(List, Room, Day).

% Base case - empty list
process_surgeries([],_, _).

% Recursive case - process each surgery
process_surgeries([Surgery|Rest], Room, Day) :-
    % Try to schedule current surgery
    (try_schedule_surgery(Surgery, Room, Day) ->
        assertz(scheduled_surgery(Surgery))
    ;
        assertz(failed_surgery(Surgery))
    ),
    % Process remaining surgeries
    process_surgeries(Rest, Room, Day).

% Attempt to schedule a single surgery
try_schedule_surgery(OpCode, Room, Day) :-
    % Get surgery details
    surgery_id(OpCode, SurgeryType),
    surgery(SurgeryType, TPrep, TSurgery, TCleaning),
    surgery_requirements(SurgeryType, [Phase1Reqs, Phase2Reqs, Phase3Reqs]),

    
    findall(Staff1, (
        member(Role-Specialty-Number, Phase1Reqs),
        staff(Staff1, Role, Specialty, _)
        % availability(Staff1, Day, _)
    ), AllPhase1Staff),
     take_n(Phase1Reqs, AllPhase1Staff, Day, Phase1Staff),

    findall(Staff2, (
        member(Role-Specialty-Number, Phase2Reqs),
        staff(Staff2, Role, Specialty, _),
        Specialty \= anaesthetist
    ), AllPhase2Staff),
    append(Phase1Staff, AllPhase2Staff, AllPhase1And2Staff),
    take_n(Phase2Reqs, AllPhase1And2Staff, Day, Phase2Staff),

    findall(Staff3, (
        member(Role-Specialty-Number, Phase3Reqs),
        staff(Staff3, Role, Specialty, _)
    ), AllPhase3Staff),
    take_n(Phase3Reqs, AllPhase3Staff, Day, Phase3Staff),
    
    % Get availabilities
    intersect_all_agendas(Phase1Staff, Day, Phase1Avail),
    intersect_all_agendas(Phase2Staff, Day, Phase2Avail),
    intersect_all_agendas(Phase3Staff, Day, Phase3Avail),
    
    % Get room availability
    agenda_operation_room1(Room, Day, RoomAgenda),
    free_agenda0(RoomAgenda, RoomAvail),
    
    % Find common time slot
    intersect_2_agendas(Phase1Avail, RoomAvail, Temp1),
    intersect_2_agendas(Phase2Avail, Temp1, Temp2),
    intersect_2_agendas(Phase3Avail, Temp2, FinalAvail),
    
    % Get valid intervals
    TTotal is TPrep + TSurgery + TCleaning,
    remove_unf_intervals(TTotal, FinalAvail, [FirstSlot|_]),
    
    % Schedule the surgery
    FirstSlot = (StartTime, _),
    EndTime is StartTime + TTotal - 1,
    
    % Update room agenda
    retract(agenda_operation_room1(Room, Day, OldRoomAgenda)),
    insert_agenda((StartTime, EndTime, OpCode), OldRoomAgenda, NewRoomAgenda),
    assertz(agenda_operation_room1(Room, Day, NewRoomAgenda)),
    
    % Calculate phase times
    TPreparationEnd is StartTime + TPrep - 1,
    TSurgeryStart is TPreparationEnd + 1,
    TSurgeryEnd is TSurgeryStart + TSurgery - 1,
    TCleaningStart is TSurgeryEnd + 1,
    TCleaningEnd is EndTime,
    
    % Update staff agendas
    insert_agenda_staff_phase((StartTime, TPreparationEnd, OpCode), Day, Phase1Staff),
    insert_agenda_staff_phase((TSurgeryStart, TSurgeryEnd, OpCode), Day, Phase2Staff),
    insert_agenda_staff_phase((TCleaningStart, TCleaningEnd, OpCode), Day, Phase3Staff).

