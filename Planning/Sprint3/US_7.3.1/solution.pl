%-------------------------------------------------------------------------
% DEFINIÇÕES DE CIRURGIAS
%-------------------------------------------------------------------------
surgery(so2,45,60,45).  % Cirurgia do Joelho
surgery(so3,45,90,45).  % Cirurgia do Ombro
surgery(so4,45,75,45).  % Cirurgia do Quadril


%-------------------------------------------------------------------------
% IDs DAS CIRURGIAS E SUAS ATRIBUIÇÕES
%-------------------------------------------------------------------------
% IDs das Cirurgias
surgery_id(so100001,so2).  % Joelho
surgery_id(so100002,so3).  % Ombro
surgery_id(so100003,so4).  % Quadril
surgery_id(so100004,so2).  % Joelho
surgery_id(so100005,so4).  % Quadril
surgery_id(so100006,so2).  % Joelho
surgery_id(so100007,so3).  % Ombro
surgery_id(so100008,so4).  % Quadril
surgery_id(so100009,so2).  % Joelho
surgery_id(so100010,so3).  % Ombro
surgery_id(so100011,so4).  % Quadril
surgery_id(so100012,so2).  % Joelho
surgery_id(so100013,so3).  % Ombro
surgery_id(so100014,so4).  % Quadril
surgery_id(so100015,so2).  % Joelho
surgery_id(so100016,so3).  % Ombro
surgery_id(so100017,so4).  % Quadril
surgery_id(so100018,so2).  % Joelho
surgery_id(so100019,so3).  % Ombro
surgery_id(so100020,so4).  % Quadril
surgery_id(so100021,so2).  % Joelho
surgery_id(so100022,so3).  % Ombro
surgery_id(so100023,so4).  % Quadril
surgery_id(so100024,so2).  % Joelho
surgery_id(so100025,so3).  % Ombro
surgery_id(so100026,so4).  % Quadril
surgery_id(so100027,so2).  % Joelho
surgery_id(so100028,so3).  % Ombro
surgery_id(so100029,so4).  % Quadril
surgery_id(so100030,so2).  % Joelho
surgery_id(so100031,so3).  % Ombro
surgery_id(so100032,so4).  % Quadril
surgery_id(so100033,so2).  % Joelho
surgery_id(so100034,so3).  % Ombro
surgery_id(so100035,so4).  % Quadril
surgery_id(so100036,so2).  % Joelho

%-------------------------------------------------------------------------
% AGENDAS DAS SALAS DE OPERAÇÃO
%-------------------------------------------------------------------------
% Pre-scheduled surgeries in or1
agenda_operation_room(or1, 20241028, [
    (0,179,so100001), 
    (250,379,so100002), 
    (400,579,so100003)
]).
agenda_operation_room(or2, 20241028, []).
agenda_operation_room(or3, 20241028, []).


%-------------------------------------------------------------------------
% AUXILIARY PREDICATES (free_agenda0, insert_agenda, etc.)
%-------------------------------------------------------------------------

% free_agenda0/2: Determine free slots from an agenda
free_agenda0([], [(0,1440)]).
free_agenda0([(0,Tfin,_)|LT], LT1) :- 
    !, free_agenda1([(0,Tfin,_)|LT], LT1).
free_agenda0([(Tin,Tfin,_)|LT], [(0,T1)|LT1]) :-
    T1 is Tin-1,
    free_agenda1([(Tin,Tfin,_)|LT], LT1).

free_agenda1([(_,Tfin,_)], [(T1,1440)]) :- 
    Tfin \== 1440, !, T1 is Tfin+1.
free_agenda1([(_,_,_)], []).
free_agenda1([(_,T,_),(T1,Tfin2,_)|LT], LT1) :-
    Tx is T+1, T1==Tx, !,
    free_agenda1([(T1,Tfin2,_)|LT], LT1).
free_agenda1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT], [(T1,T2)|LT1]) :-
    T1 is Tfin1+1,
    T2 is Tin2-1,
    free_agenda1([(Tin2,Tfin2,_)|LT], LT1).

% insert_agenda/3: Insert a new interval (Start,End,OpCode)
insert_agenda((TinS,TfinS,OpCode), [], [(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),
              [(Tin,Tfin,OpCode1)|LA],
              [(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]) :-
    TfinS < Tin, !.  
insert_agenda((TinS,TfinS,OpCode),
              [(Tin,Tfin,OpCode1)|LA],
              [(Tin,Tfin,OpCode1)|LA1]) :-
    insert_agenda((TinS,TfinS,OpCode), LA, LA1).

%-------------------------------------------------------------------------
% Conflict Detection
%-------------------------------------------------------------------------

% surgery_conflicts/4: Check if a surgery conflicts with existing ones
surgery_conflicts(Room, Day, Start, End) :-
    agenda_operation_room1(Room, Day, Agenda),
    member((ExistingStart, ExistingEnd, _), Agenda),
    Start =< ExistingEnd,
    End >= ExistingStart, !.


%-------------------------------------------------------------------------
% Improved Room Selection Logic
%-------------------------------------------------------------------------

find_best_room_slot(TotalTime, Rooms, Day, ChosenRoom, ChosenStart) :-
    % Calculate total occupied time for each room
    findall(OccupiedTime-(R,SlotStart), (
        member(R, Rooms),
        agenda_operation_room1(R, Day, RoomAgenda),
        % Calculate total occupied time
        findall(Duration, (
            member((Start, End, _), RoomAgenda),
            Duration is End - Start + 1
        ), Durations),
        sum_list(Durations, OccupiedTime),
        % Get the first valid slot for the surgery
        free_agenda0(RoomAgenda, FreeSlots),
        member((SlotStart, SlotEnd), FreeSlots),
        SlotDur is SlotEnd - SlotStart + 1,
        SlotDur >= TotalTime
    ), Candidates),
    % Ensure there are valid slots
    Candidates \= [],
    % Sort by total occupied time (ascending order: least occupied first)
    keysort(Candidates, SortedCandidates),
    SortedCandidates = [ OccupiedTime-(ChosenRoom, ChosenStart) | _ ],
    format('Choosing Room ~w (Occupied Time: ~w) for Surgery~n', [ChosenRoom, OccupiedTime]).



%-------------------------------------------------------------------------
% Main Logic for Assignment
%-------------------------------------------------------------------------

assign_surgeries_to_rooms(Day) :-
    findall(Room, agenda_operation_room(Room, Day, _), Rooms),
    forall(member(R, Rooms), (
    agenda_operation_room(R, Day, Agenda),
    assertz(agenda_operation_room1(R, Day, Agenda))
    )),
    findall(Dur-OpCode, (
        surgery_id(OpCode,Type),
        surgery(Type,Anest,Surg,Clean),
        Dur is Anest + Surg + Clean
    ), Pairs),
    keysort(Pairs, SortedAsc),
    reverse(SortedAsc, SortedDesc),
    findall(SurgID, member(_-SurgID, SortedDesc), OrderedSurgeries),
    assign_surgeries(OrderedSurgeries, Rooms, Day),
    report_assignments(Day).

% Check if a surgery has already been scheduled
surgery_already_scheduled(Surgery, Rooms, Day) :-
    member(Room, Rooms),
    agenda_operation_room1(Room, Day, Agenda),
    member((_Start, _End, Surgery), Agenda), !.

% Main predicate for scheduling surgeries
assign_surgeries([], _, _).
assign_surgeries([CurrSurg|Rest], Rooms, Day) :-
    ( surgery_already_scheduled(CurrSurg, Rooms, Day) ->
        format('Skipping already scheduled surgery: ~w~n', [CurrSurg])
    ;
        surgery_id(CurrSurg, Type),
        surgery(Type, A, S, C),
        Needed is A + S + C,
        ( find_best_room_slot(Needed, Rooms, Day, BestRoom, BestStart),
          End is BestStart + Needed - 1,
          \+ surgery_conflicts(BestRoom, Day, BestStart, End) ->
            schedule_in_room(CurrSurg, BestRoom, Day, BestStart, End),
            format('Scheduled ~w in ~w from ~w to ~w~n', [CurrSurg, BestRoom, BestStart, End])
        ;
            format('Unable to schedule ~w (no valid slots or conflicts).~n', [CurrSurg])
        )
    ),
    assign_surgeries(Rest, Rooms, Day).


schedule_in_room(Surgery, Room, Day, Start, End) :-
    retract(agenda_operation_room1(Room, Day, OldAgenda)),
    insert_agenda((Start,End,Surgery), OldAgenda, NewAgenda),
    assertz(agenda_operation_room1(Room, Day, NewAgenda)).

%-------------------------------------------------------------------------
% Reporting
%-------------------------------------------------------------------------

report_assignments(Day) :-
    format('~n=== Final Room Assignments ===~n'),
    findall(Room, agenda_operation_room1(Room, Day, _), Rooms),
    sort(Rooms, SortedRooms),
    forall(member(R, SortedRooms),
      (
        format('~nRoom ~w:~n',[R]),
        agenda_operation_room1(R, Day, Agenda),
        ( Agenda = [] ->
            format('  (No surgeries scheduled)~n')
          ; print_room_schedule(Agenda)
        )
      )
    ).

print_room_schedule([]).
print_room_schedule([(Start,End,Surg)|Rest]) :-
    surgery_id(Surg, Type),
    format('  ~w: ~w - ~w (type: ~w)~n', [Surg, Start, End, Type]),
    print_room_schedule(Rest).


% print_agenda/2: Print room's agenda for debugging
print_agenda(Room, Day) :-
    agenda_operation_room1(Room, Day, Agenda),
    format('Agenda for Room ~w on Day ~w:~n', [Room, Day]),
    (   Agenda = [] 
    ->  format('  (No surgeries scheduled)~n')
    ;   maplist(print_surgery_entry, Agenda)
    ).

% Helper predicate to print individual entries
print_surgery_entry((Start, End, Surg)) :-
    format('  Surgery: ~w | Start: ~w | End: ~w~n', [Surg, Start, End]).
