:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.


agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(850,900,m02),(901,960,m02),(1380,1440,c02)]).
agenda_staff(d003,20241028,[(720,790,m01),(910,980,m02)]).
agenda_staff(n001,20241028,[(720,790,m01)]).
agenda_staff(n002,20241028,[(850,900,m02)]).
agenda_staff(a001,20241028,[(720,790,m01),(940,980,c04)]).
agenda_staff(na001,20241028,[(720,790,m01)]).
agenda_staff(ma001,20241028,[(850,900,m02)]).


timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).
timetable(n001,20241028,(480,1200)).
timetable(n002,20241028,(500,1440)).
timetable(a001,20241028,(520,1320)).
timetable(na001,20241028,(480,1200)).
timetable(ma001,20241028,(500,1440)).

staff(d001, doctor, orthopaedist, [so2,so3,so4]).
staff(d002, doctor, orthopaedist, [so2,so3,so4]).
staff(d003, doctor, orthopaedist, [so2,so3,so4]).
staff(n001, nurse, instrumenting_nurse, [so2,so3,so4]).
staff(n002, nurse, circulating_nurse, [so2,so3,so4]).
staff(a001, doctor, anaesthetist, [so2,so3,so4]).
staff(na001, nurse, nurse_anaesthetist, [so2,so3,so4]).
staff(ma001, assistant, medical_action, [so2,so3,so4]).

% Surgery phases definition with required roles
surgery_phases(so2, [
    (anesthesia, 45, [anaesthetist, nurse_anaesthetist]),
    (surgery, 60, [orthopaedist, instrumenting_nurse, circulating_nurse, anaesthetist, nurse_anaesthetist]),
    (cleaning, 45, [medical_action])
]).
surgery_phases(so3, [
    (anesthesia, 45, [anaesthetist, nurse_anaesthetist]),
    (surgery, 90, [orthopaedist, instrumenting_nurse, circulating_nurse, anaesthetist, nurse_anaesthetist]),
    (cleaning, 45, [medical_action])
]).
surgery_phases(so4, [
    (anesthesia, 45, [anaesthetist, nurse_anaesthetist]),
    (surgery, 75, [orthopaedist, instrumenting_nurse, circulating_nurse, anaesthetist, nurse_anaesthetist]),
    (cleaning, 45, [medical_action])
]).

%surgery(SurgeryType,TAnesthesia,TSurgery,TCleaning).

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).

assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).

agenda_operation_room(or1,20241028,[(520,579,so100000),(1000,1059,so099999)]).

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


adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).


intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

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




schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Or,Date,Agenda),assert(agenda_operation_room1(Or,Date,Agenda)),
    findall(_,(agenda_staff1(D,Date,L),free_agenda0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_),
    findall(OpCode,surgery_id(OpCode,_),LOpCode),

    availability_all_surgeries(LOpCode,Room,Day),!.

% Atualizar availability_all_surgeries para usar as fases
availability_all_surgeries([],_,_).
availability_all_surgeries([OpCode|LOpCode],Room,Day):-
    surgery_id(OpCode,OpType),
    surgery_phases(OpType, Phases),
    get_total_time(Phases, TotalTime),
    availability_operation(OpCode,Room,Day,LPossibilities,LStaffByPhase),
    schedule_first_interval(TotalTime,LPossibilities,(TinS,TfinS)),
    retract(agenda_operation_room1(Room,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assertz(agenda_operation_room1(Room,Day,Agenda1)),
    insert_agenda_staff_phases((TinS,TfinS),Day,LStaffByPhase),
    availability_all_surgeries(LOpCode,Room,Day).

% Novo predicado para inserir agenda por fases
insert_agenda_staff_phases((_,_),_,[]).
insert_agenda_staff_phases((TinS,TfinTotal),Day,[(Phase,Time,Staff)|Rest]) :-
    % Calcula o tempo de início e fim para esta fase específica
    calculate_phase_time((TinS,TfinTotal), Phase, Time, (PhaseStart,PhaseEnd)),
    % Insere a agenda para cada membro do staff desta fase
    insert_agenda_staff((PhaseStart,PhaseEnd),Day,Staff),
    % Continua com as próximas fases
    insert_agenda_staff_phases((TinS,TfinTotal),Day,Rest).

% Calcula o tempo para cada fase específica
calculate_phase_time((TinS,_), anesthesia, Time, (TinS,TfinPhase)) :-
    TfinPhase is TinS + Time - 1.
calculate_phase_time((TinS,_), surgery, Time, (TinPhase,TfinPhase)) :-
    % A cirurgia começa após a anestesia
    TinPhase is TinS + 45, % Assumindo que anestesia = 45min
    TfinPhase is TinPhase + Time - 1.
calculate_phase_time((_,TfinTotal), cleaning, Time, (TinPhase,TfinTotal)) :-
    % Limpeza é a última fase
    TinPhase is TfinTotal - Time + 1.

% Substitui o antigo insert_agenda_doctors
insert_agenda_staff(_,_,[]).
insert_agenda_staff((TinS,TfinS),Day,[Staff|RestStaff]) :-
    retract(agenda_staff1(Staff,Day,Agenda)),
    insert_agenda((TinS,TfinS,'surgery'),Agenda,Agenda1),
    assert(agenda_staff1(Staff,Day,Agenda1)),
    insert_agenda_staff((TinS,TfinS),Day,RestStaff).

% Calcular tempo total de uma cirurgia somando todas as fases
get_total_time(Phases, TotalTime) :-
    findall(Time, member((_,Time,_), Phases), Times),
    sum_list(Times, TotalTime).



% Modificar availability_operation para limitar o horário final
availability_operation(OpCode, Room, Day, LPossibilities, LStaffByPhase) :-
    surgery_id(OpCode, OpType),
    surgery_phases(OpType, Phases),
    
    % Encontrar slots possíveis
    findall((StartTime, EndTime), (
        availability(_, Day, [(Start,End)|_]),
        between(Start, End, StartTime),
        
        % Calcular tempos das fases
        member((anesthesia, AnesthesiaTime, AnesthesiaRoles), Phases),
        member((surgery, SurgeryTime, SurgeryRoles), Phases),
        member((cleaning, CleaningTime, CleaningRoles), Phases),
        
        % Calcular tempos
        SurgeryStart is StartTime + AnesthesiaTime,
        SurgeryEnd is SurgeryStart + SurgeryTime - 1,
        CleaningStart is SurgeryEnd + 1,
        EndTime is CleaningStart + CleaningTime - 1,
        
        % Verificar se cabe no intervalo e não sobrepõe com outras cirurgias
        check_room_availability(Room, Day, StartTime, EndTime),
        
        % Verificar disponibilidade do staff
        check_staff_availability(Day, StartTime, SurgeryEnd, AnesthesiaRoles),
        check_staff_availability(Day, SurgeryStart, SurgeryEnd, SurgeryRoles),
        check_staff_availability(Day, CleaningStart, EndTime, CleaningRoles)
    ), AllPossibilities),
    
    sort(AllPossibilities, LPossibilities),
    get_staff_by_phase(OpType, Phases, LStaffByPhase).

% Novo predicado para verificar disponibilidade do staff
check_staff_availability(Day, Start, End, Roles) :-
    forall(member(Role, Roles), (
        staff(Member, _, Role, _),
        availability(Member, Day, Avail),
        member((AvStart, AvEnd), Avail),
        Start >= AvStart,
        End =< AvEnd
    )).

% Encontrar slot para anestesia
find_anesthesia_slot(Day, Time, Roles, StartTime) :-
    % Encontrar slots onde anestesistas estão disponíveis
    findall((Start,End), (
        % Pegar um membro da equipe de anestesia
        member(Role, Roles),
        staff(Member, _, Role, _),
        availability(Member, Day, Avail),
        member((Start,End), Avail),
        % Verificar se há tempo suficiente para anestesia
        End - Start >= Time,
        % Verificar se todos os outros membros necessários também estão disponíveis
        forall(member(OtherRole, Roles), (
            staff(OtherMember, _, OtherRole, _),
            availability(OtherMember, Day, OtherAvail),
            member((OStart,OEnd), OtherAvail),
            Start >= OStart,
            Start + Time =< OEnd
        ))
    ), Slots),
    % Ordenar slots por início mais cedo
    sort(Slots, [(StartTime,_)|_]).

% Verificar disponibilidade do staff cirúrgico
check_surgery_staff(Day, Start, End, Roles) :-
    forall(member(Role, Roles), (
        staff(Member, _, Role, _),
        availability(Member, Day, Avail),
        member((AvStart, AvEnd), Avail),
        Start >= AvStart,
        End =< AvEnd
    )).

% Verificar disponibilidade dos anestesistas durante a cirurgia
check_anesthesia_staff(Day, Start, End, Roles) :-
    forall(member(Role, Roles), (
        staff(Member, _, Role, _),
        availability(Member, Day, Avail),
        member((AvStart, AvEnd), Avail),
        Start >= AvStart,
        End =< AvEnd
    )).

% Verificar disponibilidade do staff de limpeza
check_cleaning_staff(Day, Start, End, Roles) :-
    forall(member(Role, Roles), (
        staff(Member, _, Role, _),
        availability(Member, Day, Avail),
        member((AvStart, AvEnd), Avail),
        Start >= AvStart,
        End =< AvEnd
    )).

% Obter staff por fase
get_staff_by_phase(OpType, Phases, LStaffByPhase) :-
    findall((Phase, Time, Staff), (
        member((Phase, Time, RequiredRoles), Phases),
        findall(Member, (
            member(Role, RequiredRoles),
            staff(Member, _, Role, Types),
            member(OpType, Types)
        ), Staff)
    ), LStaffByPhase).


remove_unf_intervals(_,[],[]).
remove_unf_intervals(TSurgery,[(Tin,Tfin)|LA],[(Tin,Tfin)|LA1]):-DT is Tfin-Tin+1,TSurgery=<DT,!,
    remove_unf_intervals(TSurgery,LA,LA1).
remove_unf_intervals(TSurgery,[_|LA],LA1):- remove_unf_intervals(TSurgery,LA,LA1).


schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]) :- !.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]) :-
    TfinS < Tin, !.  % Garantir que termina antes do próximo começar
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]) :-
    TinS > Tfin,     % Garantir que começa depois do anterior terminar
    insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(_,_,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).



obtain_better_sol(Room,Day,AgOpRoomBetter,LAgDoctorsBetter,TFinOp):-
		get_time(Ti),
		(obtain_better_sol1(Room,Day);true),
		retract(better_sol(Day,Room,AgOpRoomBetter,LAgDoctorsBetter,TFinOp)),
            write('Final Result: AgOpRoomBetter='),write(AgOpRoomBetter),nl,
            write('LAgDoctorsBetter='),write(LAgDoctorsBetter),nl,
            write('TFinOp='),write(TFinOp),nl,
		get_time(Tf),
		T is Tf-Ti,
		write('Tempo de geracao da solucao:'),write(T),nl.


obtain_better_sol1(Room,Day):-
    asserta(better_sol(Day,Room,_,_,1441)),
    findall(OpCode,surgery_id(OpCode,_),LOC),
    write('Found surgeries: '), write(LOC), nl,
    permutation(LOC,LOpCode),
    write('Trying permutation: '), write(LOpCode), nl,
    
    % Limpar e inicializar agendas
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    
    % Inicializar agendas do staff
    findall(_,(agenda_staff(D,Day,Agenda),
              assertz(agenda_staff1(D,Day,Agenda)),
              write('Initialized agenda for: '), write(D), nl),_),
    
    % Inicializar agenda da sala
    agenda_operation_room(Room,Day,Agenda),
    assert(agenda_operation_room1(Room,Day,Agenda)),
    write('Initial room agenda: '), write(Agenda), nl,
    
    % Calcular disponibilidades
    findall(_,(agenda_staff1(D,Day,L),
              free_agenda0(L,LFA),
              adapt_timetable(D,Day,LFA,LFA2),
              assertz(availability(D,Day,LFA2)),
              write('Availability for '), write(D), write(': '), write(LFA2), nl),_),
    
    write('Attempting to schedule surgeries...'), nl,
    
    % Tentar agendar cirurgias
    (availability_all_surgeries(LOpCode,Room,Day) ->
        write('Successfully scheduled all surgeries'), nl,
        agenda_operation_room1(Room,Day,AgendaR),
        write('Final room agenda: '), write(AgendaR), nl,
        update_better_sol(Day,Room,AgendaR,LOpCode),
        write('Solution updated'), nl
    ;
        write('Failed to schedule surgeries'), nl
    ),
    fail.

% Atualizar update_better_sol para considerar todo o staff
update_better_sol(Day,Room,Agenda,LOpCode):-
    better_sol(Day,Room,_,_,FinTime),
    reverse(Agenda,AgendaR),
    evaluate_final_time(AgendaR,LOpCode,FinTime1),
    write('Analysing for LOpCode='),write(LOpCode),nl,
    write('now: FinTime1='),write(FinTime1),write(' Agenda='),write(Agenda),nl,
    FinTime1<FinTime,
    write('best solution updated'),nl,
    retract(better_sol(_,_,_,_,_)),
    findall(S, staff(S,_,_,_), LStaff1), 
    remove_equals(LStaff1,LStaff),
    list_staff_agenda(Day,LStaff,LAgendas),
    asserta(better_sol(Day,Room,Agenda,LAgendas,FinTime1)).

% Novo predicado para listar agenda de todo o staff
list_staff_agenda(_,[],[]).
list_staff_agenda(Day,[S|LS],[(S,AgS)|LAgS]) :-
    agenda_staff1(S,Day,AgS),
    list_staff_agenda(Day,LS,LAgS).

evaluate_final_time([],_,1441).
evaluate_final_time([(_,Tfin,OpCode)|_],LOpCode,Tfin):-member(OpCode,LOpCode),!.
evaluate_final_time([_|AgR],LOpCode,Tfin):-evaluate_final_time(AgR,LOpCode,Tfin).

list_doctors_agenda(_,[],[]).
list_doctors_agenda(Day,[D|LD],[(D,AgD)|LAgD]):-agenda_staff1(D,Day,AgD),list_doctors_agenda(Day,LD,LAgD).

remove_equals([],[]).
remove_equals([X|L],L1):-member(X,L),!,remove_equals(L,L1).
remove_equals([X|L],[X|L1]):-remove_equals(L,L1).

test_schedule :-
    Room = or1,
    Day = 20241028,
    write('Starting scheduling for Room: '), write(Room), nl,
    write('Date: '), write(Day), nl,
    obtain_better_sol(Room, Day, AgOpRoomBetter, LAgDoctorsBetter, TFinOp),
    write('Best Schedule Found:'), nl,
    write('Room Agenda: '), write(AgOpRoomBetter), nl,
    write('Doctors Agendas: '), write(LAgDoctorsBetter), nl,
    write('Final Time of Last Surgery: '), write(TFinOp), nl.


test_data :-
    findall(OpCode, surgery_id(OpCode,_), Surgeries),
    write('Surgeries: '), write(Surgeries), nl,
    findall(Staff, staff(Staff,_,_,_), AllStaff),
    write('Staff: '), write(AllStaff), nl.

test_availability(Day) :-
    findall(Staff, staff(Staff,_,_,_), AllStaff),
    member(S, AllStaff),
    availability(S, Day, Avail),
    write(S), write(': '), write(Avail), nl,
    fail.

% Verificar disponibilidade da sala considerando todas as cirurgias agendadas
check_room_availability(Room, Day, StartTime, EndTime) :-
    agenda_operation_room1(Room, Day, Agenda),
    \+ (member((Start, End, _), Agenda),
        % Verifica se há sobreposição
        \+ (EndTime < Start ; StartTime > End)).

% Novo predicado para verificar disponibilidade de todas as fases
check_all_phases_availability(Day, StartTime, Phases) :-
    check_phases_sequence(Day, StartTime, Phases, 0).

check_phases_sequence(_, _, [], _).
check_phases_sequence(Day, StartTime, [(Phase, Time, Roles)|Rest], Offset) :-
    PhaseStart is StartTime + Offset,
    PhaseEnd is PhaseStart + Time - 1,
    check_phase_staff(Day, Phase, PhaseStart, PhaseEnd, Roles),
    NextOffset is Offset + Time,
    check_phases_sequence(Day, StartTime, Rest, NextOffset).

test_single_surgery_scheduling(OpCode, Room, Day) :-
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    
    % Inicializar agendas
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),
    assert(agenda_operation_room1(Room,Day,Agenda)),
    
    % Inicializar disponibilidade
    findall(_,(agenda_staff1(D,Day,L),
              free_agenda0(L,LFA),
              adapt_timetable(D,Day,LFA,LFA2),
              assertz(availability(D,Day,LFA2))),_),
    
    % Tentar agendar uma cirurgia
    availability_operation(OpCode, Room, Day, LPossibilities, LStaffByPhase),
    write('Final possibilities: '), write(LPossibilities), nl,
    write('Final staff by phase: '), write(LStaffByPhase), nl.

% Modificar check_phase_staff para ser mais flexível
check_phase_staff(Day, Phase, Start, End, Roles) :-
    write('Checking staff for phase: '), write(Phase), nl,
    % Para cada role necessário
    forall(member(Role, Roles), (
        % Encontrar pelo menos um membro do staff disponível para este role
        staff(Member, _, Role, _),
        availability(Member, Day, Avail),
        member((AvStart, AvEnd), Avail),
        Start >= AvStart,
        End =< AvEnd
    )).

% Adicionar predicado para verificar disponibilidade contínua
check_continuous_availability(Day, StartTime, TotalTime, Staff) :-
    EndTime is StartTime + TotalTime - 1,
    availability(Staff, Day, Avail),
    member((AvStart, AvEnd), Avail),
    StartTime >= AvStart,
    EndTime =< AvEnd.

test_single_surgery(OpCode) :-
    Room = or1,
    Day = 20241028,
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    
    % Inicializar agendas e disponibilidades
    findall(_,(agenda_staff(D,Day,Agenda),
              assertz(agenda_staff1(D,Day,Agenda)),
              write('Initialized agenda for: '), write(D), nl),_),
    
    agenda_operation_room(Room,Day,Agenda),
    assert(agenda_operation_room1(Room,Day,Agenda)),
    write('Initial room agenda: '), write(Agenda), nl,
    
    findall(_,(agenda_staff1(D,Day,L),
              free_agenda0(L,LFA),
              adapt_timetable(D,Day,LFA,LFA2),
              assertz(availability(D,Day,LFA2)),
              write('Availability for '), write(D), write(': '), write(LFA2), nl),_),
    
    % Usar as variáveis LPossibilities e LStaffByPhase no output
    availability_operation(OpCode,Room,Day,LPossibilities,LStaffByPhase),
    write('Found possibilities: '), write(LPossibilities), nl,
    write('Staff by phase: '), write(LStaffByPhase), nl.

  