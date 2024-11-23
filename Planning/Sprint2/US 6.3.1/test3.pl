:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.


agenda_staff(d001,20241028,[(720,790,m01),(1080,1140,c01)]).
agenda_staff(d002,20241028,[(850,900,m02)]).
agenda_staff(d003,20241028,[(720,790,m01)]).
agenda_staff(d004,20241028,[(720,790,m01)]).
agenda_staff(n004,20241028,[(720,790,m01)]).
agenda_staff(m001,20241028,[(720,790,d001)]).
agenda_staff(a001,20241028,[(850,900,d002)]).
agenda_staff(n001,20241028,[(850,900,d002)]).
agenda_staff(n002,20241028,[(850,900,d002)]).
agenda_staff(n003,20241028,[(850,900,d002)]).



timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(500,1440)).
timetable(d003,20241028,(520,1320)).
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
    [doctor-orthopaedist-3, doctor-anaesthetist-1, nurse-anaesthetist-1, circulating_nurse-1],
    % Phase 3 - Cleaning
    [medical-assistant-1]
]).

surgery_requirements(so4, [
    % Phase 1 - Anesthesia/preparation
    [doctor-anaesthetist-1, nurse-anaesthetist-1],
    % Phase 2 - Surgery
    [doctor-orthopaedist-3, doctor-anaesthetist-1, nurse-anaesthetist-1, circulating_nurse-1],
    % Phase 3 - Cleaning
    [medical-assistant-1]
]).



% staff(StaffID, Role, Speciality, Operations)
staff(d001, doctor, orthopaedist, [so2,so3,so4]).
staff(d002, doctor, orthopaedist, [so2,so3,so4]).
staff(d003, doctor, orthopaedist, [so2,so3,so4]).
staff(d004, doctor, anaesthetist, [so2,so3,so4]).
staff(a001, nurse, anaesthetist, [so2,so3,so4]).
staff(n001, nurse, instrumentation, [so2,so3,so4]).
staff(n002, nurse, circulating, [so2,so3,so4]).
staff(n003, nurse, anaesthetist, [so2,so3,so4]).
staff(m001, medical, assistant, [so2,so3,so4]).

% surgery(SurgeryType, TPrep, TSurgery, TCleaning)
surgery(so2, 45, 60, 45).
surgery(so3, 20, 45, 30).
surgery(so4, 45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so4).

assignment_surgery(so100001,d001).
assignment_surgery(so100002,d002).
assignment_surgery(so100003,d003).
assignment_surgery(so100003,n004).
assignment_surgery(so100004,d001).
assignment_surgery(so100004,d002).
assignment_surgery(so100005,d002).
assignment_surgery(so100005,d003).

agenda_operation_room(or1,20241028,[(520,579,so100000)]).

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

get_required_staff(OpCode, Phase, RequiredStaff) :-
    surgery_id(OpCode, SurgeryType),
    surgery_requirements(SurgeryType, AllPhases),
    nth1(Phase, AllPhases, PhaseRequirements),
    get_staff_for_requirements(PhaseRequirements, RequiredStaff, []).

get_staff_for_requirements([], [], _).
get_staff_for_requirements([Role-Speciality-Number|Rest], StaffList, AlreadySelected) :-
    findall(StaffID, (
        staff(StaffID, Role, Speciality, _),
        \+ member(StaffID, AlreadySelected)  % Exclui os já selecionados
    ), AvailableStaff),
    take_n(Number, AvailableStaff, Selected),  % Seleciona o número necessário
    append(Selected, AlreadySelected, UpdatedSelected),  % Atualiza os selecionados
    get_staff_for_requirements(Rest, RestStaff, UpdatedSelected),  % Processa o restante
    append(Selected, RestStaff, StaffList).  % Concatena os resultados

take_n(0, _, []).
take_n(N, List, [Elem|RestSelected]) :-
    N > 0,
    select(Elem, List, Remaining),
    N1 is N - 1,
    take_n(N1, Remaining, RestSelected).

availability_operation(OpCode, Room, Day, LPossibilities, LAllStaff) :-
    % Adicionar prints para debug
    write('Starting availability_operation for OpCode: '), write(OpCode), nl,
    
    surgery_id(OpCode, SurgeryType),
    write('Surgery Type: '), write(SurgeryType), nl,
    
    surgery(SurgeryType, TPrep, TSurgery, TCleaning),
    write('Times - Prep: '), write(TPrep), 
    write(' Surgery: '), write(TSurgery),
    write(' Cleaning: '), write(TCleaning), nl,
    
    surgery_requirements(SurgeryType, AllPhases),
    write('Surgery Requirements: '), write(AllPhases), nl,
    
    % Obtém staff para cada fase
    get_all_required_staff(AllPhases, LAllStaff),
    write('All required staff: '), write(LAllStaff), nl,
    
    % Verifica disponibilidade para todos os staff necessários
    findall(Staff, (
        member(Staff, LAllStaff),
        availability(Staff, Day, _)
    ), AvailableStaff),
    write('Available staff: '), write(AvailableStaff), nl,
    
    % Intersecta disponibilidades
    intersect_all_agendas(AvailableStaff, Day, LA),
    write('Intersected availabilities: '), write(LA), nl,
    
    % Verifica disponibilidade da sala
    agenda_operation_room1(Room, Day, LAgenda),
    write('Room agenda: '), write(LAgenda), nl,
    
    free_agenda0(LAgenda, LFAgRoom),
    write('Free room slots: '), write(LFAgRoom), nl,
    
    % Intersecta disponibilidades da sala e staff
    intersect_2_agendas(LA, LFAgRoom, LIntAgStaffRoom),
    write('Final intersected availabilities: '), write(LIntAgStaffRoom), nl,
    
    % Calcula tempo total necessário
    TTotal is TPrep + TSurgery + TCleaning,
    write('Total time needed: '), write(TTotal), nl,
    
    remove_unf_intervals(TTotal, LIntAgStaffRoom, LPossibilities),
    write('Final possibilities: '), write(LPossibilities), nl.

% Novo predicado para obter todo o staff necessário de todas as fases
get_all_required_staff(AllPhases, LAllStaff) :-
    flatten_requirements(AllPhases, FlatRequirements),
    findall(Staff, (
        member(Role-Specialty-_, FlatRequirements),
        staff(Staff, Role, Specialty, _)
    ), StaffList),
    sort(StaffList, LAllStaff).

% Auxiliar para achatar a lista de requisitos
flatten_requirements([], []).
flatten_requirements([Phase|Rest], Flat) :-
    flatten_requirements(Rest, RestFlat),
    append(Phase, RestFlat, Flat).

intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

initialize_availabilities(Day) :-
    retractall(availability(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    % Inicializar agenda da sala explicitamente
    agenda_operation_room(or1, Day, RoomAgenda),
    assertz(agenda_operation_room1(or1, Day, RoomAgenda)),
    write('Initialized room agenda: '), write(RoomAgenda), nl,
    
    % Resto do código para staff availabilities
    findall(_,(agenda_staff(D,Day,Agenda),
               free_agenda0(Agenda,LFA),
               adapt_timetable(D,Day,LFA,LFA2),
               assertz(availability(D,Day,LFA2))),_).

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

availability_all_surgeries([],_,_).
availability_all_surgeries([OpCode|LOpCode],Room,Day):-
    % Primeiro verifica se a cirurgia já está agendada
    (agenda_operation_room1(Room,Day,CurrentAgenda),
     member((_,_,OpCode), CurrentAgenda)) ->
        % Se já está agendada, passa para a próxima
        write('Surgery '), write(OpCode), write(' already scheduled, skipping...'), nl,
        availability_all_surgeries(LOpCode,Room,Day)
    ;
    surgery_id(OpCode,OpType),
    surgery(OpType,TPrep,TSurgery,TCleaning),
    TTotal is TPrep + TSurgery + TCleaning,
    write('Availability for surgery: '), write(OpCode), nl,
    
    availability_operation(OpCode,Room,Day,LPossibilities,StaffAvailable),  % Renomeado para StaffAvailable
    write('Staff available for surgery: '), write(StaffAvailable), nl,
    
    % Se encontrou possibilidades, agenda a primeira
    (LPossibilities = [] -> 
        write('No possibilities found for surgery: '), write(OpCode), nl,
        availability_all_surgeries(LOpCode,Room,Day)
    ;
        % Agenda usando o primeiro intervalo disponível
        schedule_first_interval(TTotal,LPossibilities,(TinS,TfinS)),
        
        % Divide o tempo total nas três fases
        TPreparationEnd is TinS + TPrep - 1,
        TSurgeryStart is TPreparationEnd + 1,
        TSurgeryEnd is TSurgeryStart + TSurgery - 1,
        TCleaningStart is TSurgeryEnd + 1,
        TCleaningEnd is TfinS,
        
        % Atualiza agenda da sala
        retract(agenda_operation_room1(Room,Day,Agenda)),
        insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
        assertz(agenda_operation_room1(Room,Day,Agenda1)),
        
        % Obtém staff necessário para cada fase usando o staff disponível
        surgery_requirements(OpType, [Phase1Staff, Phase2Staff, Phase3Staff]),
        
        % Filtra o staff disponível para cada fase
        filter_available_staff(Phase1Staff, StaffAvailable, Phase1Members),
        filter_available_staff(Phase2Staff, StaffAvailable, Phase2Members),
        filter_available_staff(Phase3Staff, StaffAvailable, Phase3Members),
        
        % Insere na agenda do staff para cada fase
        insert_agenda_staff((TinS,TPreparationEnd,OpCode),Day,Phase1Members),
        insert_agenda_staff((TSurgeryStart,TSurgeryEnd,OpCode),Day,Phase2Members),
        insert_agenda_staff((TCleaningStart,TCleaningEnd,OpCode),Day,Phase3Members),
        
        write('Scheduled surgery '), write(OpCode), 
        write(' from '), write(TinS), 
        write(' to '), write(TfinS), nl,
        write('Staff Phase 1: '), write(Phase1Members), nl,
        write('Staff Phase 2: '), write(Phase2Members), nl,
        write('Staff Phase 3: '), write(Phase3Members), nl
    ),
    % Continua com as próximas cirurgias
    availability_all_surgeries(LOpCode,Room,Day).

% Novo predicado para filtrar staff disponível por fase
filter_available_staff([], _, []).
filter_available_staff([Role-Specialty-Number|Rest], Available, Selected) :-
    findall(Staff, (
        member(Staff, Available),
        staff(Staff, Role, Specialty, _)
    ), StaffOptions),
    take_n(Number, StaffOptions, ThisRoleSelected),
    filter_available_staff(Rest, Available, RestSelected),
    append(ThisRoleSelected, RestSelected, Selected).

schedule_all_surgeries(Room,Day):-
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    
    % Inicializar as agendas e availabilities
    findall(_,(agenda_staff(D,Day,Agenda),assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),
    assert(agenda_operation_room1(Room,Day,Agenda)),
    
    % Inicializar disponibilidades
    initialize_availabilities(Day),
    
    % Obter lista de cirurgias e tentar agendar
    findall(OpCode,surgery_id(OpCode,_),LOC),
    write('Attempting to schedule surgeries: '), write(LOC), nl,
    availability_all_surgeries(LOC,Room,Day).

schedule_first_interval(TSurgery,[(Tin,_)|_],(Tin,TfinS)):-
    TfinS is Tin + TSurgery - 1.

insert_agenda((TinS,TfinS,OpCode),[],[(TinS,TfinS,OpCode)]).
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(TinS,TfinS,OpCode),(Tin,Tfin,OpCode1)|LA]):-TfinS<Tin,!.
insert_agenda((TinS,TfinS,OpCode),[(Tin,Tfin,OpCode1)|LA],[(Tin,Tfin,OpCode1)|LA1]):-insert_agenda((TinS,TfinS,OpCode),LA,LA1).

insert_agenda_doctors(_,_,[]).
insert_agenda_doctors((TinS,TfinS,OpCode),Day,[Doctor|LDoctors]):-
    retract(agenda_staff1(Doctor,Day,Agenda)),
    insert_agenda((TinS,TfinS,OpCode),Agenda,Agenda1),
    assert(agenda_staff1(Doctor,Day,Agenda1)),
    insert_agenda_doctors((TinS,TfinS,OpCode),Day,LDoctors).

insert_agenda_staff(_,_,[]).  % Caso base: lista vazia de staff
insert_agenda_staff((TinS,TfinS,OpCode),Day,[Staff|RestStaff]):-
    % Verifica se existe agenda para este staff
    (agenda_staff1(Staff,Day,CurrentAgenda) ->
        % Se existe, retira a agenda atual
        retract(agenda_staff1(Staff,Day,CurrentAgenda))
    ;
        % Se não existe, considera agenda vazia
        CurrentAgenda = []
    ),
    
    % Insere o novo slot na agenda
    insert_agenda((TinS,TfinS,OpCode),CurrentAgenda,NewAgenda),
    
    % Guarda a nova agenda
    assertz(agenda_staff1(Staff,Day,NewAgenda)),
    
    % Debug output
    write('Updated agenda for '), write(Staff),
    write(' Day: '), write(Day),
    write(' New agenda: '), write(NewAgenda), nl,
    
    % Continua com o resto do staff
    insert_agenda_staff((TinS,TfinS,OpCode),Day,RestStaff).

obtain_better_sol(Room,Day,AgOpRoomBetter,LAllStaffAgendas,TFinOp):-
    get_time(Ti),
    
    asserta(better_sol(Day,Room,_,_,1441)),
    (obtain_better_sol1(Room,Day);true),
    retract(better_sol(Day,Room,AgOpRoomBetter,LAllStaffAgendas,TFinOp)),
    write('Final Result:'),nl,
    write('Room Agenda: '),write(AgOpRoomBetter),nl,
    write('All Staff Agendas: '),write(LAllStaffAgendas),nl,
    write('Final Operation Time: '),write(TFinOp),nl,
    get_time(Tf),
    T is Tf-Ti,
    write('Solution generation time: '),write(T),nl.

obtain_better_sol1(Room,Day):-
    % Obter todas as cirurgias
    findall(OpCode,surgery_id(OpCode,_),LOC),!,
    % Gerar todas as permutações possíveis
    permutation(LOC,LOpCode),
    
    % Limpar estado anterior
    retractall(agenda_staff1(_,_,_)),
    retractall(agenda_operation_room1(_,_,_)),
    retractall(availability(_,_,_)),
    
    % Inicializar agendas e disponibilidades
    findall(_,(agenda_staff(D,Day,Agenda),
               assertz(agenda_staff1(D,Day,Agenda))),_),
    agenda_operation_room(Room,Day,Agenda),
    assert(agenda_operation_room1(Room,Day,Agenda)),
    findall(_,(agenda_staff1(D,Day,L),
               free_agenda0(L,LFA),
               adapt_timetable(D,Day,LFA,LFA2),
               assertz(availability(D,Day,LFA2))),_),
    
    % Tentar agendar todas as cirurgias nesta ordem
    availability_all_surgeries(LOpCode,Room,Day),
    
    % Obter agenda final da sala
    agenda_operation_room1(Room,Day,AgendaR),
    
    % Atualizar se for melhor solução
    update_better_sol(Day,Room,AgendaR,LOpCode),
    
    fail.

update_better_sol(Day,Room,Agenda,LOpCode):-
    better_sol(Day,Room,_,_,FinTime),
    reverse(Agenda,AgendaR),
    evaluate_final_time(AgendaR,LOpCode,FinTime1),
    write('Analyzing sequence: '),write(LOpCode),nl,
    write('Current end time: '),write(FinTime1),
    write(' Agenda: '),write(Agenda),nl,
    
    % Se encontrou uma solução melhor
    FinTime1 < FinTime,
    write('Better solution found!'),nl,
    
    % Atualizar melhor solução
    retract(better_sol(_,_,_,_,_)),
    
    % Obter agendas de todo o staff envolvido
    findall(Staff, (
        surgery_requirements(_, AllPhases),
        member(Phase, AllPhases),
        member(Role-Specialty-_, Phase),
        staff(Staff, Role, Specialty, _)
    ), LStaff1),
    remove_equals(LStaff1,LStaff),
    list_staff_agendas(Day,LStaff,LStaffAgendas),
    
    asserta(better_sol(Day,Room,Agenda,LStaffAgendas,FinTime1)).

% Adaptado para lidar com todo tipo de staff
list_staff_agendas(_,[],[]).
list_staff_agendas(Day,[Staff|LStaff],[(Staff,Agenda)|LAgendas]):-
    agenda_staff1(Staff,Day,Agenda),
    list_staff_agendas(Day,LStaff,LAgendas).

evaluate_final_time([],_,1441).
evaluate_final_time([(_,Tfin,OpCode)|_],LOpCode,Tfin):-
    member(OpCode,LOpCode),!.
evaluate_final_time([_|AgR],LOpCode,Tfin):-
    evaluate_final_time(AgR,LOpCode,Tfin).

remove_equals([],[]).
remove_equals([X|L],L1):-member(X,L),!,remove_equals(L,L1).
remove_equals([X|L],[X|L1]):-remove_equals(L,L1).