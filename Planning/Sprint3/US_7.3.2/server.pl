% Bibliotecas
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).

% Carregar lÃ³gica de agendamento
:- consult('genetic_algorithm.pl').

% Define os handlers
:- http_handler('/', reply_root, []).
:- http_handler('/agendaStaff', handle_agenda_staff, [method(post)]).
:- http_handler('/timetable', handle_timetable, [method(post)]).
:- http_handler('/staff', handle_staff, [method(post)]).
:- http_handler('/surgery', handle_surgery, [method(post)]).
:- http_handler('/surgeryId', handle_surgery_id, [method(post)]).
:- http_handler('/agendaOperationRoom', handle_agenda_operation_room, [method(post)]).
:- http_handler('/best', handle_best_schedule, [method(post)]).

% Responde ao request raiz
reply_root(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Bem-vindo ao servidor SWI-Prolog!~n').

% Inicia o servidor
:- initialization(start_server).

start_server :-
    Port = 6605,
    http_server(http_dispatch, [port(Port)]),
    format('Servidor HTTP iniciado na porta ~w~n', [Port]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handlers para processar JSON via POST

% Handler para agendaStaff
handle_agenda_staff(Request) :-
    http_read_json_dict(Request, Data),
    StaffID = Data.staffID,
    Day = Data.day,
    SlotList = Data.slots,

    (   agenda_staff(StaffID, Day, _) ->
        retract(agenda_staff(StaffID, Day, _)),
        assertz(agenda_staff(StaffID, Day, SlotList)),
        reply_json_dict(_{status: "updated", staffID: StaffID, day: Day})
    ;   assertz(agenda_staff(StaffID, Day, SlotList)),
        reply_json_dict(_{status: "created", staffID: StaffID, day: Day})
    ).

% Handler para timetable
handle_timetable(Request) :-
    http_read_json_dict(Request, Data),
    StaffID = Data.staffID,
    Day = Data.day,
    StartTime = Data.start_time,
    EndTime = Data.end_time,

    assertz(timetable(StaffID, Day, (StartTime, EndTime))),
    reply_json_dict(_{status: "created", staffID: StaffID, day: Day, start_time: StartTime, end_time: EndTime}).

% Handler para staff
handle_staff(Request) :-
    http_read_json_dict(Request, Data),
    StaffID = Data.staffID,
    Role = Data.role,
    Speciality = Data.specialization,
    OperationTypes = Data.oTs,

    (   staff(StaffID, _, _, _) ->
        retract(staff(StaffID, _, _, _)),
        assertz(staff(StaffID, Role, Speciality, OperationTypes))
    ;   assertz(staff(StaffID, Role, Speciality, OperationTypes))
    ),
    reply_json_dict(_{status: "updated", staffID: StaffID, role: Role, specialization: Speciality, operationTypes: OperationTypes}).

% Handler para surgery
handle_surgery(Request) :-
    http_read_json_dict(Request, Data),
    SurgeryID = Data.surgeryID,
    T1 = Data.t1,
    T2 = Data.t2,
    T3 = Data.t3,

    (   surgery(SurgeryID, _, _, _) ->
        retract(surgery(SurgeryID, _, _, _)),
        assertz(surgery(SurgeryID, T1, T2, T3))
    ;   assertz(surgery(SurgeryID, T1, T2, T3))
    ),
    reply_json_dict(_{status: "updated", surgeryID: SurgeryID, t1: T1, t2: T2, t3: T3}).

% Handler para surgeryId
handle_surgery_id(Request) :-
    http_read_json_dict(Request, Data),
    OperationRequestID = Data.oRID,
    SurgeryID = Data.surgeryID,

    assertz(surgery_id(OperationRequestID, SurgeryID)),
    reply_json_dict(_{status: "created", oRID: OperationRequestID, surgeryID: SurgeryID}).

% Handler para agendaOperationRoom
handle_agenda_operation_room(Request) :-
    http_read_json_dict(Request, Data),
    Room = Data.room,
    Day = Data.day,
    Slots = Data.slots,

    (   agenda_operation_room(Room, Day, ExistingSlots) ->
        append(ExistingSlots, Slots, UpdatedSlots),
        retract(agenda_operation_room(Room, Day, ExistingSlots)),
        assertz(agenda_operation_room(Room, Day, UpdatedSlots)),
        reply_json_dict(_{status: "updated", room: Room, day: Day, slots: UpdatedSlots})
    ;   assertz(agenda_operation_room(Room, Day, Slots)),
        reply_json_dict(_{status: "created", room: Room, day: Day, slots: Slots})
    ).

% Handler para obter melhor agendamento
handle_best_schedule(Request) :-
    http_read_json_dict(Request, Data),
    Room = Data.room,
    Day = Data.day,

    ( obtain_better_sol(Room, Day, X, Y, Z) ->
        reply_json_dict(_{status: "success", room: Room, day: Day, schedule: _{X: X, Y: Y, Z: Z}})
    ;   reply_json_dict(_{status: "failure", room: Room, day: Day, message: "Failed to find a schedule"})
    ).