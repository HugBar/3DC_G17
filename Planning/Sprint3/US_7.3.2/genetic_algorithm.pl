:- dynamic availability/3.
:- dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room/3.
:-dynamic agenda_operation_room1/3.
:-dynamic better_sol/5.

:- dynamic timetable/3.
:- dynamic agenda_staff/3.
:- dynamic staff/4.
:- dynamic surgery/4.
:- dynamic surgery_id/2.
:- dynamic surgery_requirements/2.

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

agenda_operation_room(or1,20241028,[(520,579,so100000), (1000,1059,so099999)]).
