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
