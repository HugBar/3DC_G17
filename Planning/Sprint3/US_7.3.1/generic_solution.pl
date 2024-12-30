:- dynamic generations/1.
:- dynamic population/1.
:- dynamic prob_crossover/1.
:- dynamic prob_mutation/1.
:- dynamic population_size/1.
:- dynamic fitness_history/1.
:- dynamic no_improvement_count/1.

room(r1,8,[xray,ventilator]).
room(r2,6,[ventilator]).
room(r3,10,[xray,ventilator,cardiac_monitor]).
room(r4,12,[ventilator,cardiac_monitor]).
room(r5,10,[ventilator,xray]).

surgery(s1,3,general,[ventilator]).
surgery(s2,5,orthopedic,[xray]).
surgery(s3,2,pediatric,[ventilator]).
surgery(s4,4,general,[ventilator,cardiac_monitor]).
surgery(s5,6,orthopedic,[xray,ventilator]).
surgery(s6,4,cardiac,[cardiac_monitor]).
surgery(s7,3,robotic,[ventilator]).
surgery(s8,6,orthopedic,[xray,ventilator]).
surgery(s9,4,cardiac,[cardiac_monitor]).
surgery(s10,3,robotic,[ventilator]).

initialize :-
    retractall(generations(_)), retractall(population(_)),
    retractall(prob_crossover(_)), retractall(prob_mutation(_)),
    retractall(fitness_history(_)), retractall(no_improvement_count(_)),

    % Just hardcode for testing multiple gens without user input:
    asserta(generations(10)),
    asserta(population(20)),
    asserta(prob_crossover(0.9)),
    asserta(prob_mutation(0.3)),

    asserta(no_improvement_count(0)).

generate :-
    initialize,
    generate_initial_population(Pop),
    write('Initial population: '), write(Pop), nl,
    evaluate_population(Pop, PopValue),
    write('Population values: '), write(PopValue), nl,
    order_population_desc(PopValue, PopOrd),
    extract_best_fitness(PopOrd, _BestF),
    generations(NG),
    generate_generation(0, NG, PopOrd).

generate_initial_population(Pop) :-
    population(PS),
    findall(S,surgery(S,_,_,_),Surgeries),
    generate_population(PS,Surgeries,Pop).

generate_population(0,_,[]):-!.
generate_population(N,Surgeries,[Ind|Rest]):-
    N>0,
    N1 is N-1,
    generate_population(N1,Surgeries,Rest),
    generate_individual(Surgeries,Ind),
    \+ member(Ind,Rest).

generate_individual(Surgeries, Assignment) :-
    random_permutation(Surgeries,Rand),
    findall(R,room(R,_,_),Rooms),
    assign_surgeries_relaxed(Rand,Rooms,[],Assignment).

assign_surgeries_relaxed([],_,A,A).
assign_surgeries_relaxed([S|Rest],Rooms,Cur,Fin):-
    random_member(R,Rooms),
    add_to_assignment(R,S,Cur,Next),
    assign_surgeries_relaxed(Rest,Rooms,Next,Fin).

add_to_assignment(R,S,[],[R-[S]]) :- !.
add_to_assignment(R,S,[R-L|Rest],[R-[S|L]|Rest]):-!.
add_to_assignment(R,S,[O|Rest],[O|NewRest]):-
    add_to_assignment(R,S,Rest,NewRest).

evaluate_population([],[]).
evaluate_population([I|R],[I*V|R1]):-
    evaluate_assignment(I,V),
    evaluate_population(R,R1).

evaluate_assignment(A,Val):-
    calculate_capacity_usage(A,Cap),
    calculate_equipment_usage(A,Equip),
    calculate_constraint_penalties(A,Pen),
    Val is 0.6*Cap+0.4*Equip - Pen.

calculate_capacity_usage(A,Score):-
    findall(RSc,(member(R-S,A),room(R,C,_),total_duration(S,U),(C>0->RSc is U/C;RSc=0)),L),
    (L=[]->Score=0; sum_list(L,Sum), length(L,Len), Score is (Sum/Len)*100).

calculate_equipment_usage(A,Score):-
    findall(EqSc,(member(R-S,A),room(R,_,Eq),calc_room_eq_score(S,Eq,EqSc)),L),
    (L=[]->Score=0; sum_list(L,Sum),length(L,Ln),Score is Sum/Ln).

calc_room_eq_score(Surg,RoomEq,Score):-
    findall(M,(member(Su,Surg),surgery(Su,_,_,Req),count_matching_equipment(Req,RoomEq,M)),Ms),
    (Ms=[]->Score=0; sum_list(Ms,Sum), length(Ms,L), Score is Sum/L).

count_matching_equipment(Rq,Av,Sc):-
    findall(1,(member(E,Rq),member(E,Av)),Mat),
    length(Mat,M),
    length(Rq,RR),
    (RR>0->Sc is M/RR;Sc=0).

calculate_constraint_penalties(As,Tot):-
    capacity_violation_penalty(As,CapP),
    equipment_violation_penalty(As,EQP),
    Tot is CapP+EQP.

capacity_violation_penalty(As,P):-
    findall(Ex,(member(R-S,As),room(R,C,_),total_duration(S,U),(U>C->Ex is (U-C)*2;Ex=0)),L),
    sum_list(L,P).

equipment_violation_penalty(As,P):-
    findall(Miss,(member(R-Su,As),room(R,_,EQ),member(SX,Su),surgery(SX,_,_,RQ),
    count_missing_equipment(RQ,EQ,C),C>0,Miss=C),L),
    sum_list(L,P).

count_missing_equipment(Rq,Av,C):-
    findall(1,(member(X,Rq),\+ member(X,Av)),M),
    length(M,C).

total_duration(Surg,Tot):-
    findall(D,(member(S,Surg),surgery(S,D,_,_)),Ds),
    sum_list(Ds,Tot).

assignment_to_list(Assign, List):-
    findall(S,(member(_-Surgeries,Assign),member(S,Surgeries)),List).

list_to_assignment(List,Assignment):-
    findall(R-[],room(R,_,_),EmptyRooms),
    distribute_surgeries(List,EmptyRooms,Assignment).

distribute_surgeries([],A,A).
distribute_surgeries([S|Rest],Current,Final):-
    random_member(R-Old,Current),
    select(R-Old,Current,R-[S|Old],Updated),
    distribute_surgeries(Rest,Updated,Final).

% no stagnation or improvement checks
update_fitness_history(_).

check_stagnation :- fail.

tournament_size(3).

select_parent(Pop,Parent):-
    tournament_size(T),
    length(Pop,Len),
    findall(X,(between(1,T,_),random_between(1,Len,I),nth1(I,Pop,IndFit),X=IndFit),Tour),
    max_fitness(Tour,Parent).

max_fitness([X],X):-!.
max_fitness([X*FX,Y*FY|R],B):- (FX>FY->max_fitness([X*FX|R],B);max_fitness([Y*FY|R],B)).

cross_assignments(I1,I2,O1,O2):-
    assignment_to_list(I1,L1),
    assignment_to_list(I2,L2),
    length(L1,L),
    (L<2->O1=I1,O2=I2;(
      random_between(1,L,CP),
      split_at(L1,CP,A1,B1),
      split_at(L2,CP,A2,B2),
      append(A1,B2,New1),
      append(A2,B1,New2),
      repair_offspring(New1,O1,_I1),
      repair_offspring(New2,O2,_I2)
    )).

split_at(L,N,Front,Back):-
    length(Front,N),append(Front,Back,L).

repair_offspring(List,Assignment,_):-
    findall(S,surgery(S,_,_,_),AllS),
    sort(List,Ls),
    sort(AllS,All),
    findall(S,(member(S,All),\+ member(S,List)),Missing),
    remove_excess(List,All,Inter),
    append(Inter,Missing,Final),
    list_to_assignment(Final,Assignment).

remove_excess([],_,[]).
remove_excess([X|Xs],All,[X|Ys]):-
    select(X,All,All2),!,
    remove_excess(Xs,All2,Ys).
remove_excess([_|Xs],All,Ys):-
    remove_excess(Xs,All,Ys).

mutate_assignment_relaxed(Assignment,MutatedAssignment):-
    assignment_to_list(Assignment,SurgeryList),
    length(SurgeryList,Len),
    (Len<2->MutatedAssignment=Assignment;(
        random_between(1,Len,I1),
        random_between(1,Len,I2),
        I1\=I2,
        nth1(I1,SurgeryList,S1,T1),
        nth1(I2,T1,S2,T2),
        nth1(I1,Mut,S2,T2),
        nth1(I2,Mut,S1,T2),
        list_to_assignment(Mut,MutatedAssignment)
    )).

validate_solution(_):-true.

calculate_generation_stats(Pop,Avg,Best,Div):-
    findall(F,member(_*F,Pop),Fits),
    average(Fits,Avg),
    max_list(Fits,Best),
    calculate_population_diversity(Pop,Div).

calculate_population_diversity(Pop,Div):-
    findall(D,(member(I1*_,Pop),member(I2*_,Pop),I1@<I2,calculate_solution_distance(I1,I2,D)),Dist),
    (Dist=[]->Div=0;average(Dist,Div)).

average(L,A):-sum_list(L,S),length(L,N),(N>0->A is S/N;A=0).

extract_best_fitness([_*F|_],F).

generate_generation(N,G,Pop):-
    (N<G->
       writeln('--------------------------------'),
       format('Generation ~w:~n',[N]),
       calculate_generation_stats(Pop, Avg, Best, Div),
       format('  Average Fitness: ~2f~n',[Avg]),
       format('  Best Fitness: ~2f~n',[Best]),
       format('  Population Diversity: ~2f~n',[Div]),
       population(PS),
       evolve_population(Pop,PS,NewPop),
       evaluate_population(NewPop,ValPop),
       order_population_desc(ValPop,OrderedPop),
       N1 is N+1,
       generate_generation(N1,G,OrderedPop)
    ;
       format('Final Population:~n~w~n',[Pop]),
       format('Genetic Algorithm Completed.~n',[])
    ).

evolve_population(Pop,Size,NewPop):-
    findall(I,member(I,Pop),All),
    evolve_loop(All,Size,[],NewPop).

evolve_loop(_,0,Acc,Acc):-!.
evolve_loop(All,N,Acc,Final):-
    N>1,
    N1 is N-2,
    select_parent(All,P1*F1),
    select_parent(All,P2*F2),
    P1\=P2,
    cross_assignments(P1,P2,O1,O2),
    mutate_assignment_relaxed(O1,M1),
    mutate_assignment_relaxed(O2,M2),
    evaluate_assignment(M1,V1),
    evaluate_assignment(M2,V2),
    append(Acc,[M1*V1,M2*V2],NewAcc),
    evolve_loop(All,N1,NewAcc,Final).
evolve_loop(All,1,Acc,Final):-
    select_parent(All,P1*_),
    mutate_assignment_relaxed(P1,M1),
    evaluate_assignment(M1,V1),
    append(Acc,[M1*V1],Final).

order_population_desc(P,Desc):-
    sort(2,@>=,P,Desc).

calculate_solution_distance(Sol1,Sol2,Distance):-
    solution_to_pairs(Sol1,P1),
    solution_to_pairs(Sol2,P2),
    count_differences(P1,P2,Distance).

solution_to_pairs(Sol,Pairs):-
    findall(S-R,(member(R-L,Sol),member(S,L)),Pairs).

count_differences(P1,P2,C):-
    findall(1,(member(S-R1,P1),member(S-R2,P2),R1\=R2),Diff),
    length(Diff,C).

test_genetic_assignment:-
    retractall(generations(_)),
    retractall(population(_)),
    retractall(prob_crossover(_)),
    retractall(prob_mutation(_)),
    asserta(generations(10)),
    asserta(population(20)),
    asserta(prob_crossover(0.9)),
    asserta(prob_mutation(0.3)),
    generate, !.
