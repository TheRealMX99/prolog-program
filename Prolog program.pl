assign_proctors(AllTAs, Quizzes, TeachingSchedule, ProctoringSchedule):-
	 free_schedule(AllTAs, TeachingSchedule, FreeSchedule),
	 assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).
daySlots([],_,_,[]).
daySlots([ta(Name,Day_Off)|Rest],DayName,Slot,[X|Free_Slots]):-
    (Day_Off \= DayName,\+member(Name,Slot)),
	X=Name,
	daySlots(Rest,DayName,Slot,Free_Slots).
daySlots([_|Rest],DayName,Slot,Free_Slots):-
	daySlots(Rest,DayName,Slot,Free_Slots).
daySchedule(_,_,[],[]).
daySchedule(TAs,DayName,[Slot|RestSlots],[FinalFreeSlot|RestOfDay]):-
	daySlots(TAs,DayName,Slot,FreeSlot1),
	FinalFreeSlot=FreeSlot1,
	daySchedule(TAs,DayName,RestSlots,RestOfDay).
free_schedule(_,[],[]).
free_schedule(TAs,[day(Dn,X)|RestOfDays],[FreeSlots|RestFreeSchedule]):-
	daySchedule(TAs,Dn,X,FinalFreeSlot),
	FreeSlots=day(Dn,FinalFreeSlot),
	free_schedule(TAs,RestOfDays,RestFreeSchedule).

assign_quizzes([],_, []).
assign_quizzes([Quiz|Quizzes], FreeSchedule, [Proctoring|ProctoringSchedule]) :-
    assign_quiz(Quiz, FreeSchedule, G),
    X=proctors(Quiz,G),
    Proctoring=X,
    assign_quizzes(Quizzes, FreeSchedule, ProctoringSchedule).

    take(0, _, []).
    take(_, [], []).
    take(N, [H|T], [H|R]) :-
        N > 0,
        N1 is N - 1,
        take(N1, T, R).
    avail_TAs([H|_],0,R):-
        R=H.
    avail_TAs([_|T],L,R):-
        V is L - 1,
        avail_TAs(T,V,R).
    same_day([H|_], Day, G) :-
        H=day(Day,_),
        G=H.
    same_day([_|T], Day, G) :-
        same_day(T, Day, G).
    num_TAs(T,L):-
        length(T,L).
    assign_quiz(quiz(_,Day,Slot,Count),L1,L3):-
        same_day(L1,Day,L),
        L=day(_,X),
        B is Slot - 1,
        avail_TAs(X,B,G),
        length(G,V),
        V>=Count,
    	permutation(G,F),
        take(Count,F,L3).