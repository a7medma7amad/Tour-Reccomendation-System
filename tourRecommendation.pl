% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion
% mean wrt customer X with relevance

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).
%project
subset([],[]).
subset([X|R],[X|R1]) :-
  subset(R,R1).
subset(L, [_|R]) :-
  subset(L,R).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).
perm([H|T],L) :- perm(T,P), insert(H,P,L).
perm([],[]).
possibleSubset([],[]).
possibleSubset(L,R):-subset(R1,L),perm(R1,R).

choosePreferences(L,ChoosenPreferences):-
	((checkActivityExist(L,[activity(A)]),A\=[],possibleSubset(A1,A));(checkActivityExist(L,[activity(A)]),A==[])),
    delete(L,activity(A),X1),
	possibleSubset(A,R1),
	append([activity(R1)],X1,X2),
	possibleSubset(X2,ChoosenPreferences),R1\=[].
preferedActivity(_,[],0).
preferedActivity(Customer,[H|T],R):-
    customerPreferredActivity(Customer,H,R1),
    preferedActivity(Customer,T,R2),
    R is R1+R2.
preferenceSatisfaction(offer(X1,A1,C1,V1,T1,period(P1,P11),D1,N1),Customer,ChosenPrefs,S):-
        offerMean(offer(X1,A1,C1,V1,T1,period(P1,P11),D1,N1),Mean1),
        offerAccommodation(offer(X1,A1,C1,V1,T1,period(P1,P11),D1,N1),Acc1),
		((checkMeansExist(ChosenPrefs,[means(M)]),M\=[],customerPreferredMean(Customer,M,R));(checkMeansExist(ChosenPrefs,[means(M)]),M==[],R is 0)),
        ((checkAccExist(ChosenPrefs,[accommodation(Acc)]),Acc\=[],customerPreferredAccommodation(Customer,Acc,X));(checkAccExist(ChosenPrefs,[accommodation(Acc)]),Acc==[],X is 0)),
        ((checkActivityExist(ChosenPrefs,[activity(A)]),A\=[],preferedActivity(Customer,A,Y));(checkActivityExist(ChosenPrefs,[activity(A)]),A==[]),Y is 0),
         S is R+X+Y.
compareDate(A-B-C,X-Y-Z):-
	(A=<X);(A=X ,B=<Y);(A=X,B=Y,C=<Z).
overlapPeriod(period(A1,B1),period(A2,B2)):-
	(compareDate(A2,B1),compareDate(A1,A2));
	(compareDate(B2,B1),compareDate(A1,B2));
	(compareDate(A1,B2),compareDate(A2,A1));
	(compareDate(A2,B1),compareDate(B1,B2)).
%%%%%% preferences check
%%%%dest

checkDestExist([],[dest(D)]):-
	D=[].
checkDestExist([H|T],[dest(D)]):-
		H\=dest(_),
		checkDestExist(T,[dest(D)]).
checkDestExist([H|T],[dest(D)]):-
	H=dest(D).
%%%%budget
checkBudgetExist([],[budget(D)]):-
	D=[].
checkBudgetExist([H|T],[budget(B)]):-
		H\=budget(_),
		checkBudgetExist(T,[budget(B)]).
checkBudgetExist([H|T],[budget(B)]):-
	H=budget(B).
%%%%means
checkMeansExist([],[means(D)]):-
	D=[].

checkMeansExist([H|T],[means(M)]):-
	H=means(M).
checkMeansExist([H|T],[means(M)]):-
		H\=means(_),
		checkMeansExist(T,[means(M)]).


%%%%accommodation
checkAccExist([],[accommodation(D)]):-
	D=[].
checkAccExist([H|T],[accommodation(Acc)]):-
		H\=accommodation(_),
		checkAccExist(T,[accommodation(Acc)]).
checkAccExist([H|T],[accommodation(Acc)]):-
	H=accommodation(Acc).
%%%%activity
checkActivityExist([],[activity(D)]):-
	D=[].
checkActivityExist([H|T],[activity(A)]):-
		H\=activity(_),
		checkActivityExist(T,[activity(A)]).
checkActivityExist([H|T],[activity(A)]):-
	H=activity(A).
%%%%period
checkDestExist([],[period(F,To)]):-
	F=[],
	To=[].
checkPeriodExist([H|T],[period(F,To)]):-
		H\=period(_,_),
		checkPeriodExist(T,[period(F,To)]).
checkPeriodExist([H|T],[period(F,To)]):-
	H=period(F,To).
%%%%%%%%%%%%%%%%%%%%%	
generateListPrefs(ChosenPrefs,List):-
	checkDestExist(ChosenPrefs,[dest(D)]),
	checkAccExist(ChosenPrefs,[accommodation(Acc)]),
	checkActivityExist(ChosenPrefs,[activity(A)]),
	checkBudgetExist(ChosenPrefs,[budget(B)]),
	checkMeansExist(ChosenPrefs,[means(M)]),
	checkPeriodExist(ChosenPrefs,[period(F,To)]),
	((append([D],[],Tmp),D\=[]);(append(D,[],Tmp),D==[])),
	((append([Acc],Tmp,Tmp1),Acc\=[]);(append(Acc,Tmp,Tmp1),Acc==[])),
	((append(Tmp1,[B],Tmp2),B\=[]);(append(Tmp1,B,Tmp2),B==[])),
	((append(Tmp2,[M],List),M\=[]);(List=Tmp2,M==[])).

getOffer(ChosenPrefs,offer(X1,A1,C1,V1,T1,period(P1,P11),D1,N1)):-
	(offerAccommodation(offer(X1,A1,C1,V1,T1,period(P1,P11),D1,N1),Acc1);
	 offerMean(offer(X1,A1,C1,V1,T1,period(P1,P11),D1,N1),Mean1)),
	((checkActivityExist(ChosenPrefs,[activity(A)]),A\=[],possibleSubset(A1,A));(checkActivityExist(ChosenPrefs,[activity(A)]),A==[])),
	((checkBudgetExist(ChosenPrefs,[budget(B)]),B\=[],B>=C1);(checkBudgetExist(ChosenPrefs,[budget(B)]),B==[])),
	((checkDestExist(ChosenPrefs,[dest(D)]),D\=[],D==X1);(checkDestExist(ChosenPrefs,[dest(D)]),D==[])),
	((checkAccExist(ChosenPrefs,[accommodation(Acc)]),Acc\=[],Acc==Acc1);(checkAccExist(ChosenPrefs,[accommodation(Acc)]),Acc==[])),
	((checkMeansExist(ChosenPrefs,[means(M)]),M\=[],M==Mean1);(checkMeansExist(ChosenPrefs,[means(M)]),M==[])),
	((checkPeriodExist(ChosenPrefs,[period(F,To)]),overlapPeriod(period(F,To),period(P1,P11)));(checkPeriodExist(ChosenPrefs,[period(F,To)]),F==[],To==[])).
	
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
		choosePreferences(Prefs,ChosenPrefs),
		getOffer(ChosenPrefs,O).

custPreferenceSatisfaction([],Offer,[],[]).
custPreferenceSatisfaction([H1|T1],Offer,[H2|T2],Acc):-
	recommendOfferForCustomer(H2,C,Offer),
	preferenceSatisfaction(Offer,H1,C,S),
	append(Acc,[S],Acc1),
	CustPreferenceSatisfaction(T1,Offer,T2,Acc1).

		
	
	
		


	
		
