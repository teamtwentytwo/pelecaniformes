%order   family  genus   species
/*pelecaniformes
        pelecanidae
                pelecanus (pelican)
                        erythrorhynchos (americanWhitePelican)
                        occidentalis (brownPelican)
        ardeidae
                botaurus (bittern)
                        lentiginosus (americanBittern)
                ixobrychus (bittern)
                        exilis (leastBittern)
                ardea (heron)
                        herodias (greatBlueHeron)
                        alba (greatEgret)
                egretta (heron, egret)
                        thula (snowyEgret)
                        caerulea (littleBlueHeron)
                        tricolor (tricoloredHeron)
                        rufescens (reddishEgret)
                bubulcus (egret)
                        ibis (cattleEgret)
                butorides (heron)
                        virescens (greenHeron)
                nycticorax (nightHeron)
                        nycticorax (blackCrownedNightHeron)
                nyctanassa (nightHeron)
                        violacea (yellowCrownedNightHeron)
        threskiornithdae
                eudocimus (ibis)
                        albus (whiteIbis)
                plegadis (ibis)
                        falcinellus (glossyIbis)
                        chihi (whiteFacedIbis)
                platalea (spoonbill)
                        ajaja (roseateSpoonbill)*/
%Order(Name)
order(pelecaniformes).
	%Family(Parent, Name)
	family(pelecaniformes, pelecanidae).
	family(pelecaniformes, ardeidae).
	family(pelecaniformes, threskiornithdae).
		%genus(Parent, Name, CommonName)
		genus(pelecanidae, pelecanus, pelican).
		genus(ardeidae, botaurus, bittern).
		genus(ardeidae, ixobrychus, bittern).
		genus(ardeidae, ardea, heron).
		genus(ardeidae, egretta, heron).
		genus(ardeidae, egretta, egret).
		genus(ardeidae, bubulcus, egret).
		genus(ardeidae, butorides, heron).
		genus(ardeidae, nycticorax, nightHeron).
		genus(ardeidae, nyctanassa, nightHeron).
		genus(threskiornithdae, eudocimus, ibis).
		genus(threskiornithdae, plegadis, ibis).
		genus(threskiornithdae, platalea, spoonbill).
			%species(Parent, Name, Common Name, habitat, food, nesting, behavior, conservation)
			species(pelecanus, erythrorhynchos, americanWhitePelican, lakePond, fish, ground, surfaceDive, lc).
			species(pelecanus, occidentalis, brownPelican, ocean, fish, tree, aerialDive, lc).
			species(botaurus, lentiginosus, americanBittern, marsh, fish, ground, stalking, lc).
			species(ixobrychus, exilis, leastBittern, marsh, fish, ground, stalking, lc).
			species(ardea, herodias, greatBlueHeron, marsh, fish, tree, stalking, lc).
			species(ardea, alba, greatEgret, marsh, fish, tree, stalking, lc).
			species(egretta, thula, snowyEgret, marsh, fish, tree, stalking, lc).
			species(egretta, caerulea, littleBlueHeron, marsh, fish, tree, stalking, lc).
			species(egretta, tricolor, tricoloredHeron, marsh, fish, tree, stalking, lc).
			species(egretta, rufescens, reddishEgret, marsh, fish, tree, stalking, nt).
			species(bubulcus, ibis, cattleEgret, marsh, insects, tree, groundForager, lc).
			species(butorides, virescens, greenHeron, marsh, fish, tree, stalking, lc).
			species(nycticorax, nycticorax, blackCrownedNightHeron, marsh, fish, tree, stalking, lc).
			species(nyctanassa, violacea, yellowCrownedNightHeron, marsh, insects, tree, stalking, lc).
			species(eudocimus, albus, whiteIbis, marsh, insects, tree, probing, lc).
			species(plegadis, falcinellus, glossyIbis, marsh, insects, ground, probing, lc).
			species(plegadis, chihi, whiteFacedIbis, marsh, insects, ground, probing, lc).
			species(platalea, ajaja, roseateSpoonbill, marsh, fish, tree, probing, lc).
%Filters species to a smaller set of data
speciesNameInfo(P, N, C) :- species(P, N, C, _, _, _, _, _).
speciesBirdInfo(Name, H, F, N, B, C) :- species(_, Name, _, H, F, N, B, C).
%Only One Order Taken care of Up Top
%order(A) :- order(A).
%Filter and check family name
family(A) :- family(_, A).
%Filter and check genus name
genus(A) :- genus(_, A, _).
%Filter and check raw species name
species(A) :- speciesNameInfo(_, A, _).

%Checks for parent for raw species name, genus, and family
hasParent(A, B) :- family(B, A) ; genus(B, A, _) ; speciesNameInfo(B, A, _).

%Checks for parent for compound species name, genus, and Family
hasParent2(A, B) :- family(B, A) ; genus(B, A, _) ; (speciesNameInfo(B, X, _), hasCompoundName(B, X, A)).

%Checks for common name given the compound name, or genus name
hasCommonName(N, C) :- genus(_, N, C) ; (hasCompoundName(X, Y, N), speciesNameInfo(X, Y, C)).

%Checks for common name given the genus, and species
hasCommonName(G, S, C) :- speciesNameInfo(G, S, C).

%Given a common name, checks for the compound name, or genus name
hasSciName(C, N) :- hasCommonName(N, C).

%Checks for compound name given a genus and species by using atom_concat to add underscore between genus and species
hasCompoundName(G,S,N) :- speciesNameInfo(G, S, _), atom_concat(G,'_', X), atom_concat(X,S,N).

%Checks for any ancestors including itself using all names except raw species name
isaStrict(A, B) :- A = B, (order(B) ; family(B) ; genus(B) ; hasCompoundName(_,_,B)).
%Through use of recursion, and hasParent2 to check link between ancestors and compound name
isaStrict(A, B) :- hasParent2(A, X), isaStrict(X, B).

%Extends isaStrict by putting conditions on A B being variables or atoms to enable check with common names, but return compound names
isa(A,B) :-  atomic(A), atomic(B), hasCommonName(X, A), hasCommonName(Y,B), isaStrict(X,Y).
isa(A,B) :-  atomic(A), hasCommonName(X, A), isaStrict(X,B).
isa(A,B) :-  atomic(B), hasCommonName(Y,B), isaStrict(A,Y).
isa(A,B) :-  isaStrict(A,B).

%Check if two names are synonym by checking if their common names match, or name provided is common name to taxonmical name. Does not allow for same name
synonym(A, B) :- (hasCommonName(A, B) ; hasCommonName(B, A) ; (hasCommonName(X, A), hasCommonName(X, B))),A \= B.

%Count species will return the amount of species within a family genus or order. Species itself will return one.
countSpecies(A, N) :- listSpecies(A, L), length(L, N).
countSpecies(A, N) :- \+ listSpecies(A, _), N is 0.
%Helper function to countSpecies to put the species into a list
listSpecies(A, [A]) :- hasCompoundName(G, S, A), species(S), genus(G).
listSpecies(A, L) :- genus(A), setof(X, hasSpecies(A, X), L).
listSpecies(A, L) :- family(A), setof(X, hasSpecies(A, X), L).
listSpecies(A, L) :- order(A), setof(X, hasSpecies(A, X), L).
%helper function to countSpecies to check if species exist under that ancestor
hasSpecies(G, S) :- genus(G), hasParent2(S, G).
hasSpecies(F, S) :- family(F), hasParent2(G, F), hasParent2(S, G).
hasSpecies(O, S) :- order(O), hasParent2(F, O), hasParent2(G, F), hasParent2(S, G).

%Data facts that indicate species that are within canada and alberta
rangesTo(pelecanus_erythrorhynchos, alberta).
rangesTo(botaurus_lentiginosus, alberta).
rangesTo(ardea_herodias, alberta).
rangesTo(ardea_alba, canada).
rangesTo(bubulcus_ibis, canada).
rangesTo(butorides_virescens, canada).
rangesTo(nycticorax_nycticorax, alberta).
%Rules for rangeTo that allows for the implication of whatever is in alberta is also in canada
rangesTo(A, canada) :- rangesTo(A, alberta).
%Using recursion with hasParent2 to account for relationships between species, genus, family, and order that are within canada, to allow for search.
rangesTo(A, P) :- \+hasCompoundName(_,_, A), hasParent2(B,A) , rangesTo(B, P).

%Following are implemented using the same format but changing which field of speciesBirdInfo we are accessing from the information defined in species.
%First rule of each of the following uses hasCompoundName name to split up the raw species name to search for within speciesBirdInfo defined above
%second rule of each of the following uses recursion and hasParent2 similar to rangeTo to allow searches other than with compound names.

%Checks species habitat given a compound name
habitat(A, H) :- hasCompoundName(_, X, A), speciesBirdInfo(X, H, _, _, _, _).
habitat(A, H) :- \+hasCompoundName(_,_,A), hasParent2(X, A), habitat(X,H).

%Checks species food given a compound name
food(A, F) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, F, _, _, _).
food(A, F) :- \+hasCompoundName(_,_,A), hasParent2(X, A), food(X,F).

%Checks species nesting given a compound name
nesting(A, N) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, _, N, _, _).
nesting(A, N) :- \+hasCompoundName(_,_,A), hasParent2(X, A), nesting(X,N).

%Checks species behavior given a compound name
behavior(A, B) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, _, _, B, _).
behavior(A, B) :- \+hasCompoundName(_,_,A), hasParent2(X, A), behavior(X,B).

%check species conservation given a compound name
conservation(A, C) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, _, _, _, C).
conservation(A, C) :- \+hasCompoundName(_,_,A), hasParent2(X, A), conservation(X,C).
