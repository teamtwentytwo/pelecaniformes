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
family(A) :- family(_, A).
genus(A) :- genus(_, A, _).
species(A) :- speciesNameInfo(_, A, _).

hasParent(A, B) :- family(B, A) ; genus(B, A, _) ; speciesNameInfo(B, A, _).

hasParent2(A, B) :- family(B, A) ; genus(B, A, _) ; (speciesNameInfo(B, X, _), hasCompoundName(B, X, A)).

hasCommonName(N, C) :- genus(_, N, C) ; (hasCompoundName(X, Y, N), speciesNameInfo(X, Y, C)).

hasCommonName(G, S, C) :- speciesNameInfo(G, S, C).

hasSciName(C, N) :- hasCommonName(N, C).

hasCompoundName(G,S,N) :- speciesNameInfo(G, S, _), atom_concat(G,'_', X), atom_concat(X,S,N).

isaStrict(A, B) :- A = B, (order(B) ; family(B) ; genus(B) ; hasCompoundName(_,_,B)).
isaStrict(A, B) :- hasParent2(A, X), isaStrict(X, B).

%isa(A,B) :-   .

synonym(A, B) :- (hasCommonName(A, B) ; hasCommonName(B, A) ; (hasCommonName(X, A), hasCommonName(X, B))),A \= B.

%countSpecies :- .

rangesTo(pelecanus_erythrorhynchos, alberta).
rangesTo(botaurus_lentiginosus, alberta).
rangesTo(ardea_herodias, alberta).
rangesTo(ardea_alba, canada).
rangesTo(bubulcus_ibis, canada).
rangesTo(butorides_virescens, canada).
rangesTo(nycticorax_nycticorax, alberta).

rangesTo(A, canada) :- rangesTo(A, alberta).
rangesTo(A, P) :- \+hasCompoundName(_,_, A), hasParent2(B,A) , rangesTo(B, P). 
/*
%birdInfo(name, habitat, food, nesting, behavior, conservation)
birdInfo(americanWhitePelican, lakePond, fish, ground, surfaceDive, lc).
birdInfo(brownPelican, ocean, fish, tree, aerialDive, lc).
birdInfo(americanBittern, marsh, fish, ground, stalking, lc).
birdInfo(leastBittern, marsh, fish, ground, stalking, lc).
birdInfo(greatBlueHeron, marsh, fish, tree, stalking, lc).
birdInfo(greatEgret, marsh, fish, tree, stalking, lc).
birdInfo(snowyEgret, marsh, fish, tree, stalking, lc).
birdInfo(littleBlueHeron, marsh, fish, tree, stalking, lc).
birdInfo(tricoloredHeron, marsh, fish, tree, stalking, lc).
birdInfo(reddishEgret, marsh, fish, tree, stalking, nt).
birdInfo(cattleEgret, marsh, insects, tree, groundForager, lc).
birdInfo(greenHeron, marsh, fish, tree, stalking, lc).
birdInfo(blackCrownedNightHeron, marsh, fish, tree, stalking, lc).
birdInfo(yellowCrownedNightHeron, marsh, insects, tree, stalking, lc).
birdInfo(whiteIbis, marsh, insects, tree, probing, lc).
birdInfo(glossyIbis, marsh, insects, ground, probing, lc).
birdInfo(whiteFacedIbis, marsh, insects, ground, probing, lc).
birdInfo(roseateSpoonbill, marsh, fish, tree, probing, lc).
*/
habitat(A, H) :- hasCompoundName(_, X, A), speciesBirdInfo(X, H, _, _, _, _).
habitat(A, H) :- \+hasCompoundName(_,_,A), hasParent2(X, A), habitat(X,H).

food(A, F) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, F, _, _, _).
food(A, F) :- \+hasCompoundName(_,_,A), hasParent2(X, A), food(X,F).

nesting(A, N) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, _, N, _, _).
nesting(A, N) :- \+hasCompoundName(_,_,A), hasParent2(X, A), nesting(X,N).

behavior(A, B) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, _, _, B, _).
behavior(A, B) :- \+hasCompoundName(_,_,A), hasParent2(X, A), behavior(X,B).

conservation(A, C) :- hasCompoundName(_, X, A), speciesBirdInfo(X, _, _, _, _, C).
conservation(A, C) :- \+hasCompoundName(_,_,A), hasParent2(X, A), conservation(X,C).

