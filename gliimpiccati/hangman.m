(* ::Package:: *)

(* :Title: HangmanGame *)
(* :Context: HangmanGame` *)
(* :Author: GS *)
(* :Summary: funzioni utilizzate nell'interfaccia hangman.nb *)
(* :Copyright: GS 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 07/04/2025 *)
(* :Sources: bblio *)
(* :Limitations: educational purposes *)
(* :Discussion: *)
(* :Requirements: *)
(* :Warning: DOCUMENTATE TUTTO il codice *)

BeginPackage["HangmanGame`"]

GeneraEsercizio::usage = "La funzionalit\[AGrave] Genera Esercizio prevede un generatore casuale di un numero potenzialmente infinito di
esercizi. Date all\[CloseCurlyQuote]utente la possibilit\[AGrave] di generare un esercizio e/o rigenerare l\[CloseCurlyQuote]ultimo esercizio corrente, chiedendo un
numero (Seed), ad esempio tramite finestra di input o checkbox."

MostraSoluzione::usage = "La funzionalit\[AGrave] Mostra Soluzione pu\[OGrave] essere resa disponibile anche tramite finestra di pop-up."

VerificaRisultato::usage = "La funzionalit\[AGrave] Verifica Risultato serve ad aiutare l\[CloseCurlyQuote]utente ad uscire da una eventuale impasse."

Pulisci::usage = "La funzionalit\[AGrave] Pulisci riporta l\[CloseCurlyQuote]interfaccia allo stato iniziale, ripulendo anche eventuali celle create (ad
esempio, da Mostra Soluzione e/o Verfica Risultato) durante l\[CloseCurlyQuote]uso della interfaccia stessa"

GeneraInterfaccia::usage = "GeneraInterfaccia[]
	Funzione che permette di generare un'interfaccia interattiva e dinamica, la quale richiama
	le altre funzionalita' del gioco."


Begin["`Private`"]


(* Implementazione della funzione GeneraEsericizio *)
GeneraEsercizio[ gamemode_:1, seed_:Automatic ] :=
Module[ { wordlist, pattern, wordlen, word, state, errors={}, score=0 }, 
	(* Imposta la lunghezza delle possibili parole in base alla difficolta' selezionata *)
	If[ gamemode === 1, wordlen = {1,5} ];
	If[ gamemode === 2, wordlen = {6,8} ];
	If[ gamemode === 3, wordlen = {9,12} ];
	
	(* Recupera la lista di parole dal dizionario italiano filtrando in base alla lunghezza e i caratteri non consentiti *)
	wordlist = Select[DictionaryLookup[{"Italian","*"}], Between[wordlen][StringLength[#]] && !HaCaratteriNonAmmessiQ[#] &];
	
	(* Imposta il seed per la selezione pseudo casuale *)
	SeedRandom[seed];
	
	(* Selezione pseudo casuale di una parola dalla lista *)
	word = Characters[RandomChoice[wordlist]];
	
	(* Inizializzazione dello stato *)
	state = InizializzaStato[word];
	
	(* Ritorno della parola selezionata come lista di caratteri, lo stato iniziale e una lista vuota di errori *)
	{word, state, errors, score}
];


(* Implementazione della funzione MostraSoluzione *)
MostraSoluzione[soluzione_List] := 
Module[{stringa},
	(* Concatenazione della lista di caratteri in una stringa mostrare all'utente *)
	stringa = StringJoin[soluzione];
	
	(* Messaggio da mostrare all'utente attraverso un pop-up *)
	MessageDialog[
		Panel[
			Row[{"La parola da indovinare \[EGrave]: ", Style[stringa, Bold]}],
			Alignment -> Center
		]
	]
];
{ word, state, errors } = GeneraEsercizio[];
MostraSoluzione[word]


Suggerimento[word_List, state_List, errors_List, score_Integer, gameMode_Integer] :=
Module[ {suggerimento, lettereMancanti, newScore},
	lettereMancanti = DeleteDuplicates[Complement[word, state]];
	
	suggerimento = RandomChoice[lettereMancanti];
	
	newScore = score - 5 * gameMode;
	
	AggiornaStato[word, state, suggerimento, newScore, 0, errors]
];


(* Implementazione della funzione Pulisci *)
Pulisci[state_List] :=
Module[ {newState, errors = {}, score = 0},
	(* Reinizializza lo stato *)
	newState = InizializzaStato[state];
	
	(* Ritorna il nuovo stato e la lista vuota degli errori *)
	{newState, errors, score} 
];


(* Implementazione della funzione GeneraInterfaccia *)
GeneraInterfaccia[]:= DynamicModule[{},
	Manipulate[]]


(* Funzioni ausiliarie *)
(* Funzione booleana che verifica se in una stringa data sono presenti lettere accentate o apostrofi *)
HaCaratteriNonAmmessiQ[s_] :=
Module[ { accenti = {"\[AGrave]", "\[EGrave]", "\[IGrave]", "\[OGrave]", "\[UGrave]", "\[AAcute]", "\[EAcute]", "\[IAcute]", "\[OAcute]", "\[UAcute]"} },
	StringContainsQ[s, Alternatives @@ accenti] || StringContainsQ[s, "'"]
];

(* Funzione che data una lista caratteri ritorna una lista di "_" della stessa lunghezza della lista data *)
InizializzaStato[word_List] := ConstantArray["_", Length[word]]

(* Funzione che aggiorna lo stato del gioco *)
AggiornaStato[word_List, currentState_List, guess_, score_Integer, gameMode_, errors_List:{}] := 
Module[{newState, newErrors, newScore},

	If[MemberQ[word, guess],
		(* Lettera correta: aggiorna currentState *)
		newState = MapThread[If[#2 == guess, guess, #1] &, {currentState, word}];
		newErrors = errors;
		newScore = score + 10 * gameMode, (* In caso di lettera corretta per suggerimento gameMode = 0 *)
		
		(* Lettera sbagliata: aggiorna errors *)
		newState = currentState;
		newErrors = Append[errors, guess];
		newScore = score
	];
	
	{newState, newErrors, newScore}
];

SalvaRecord[nome_String, punteggio_Integer, file_:"score.json"] := 
Module[{record, datiEsistenti = {}, nuovoContenuto},
  
	(* Nuovo record come associazione *)
	record = <|"nome" -> nome, "punteggio" -> punteggio|>;
  
	(* Se il file esiste, prova a caricarlo *)
	datiEsistenti = RecuperaClassifica[];
  
	(* Aggiungi il nuovo record *)
	nuovoContenuto = Append[datiEsistenti, record];

	(* Sovrascrivi il file con tutti i record *)
	Export[file, nuovoContenuto, "JSON"];
];

(* Funzione per caricare e ordinare la classifica *)
RecuperaClassifica[file_: "score.json"] := 
Module [{classifica},
	If[FileExistsQ[file],
	classifica = Import[file, "JSON"];
	If[!ListQ[classifica], classifica = {}],
	classifica = {}
	];
	classifica = Reverse@SortBy[classifica, #[[2,2]] &]
];

MostraClassificaGUI[score_Integer] := 
DynamicModule[{nome = "", classifica = {}, file},
	(* Inizializza classifica *)
	SetDirectory[NotebookDirectory[]];
	file = "score.json";
	classifica = RecuperaClassifica[];
	CreateDialog[
		Framed[
			Column[{
				Style["Classifica", Bold, 16],
	    
				Dynamic@Grid[
				Prepend[
					MapIndexed[With[{assoc = Association[#1]}, {#2[[1]], assoc["nome"], assoc["punteggio"]}] &, classifica],
					{"#", "Nome", "Punteggio"}
					],
					Frame -> All,
					Alignment -> Center
				],
				
			"Inserisci il tuo nome:",
			InputField[Dynamic[nome], String, FieldSize -> 20],
			Button["Salva Punteggio", SalvaRecord[nome, score]; classifica = RecuperaClassifica[];, Enabled -> Dynamic[StringLength[nome] > 0]]
			}, Spacings -> 1.5],
			
			FrameMargins -> 20,
			FrameStyle -> None,
			RoundingRadius -> 10,
			Background -> LightGray
		],
		WindowTitle->"Classifica"
	]
];



(* Si puo' giocare da qua *)
gameMode = 2;
{word, state, errors, score} = GeneraEsercizio[gameMode];
word


{state, errors, score} = AggiornaStato[word, state, "a", score, gameMode, errors]


{state, errors, score} = Suggerimento[word, state, errors, score, gameMode]


MostraClassificaGUI[score]


End[]


EndPackage[]
