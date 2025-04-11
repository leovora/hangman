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

Suggerimento::usage = "La funzionalit\[AGrave] Verifica Risultato serve ad aiutare l\[CloseCurlyQuote]utente ad uscire da una eventuale impasse."

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


(* Implementazione della funzione suggerimento *)
Suggerimento[word_List, state_List, errors_List, score_Integer, gameMode_Integer] :=
Module[ {suggerimento, lettereMancanti, newScore},
	(* Recupero delle lettere mancanti tramite una sottrazione insiemistica *)
	lettereMancanti = DeleteDuplicates[Complement[word, state]];
	
	(* Scelta casuale di una tra le lettere mancanti *)
	suggerimento = RandomChoice[lettereMancanti];
	
	(* Sottrazione del punteggio per aver usato un suggerimento *)
	newScore = score - 5 * gameMode;
	
	(* Simulazione della scelta di una lettera con la lettera suggerita, chiamata con gameMode=0 per evitare che incrementi il punteggio *)
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
	
	(* Ritorna lo stato, gli errori e lo score aggiornati *)
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
	If[FileExistsQ[file], (* Controllo l'esistenza del file *)
	classifica = Import[file, "JSON"]; (* Se il file esiste carica i suoi dati *)
	If[!ListQ[classifica], classifica = {}], (* Se non ci sono dati inizializza classifica come lista vuota *)
	classifica = {} (* Se non esite il file inizializza classifica come lista vuota *)
	];
	
	(* Ritorna la classifica ordinata in maniera decrescente in base al punteggio *)
	classifica = Reverse@SortBy[classifica, #[[2,2]] &] (* Usa l'operatore di partizione per accedere al valore del punteggio *)
];

MostraClassificaGUI[score_Integer] := 
DynamicModule[{nome = "", classifica = {}, file},
	(* Inizializza classifica *)
	SetDirectory[NotebookDirectory[]];
	file = "score.json";
	classifica = RecuperaClassifica[];
	
	(* Creazione del pop-up *)
	CreateDialog[
		Framed[
			Column[{
				Style["Classifica", Bold, 16],
				(* Creazione della tablella dei punteggi *)
				Dynamic@Grid[
				Prepend[
					(* Con MapIndexed applico l'operatore di partizione a ogni elemento di classifica accedendo a nome e punteggio *)
					MapIndexed[{#2[[1]], #[[1,2]], #[[2,2]]} &, classifica],
					{"#", "Nome", "Punteggio"}
					],
					Frame -> All,
					Alignment -> Center
				],
			(* Campo di inserimento del nome per il salvataggio nella classifica *)
			"Inserisci il tuo nome:",
			InputField[Dynamic[nome], String, FieldSize -> 20],
			(* Bottone per salvare il punteggio della partita e aggiornare la classifica *)
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



DisegnaImpiccato[n_] := Graphics[
  {
    Thick,
    Line[{{0, 0}, {3, 0}}], (* base *)
    Line[{{1.5, 0}, {1.5, 5}}], (* palo verticale *)
    Line[{{1.5, 5}, {3, 5}}], (* trave orizzontale *)
    Line[{{3, 5}, {3, 4.5}}], (* corda *)

    If[n >= 1, Circle[{3, 4}, 0.5], Nothing], (* testa *)
    If[n >= 2, Line[{{3, 3.5}, {3, 2.5}}], Nothing], (* corpo *)
    If[n >= 3, Line[{{3, 3.3}, {2.7, 3}}], Nothing], (* braccio sinistro *)
    If[n >= 4, Line[{{3, 3.3}, {3.3, 3}}], Nothing], (* braccio destro *)
    If[n >= 5, Line[{{3, 2.5}, {2.7, 2}}], Nothing], (* gamba sinistra *)
    If[n >= 6, Line[{{3, 2.5}, {3.3, 2}}], Nothing] (* gamba destra *)
  },
  PlotRange -> {{0, 4}, {0, 6}}, ImageSize -> 200
]


GeneraInterfaccia[] := DynamicModule[
  {
    fase = "selezione", (* Fase corrente del gioco: "selezione" o "gioco" *)
    gamemode = 1, (* Modalit\[AGrave] di difficolt\[AGrave]: 1 = Facile, 2 = Media, 3 = Difficile *)
    seed = Automatic, (* Valore del seed per la generazione pseudo-casuale della parola *)
    parola, stato, errori, score, (* Variabili per la parola da indovinare, lo stato del gioco, gli errori e il punteggio *)
    letteraUtente = "", (* Lettera immessa dall'utente *)
    messaggio = "", (* Messaggio da mostrare all'utente (corretto/sbagliato) *)
    maxErrori = 6, (* Numero massimo di errori consentiti *)
    classificaMostrata = False (* Flag per verificare se la classifica \[EGrave] gi\[AGrave] stata mostrata *)
  },

  Dynamic[
	(* Switch per determinare quale parte dell'interfaccia mostrare in base alla fase del gioco *)
    Switch[fase, 

      "selezione", (* Fase di selezione della difficolt\[AGrave] e del seed *)
      Column[{
        Style["\|01f3afSeleziona la difficolt\[AGrave]", Bold, 16],
        RadioButtonBar[Dynamic[gamemode], {1 -> "Facile", 2 -> "Media", 3 -> "Difficile"}],
        
        Row[{"Seed (opzionale): ", InputField[Dynamic[seed], String]}], 
        
        Button["Inizia partita", 
          (
            If[StringMatchQ[seed, NumberString], seed = ToExpression[seed], seed = Automatic]; (* Verifica e converte il seed in numero, altrimenti lascia Automatic *)
            {parola, stato, errori, score} = GeneraEsercizio[gamemode, seed]; (* Chiamata alla funzione GeneraEsercizio per generare la parola, stato iniziale, errori e punteggio *)
            fase = "gioco"; (* Passa alla fase di gioco *)
            letteraUtente = ""; messaggio = ""; classificaMostrata = False; (* Resetta le variabili per la partita *)
          ),
          ImageSize -> Medium 
        ]
      }, Spacings -> 2], 

      "gioco", (* Fase di gioco, dove l'utente interagisce con il gioco *)
      Column[{
        Style["Gioco dell'impiccato", Bold, 20],
        Dynamic[Row[Riffle[If[# === "_", Style[" _ ", Gray], Style[#]] & /@ stato, " "]]], (* Visualizza lo stato attuale della parola con gli spazi vuoti per le lettere da indovinare *)

        Row[{
          "Lettera: ", 
          InputField[Dynamic[letteraUtente], String], (* Input per la lettera che l'utente vuole provare *)
          Button["Prova", (* Bottone per fare una prova con la lettera scelta *)
            Module[{guess = ToLowerCase[StringTrim[letteraUtente]]}, (* Elimina gli spazi e converte la lettera in minuscolo *)
              If[StringLength[guess] == 1 && LetterQ[guess], (* Verifica che la stringa sia una sola lettera *)
                If[!MemberQ[Join[stato, errori], guess], (* Verifica che la lettera non sia gi\[AGrave] stata provata *)
                  {stato, errori, score} = AggiornaStato[parola, stato, guess, score, gamemode, errori]; (* Aggiorna lo stato del gioco con la lettera scelta *)
                  messaggio = If[MemberQ[parola, guess], "Lettera corretta!", "Lettera sbagliata!"]; (* Mostra un messaggio in base al risultato della prova *)
                ];
              ];
              letteraUtente = ""; (* Reset dell'input della lettera dopo la prova *)
            ]
          ]
        }],
        
        Row[{
          Button["\|01f4a1Suggerimento", 
            {stato, errori, score} = Suggerimento[parola, stato, errori, score, gamemode] (* Fornisce un suggerimento e aggiorna stato, errori e punteggio *)
          ],
          Spacer[20],
          Button["\|01f9fdPulisci", 
            {stato, errori, score} = Pulisci[parola] (* Resetta lo stato del gioco con la funzione Pulisci *)
          ],
          Spacer[20], 
          Button["\|01f50eMostra soluzione",
            MostraSoluzione[parola] (* Mostra la soluzione *)
          ]
        }],

        Dynamic[Row[{"Lettere sbagliate: ", StringJoin[Riffle[errori, ", "]]}]], (* Visualizza le lettere sbagliate provate dall'utente *)
        Dynamic[Row[{"Errori: ", Length[errori], "/", maxErrori}]], (* Mostra il numero di errori *)
        Dynamic[Style[messaggio, Blue]], (* Visualizza il messaggio di feedback (corretto/sbagliato) *)
        Dynamic[DisegnaImpiccato[Length[errori]]]

        Dynamic[
          If[
            stato === parola || Length[errori] >= maxErrori, (* Se la parola \[EGrave] indovinata o sono stati raggiunti troppi errori *)
            (
              If[!classificaMostrata,
                classificaMostrata = True;
                MostraClassificaGUI[score]; (* Mostra la classifica al termine del gioco *)
              ];
              Style[
                If[stato === parola,
                  "Hai vinto!", (* Se la parola \[EGrave] indovinata, mostra il messaggio di vittoria *)
                  "Hai perso! La parola era: " <> StringJoin[parola] (* Altrimenti, mostra la parola corretta e il messaggio di sconfitta *)
                ],
                If[stato === parola, Green, Red], Bold 
              ]
            ),
            ""
          ]
        ],

        Button["\|01f504Nuova partita", (* Bottone per iniziare una nuova partita *)
          (
            fase = "selezione"; (* Torna alla fase di selezione per una nuova partita *)
            letteraUtente = ""; messaggio = ""; (* Resetta le variabili *)
          )
        ]
      }]
    ]
  ]
];



(* Si puo' giocare da qua *)
GeneraInterfaccia[]


{state, errors, score} = AggiornaStato[word, state, "a", score, gameMode, errors]


{state, errors, score} = Suggerimento[word, state, errors, score, gameMode]


MostraClassificaGUI[score]


End[]


EndPackage[]
