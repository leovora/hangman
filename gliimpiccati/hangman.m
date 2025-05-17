(* ::Package:: *)

(* :Title: HangmanGame *)
(* :Context: HangmanGame` *)
(* :Author: AN, LV, CN *)
(* :Summary: funzioni utilizzate nell'interfaccia hangman.nb *)
(* :Copyright: AN, LV, CN 2025 *)
(* :Package Version: 3 *)
(* :Mathematica Version: 14 *)
(* :History: last modified 17/05/2025 *)
(* :Sources: bblio *)
(* :Limitations: educational purposes *)
(* :Discussion: *)
(* :Requirements: *)

BeginPackage["HangmanGame`"];

GeneraInterfaccia::usage = "GeneraInterfaccia[]
	Funzione che permette di generare un'interfaccia interattiva e dinamica, la quale richiama
	le altre funzionalit\[AGrave] del gioco.";


Begin["`Private`"];


(* Implementazione della funzione GeneraEsericizio *)
GeneraEsercizio[ gamemode_:1, seed_:Automatic ] := Module[ 
{ 
	wordlist, (* Lista delle possibili parole *)
	wordlen, (* Lunghezza della parola *)
	word, (* Parola da indovinare *)
	stato, (* Vettore di trattini bassi *)
	errors={}, (* Lista di errori *)
	score=0 (* Punteggio *)
}, 
	(* Imposta la lunghezza delle possibili parole in base alla difficolt\[AGrave] selezionata *)
	If[ gamemode === 1, wordlen = {1,5} ]; (* 1 = facile *)
	If[ gamemode === 2, wordlen = {6,8} ]; (* 2 = media *)
	If[ gamemode === 3, wordlen = {9,15} ]; (* 3 = difficile *)
	
	(* Filtra il dizionario italiano per parole della lunghezza desiderata e senza caratteri non ammessi *)
	wordlist = Select[DictionaryLookup[{"Italian","*"}], Between[wordlen][StringLength[#]] && !HaCaratteriNonAmmessiQ[#] &];
	
	(* Imposta il seed per la selezione pseudo-casuale *)
	SeedRandom[seed];
	
	(* Estrae una parola casuale e la scompone in caratteri *)
	word = Characters[RandomChoice[wordlist]];
	
	(* Inizializzazione dello stato di gioco con trattini bassi per ogni lettera *)
	stato = InizializzaStato[word];
	
	(* Ritorna il vettore lettere (in minuscolo), lo stato, la lista vuota di errori e punteggio a zero *)
	{ToLowerCase[word], stato, errors, score}
];


(* Mostra la selezione in un pop-up *)
MostraSoluzione[soluzione_List] := Module[
{
	stringa (* Soluzione da mostrare *)
},
	(* Concatenazione della lista di caratteri in una stringa da mostrare all'utente *)
	stringa = StringJoin[soluzione];
	
	(* Messaggio da mostrare all'utente attraverso un pop-up *)
	MessageDialog[
		Panel[
			Row[{"La parola da indovinare era: ", Style[stringa, Bold]}],
			Alignment -> Center
		]
	]
];


(* Fornisce un suggerimento e penalizza il punteggio *)
Suggerimento[word_List, stato_List, errors_List, score_Integer, gameMode_Integer] := Module[
{
	suggerimento, (* Una tra le lettere mancanti *)
	lettereMancanti, (* Lista di lettere mancanti senza ripetizioni *)
	newScore (* Punteggio aggiornato con la penalit\[AGrave] *)
},
	(* Calcola le lettere mancanti senza ripetizioni *)
	lettereMancanti = DeleteDuplicates[Complement[word, stato]];
	
	(* Scelta casuale di una lettera mancante *)
	suggerimento = RandomChoice[lettereMancanti];
	
	(* Penalit\[AGrave]: -5 punti per ogni livello di difficolt\[AGrave]  *)
	newScore = score - 5 * gameMode;
	
	(* Applica il suggerimento aggiornando lo stato senza cambiare punteggio *)
	AggiornaStato[word, stato, suggerimento, newScore, 0, errors]
];


(* Reinizializza lo stato di gioco *)
Pulisci[stato_List] := Module[
{
	newState, (* Vettore di trattini bassi quanti la lunghezza della parola *)
	errors = {}, (* Errori *)
	score = 0 (* Punteggio *)
},
	(* Reinizializza lo stato *)
	newState = InizializzaStato[stato];
	
	(* Ritorna il nuovo stato e la lista vuota degli errori *)
	{newState, errors, score} 
];


(* Funzioni ausiliarie *)
(* Verifica la presenza di caratteri accentati o apostrofi *)
HaCaratteriNonAmmessiQ[s_] := Module[ 
{ 
	accenti = {"\[AGrave]", "\[EGrave]", "\[IGrave]", "\[OGrave]", "\[UGrave]", "\[AAcute]", "\[EAcute]", "\[IAcute]", "\[OAcute]", "\[UAcute]"} (* Vettore delle lettere accentate presenti nella lingua italiana *)
},
	StringContainsQ[s, Alternatives @@ accenti] || StringContainsQ[s, "'"]
];

(* Crea un array di trattini bassi della lunghezza della parola *)
InizializzaStato[word_List] := ConstantArray["_", Length[word]]

(* Aggiorna lo stato in base al tentativo dell'utente *)
AggiornaStato[word_List, currentState_List, guess_, score_Integer, gameMode_, errors_List:{}] := Module[
{
	newState, (* Nuovo vettore che indica le lettere indovinate o trattini bassi *)
	newErrors, (* Nuovo vettore con le lettere non presenti nella parola da indovinare *)
	newScore (* Punteggio aggiornato in caso la lettera \[EGrave] stata indovinata *)
},

	If[MemberQ[word, guess],
		(* Lettera corretta: aggiorno currentState e punteggio *)
		newState = MapThread[If[#2 == guess, guess, #1] &, {currentState, word}];
		newErrors = errors;
		newScore = score + 10 * gameMode, (* In caso di lettera corretta per suggerimento gameMode = 0 *)
		
		(* Lettera sbagliata: mantengo lo stato e aggiungo l'errore *)
		newState = currentState;
		newErrors = Append[errors, guess];
		newScore = score
	];
	
	(* Ritorna un vettore con lo stato, gli errori e il punteggio aggiornati *)
	{newState, newErrors, newScore}
];

(* Salva il record di punteggio in un file JSON *)
SalvaRecord[nome_String, punteggio_Integer, file_:"score.json"] := Module[
{
	record, (* Record da memorizzare *)
	datiEsistenti = {}, (* Record memorizzati precedentemente *)
	nuovoContenuto (* Lista dei 10 migliori record ordinati in base al punteggio *)
},
	(* Nuovo record come associazione *)
	record = {"nome" -> nome, "punteggio" -> punteggio};
  
	(* Se il file esiste, prova a caricarlo *)
	datiEsistenti = RecuperaClassifica[];
  
	(* Aggiungi il nuovo record *)
	nuovoContenuto = Append[datiEsistenti, record];

	(* Ordina per punteggio decrescente e prendi solo i primi 10 *)
	nuovoContenuto = Take[Reverse@SortBy[nuovoContenuto, #[[2,2]] &], UpTo[10]];
	
	(* Sovrascrivi il file con tutti i record *)
	Export[file, nuovoContenuto, "JSON"];
];

(* Carica e ordina la classifica dal file JSON *)
RecuperaClassifica[file_:"score.json"] := Module [
{
	classifica = {} (* Lista ordinata dei record *)
},
	If[FileExistsQ[file], (* Verifica l'esistenza del file *)
	classifica = Import[file, "JSON"]; (* Se il file esiste carica i suoi dati *)
	If[!ListQ[classifica], classifica = {}], (* Se non ci sono dati inizializza classifica come lista vuota *)
	classifica = {} (* Se non esite il file inizializza classifica come lista vuota *)
	];
	
	(* Ordina in ordine descrescente di punteggio *)
	classifica = Reverse@SortBy[classifica, #[[2,2]] &] (* Usa l'operatore di partizione per accedere al valore del punteggio *)
];

(* Interfaccia grafica per mostrare la classifica *)
MostraClassificaGUI[score_Integer] := DynamicModule[
{
	nome = "", (* Nome da memorizzare nella classifica *)
	classifica = {}, (* Lista ordinata dei record *)
	punteggioSalvato = False, (* Flag per verificare se il punteggio \[EGrave] gi\[AGrave] stato salvato *)
	file = "score.json" (* Nome del file dove viene memorizzata la classifica *)
},
	(* Inizializza classifica *)
	SetDirectory[NotebookDirectory[]];
	classifica = RecuperaClassifica[];

	(* Creazione del pop-up *)
	CreateDialog[
		Framed[
			Dynamic[
				Column[{
					Style["Classifica", Bold, 16],
					(* Creazione della tablella dei punteggi *)
						Grid[
							Prepend[
								(* Con MapIndexed applico l'operatore di partizione a ogni elemento di classifica accedendo a nome e punteggio *)
								MapIndexed[{#2[[1]], #[[1,2]], #[[2,2]]} &, classifica],
								{"#", "Nome", "Punteggio"}
							],
						Frame -> All,
						Alignment -> Center
						],
					If[
						!punteggioSalvato,
						Column[{
								(* Campo per inserire il nome *)
	                            "Inserisci il tuo nome:",
	                            InputField[Dynamic[nome], String, FieldSize -> 20],
	                            (* Pulsante per salvare il record *)
		                            Button[
		                                 "Salva Punteggio",
										Module[{nomeVal = nome},
		                                    SalvaRecord[nomeVal, score];
		                                    classifica = RecuperaClassifica[];
		                                    punteggioSalvato = True;
		                                 ],
		                                 Enabled -> Dynamic[StringLength[nome] > 0]
		                            ]
	                        }],
	                        (* Pulsante per chiudere il pop-up *)
	                        Button["Chiudi", DialogReturn[]]
					]
				}, Spacings -> 1.5]
			],
			FrameMargins -> 20,
			FrameStyle -> None,
			RoundingRadius -> 10,
			Background -> LightGray
		],
		WindowTitle->"Classifica"
	]
];


(* Disegna l'impiccato in base al numero di errori *)
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


(* righe della tastiera con relativo offset *)
righeTastiera = {
  {0, {"Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P"}},   
  {1, {"A", "S", "D", "F", "G", "H", "J", "K", "L"}},        
  {2, {"Z", "X", "C", "V", "B", "N", "M"}}              
};


(* Genera l'interfaccia complete per il gioco *)
GeneraInterfaccia[] := DynamicModule[
  {
    seed, (* Valore del seed per la generazione pseudo-casuale della parola *)
    seedError = "", (* Variabile per memorizzare il messaggio di errore del seed *)
    fase = "selezione", (* Fase corrente del gioco: "selezione" o "gioco" *)
    gamemode = 1, (* Modalit\[AGrave] di difficolt\[AGrave]: 1 = Facile, 2 = Media, 3 = Difficile *)
    parola, stato, errori, score, (* Variabili per la parola da indovinare, lo stato del gioco, gli errori e il punteggio *)
    letteraUtente = "", (* Lettera immessa dall'utente *)
    messaggio = "", (* Messaggio da mostrare all'utente (corretto/sbagliato) *)
    maxErrori = 6, (* Numero massimo di errori consentiti *)
    classificaMostrata = False (* Flag per verificare se la classifica \[EGrave] gi\[AGrave] stata mostrata *)
  },

  Dynamic[
	
    Switch[fase, (* Switch per determinare quale parte dell'interfaccia mostrare in base alla fase del gioco *)

      "selezione", (* Fase di selezione della difficolt\[AGrave] e del seed *)
      Column[{
        Style["\|01f3afSeleziona la difficolt\[AGrave]", Bold, 16], (* Titolo della sezione di selezione *)
        RadioButtonBar[Dynamic[gamemode], {1 -> "Facile", 2 -> "Media", 3 -> "Difficile"}], (*Selezione difficolt\[AGrave]*)
        (* Controllo sull'input del seed su ogni carattere inserito *)
        Row[{"Seed (opzionale): ", InputField[
				Dynamic[seed, ({seed, seedError} = If[StringMatchQ[#, DigitCharacter ..] || StringMatchQ[#, ""], {#, ""}, {seed, "\:26a0\:fe0f Inserire solo numeri naturali (0, 1, 2, ...)."}]) &],  (* Controllo dinamico del seed inserito *)
				String, (* Tipo di input: stringa *)
				FieldHint->"Inserire un numero naturale", (* Suggerimento per il tipo di input accettato *)
				ContinuousAction->True (* Per eseguire il controllo ad ogni input *)
				]}], 
		(* Messaggio di errore in caso non venga inserito un input valido *)
        Dynamic[
			If[seedError != "",
				Style[seedError, Red, Italic], (* Mostra il messaggio di errore in rosso *)
				"" (* Nessun messaggio se il seed \[EGrave] corretto *)
			]
        ],
        Button["Inizia partita", (*Pulsante per iniziare la partita*)
          Module[{},
            If[StringQ[seed] && seed != "", seed = ToExpression[seed], seed = Automatic]; (* Verifica e converte il seed in numero, altrimenti lascia Automatic *)
            {parola, stato, errori, score} = GeneraEsercizio[gamemode, seed]; (* Chiamata alla funzione GeneraEsercizio per generare la parola, stato iniziale, errori e punteggio *)
            fase = "gioco"; (* Passa alla fase di gioco *)
            letteraUtente = ""; messaggio = ""; classificaMostrata = False; (* Resetta le variabili per la partita *)
          ],
          ImageSize -> Medium 
        ]
      }, Spacings -> 2], 

      "gioco", (* Fase di gioco, dove l'utente interagisce con il gioco *)
      Column[{
        Style["Gioco dell'impiccato", Bold, 20],
        Dynamic[Row[{"Punteggio: ", Style[score, Blue, Bold]}]], (* Visualizza il punteggio *)
        Dynamic[Row[Riffle[If[# === "_", Style[" _ ", Gray], Style[#]] & /@ stato, " "]]], (* Visualizza lo stato attuale della parola con gli spazi vuoti per le lettere da indovinare *)
        
        Row[{
          Button["\|01f4a1Suggerimento", 
            {stato, errori, score} = Suggerimento[parola, stato, errori, score, gamemode], (* Fornisce un suggerimento e aggiorna stato, errori e punteggio *)
			Enabled -> MemberQ[stato, "_"] && Length[errori] < maxErrori
          ],
          Spacer[20], 
          Button["\|01f50eMostra soluzione",
           MostraSoluzione[parola] (* Fornisce la soluzione e termina la partita *)
           {stato = parola, fase = "selezione", letteraUtente = ""; messaggio = ""; seedError = ""}
           ]
        }],

        Dynamic[Row[{"Lettere sbagliate: ", StringJoin[Riffle[errori, ", "]]}]], (* Visualizza le lettere sbagliate provate dall'utente *)
        Dynamic[Row[{"Errori: ", Length[errori], "/", maxErrori}]], (* Mostra il numero di errori *)
        Dynamic[Style[messaggio, Blue]], (* Visualizza il messaggio di feedback (corretto/sbagliato) *)
        Dynamic[DisegnaImpiccato[Length[errori]]], (*Disegna l'impiccato con l'attuale numero di errori*)
		(* Mostra l'intera tastiera come colonna di righe *)
		Dynamic[
			Column[
				Map[
					Row[
						Join[
							{Spacer[#[[1]]*25]}, 
							Table[
								With[{l = lettera}, (* Blocca il valore della lettera per ogni bottone, cos\[IGrave] non viene sovrascritto *)
									Button[
										l, 
										Module[{guess = ToLowerCase[StringTrim[l]]}, (* Elimina gli spazi e converte la lettera in minuscolo *)
											{stato, errori, score} = AggiornaStato[parola, stato, guess, score, gamemode, errori]; (* Aggiorna lo stato del gioco con la lettera scelta *)
											messaggio = If[MemberQ[parola, guess], "Lettera corretta!", "Lettera sbagliata!"]; (* Mostra un messaggio in base al risultato della prova *)
										], 
										(* Disabilito il tasto se e' gia' stato premuto oppure se la partita e' finita *)
										Enabled -> !MemberQ[Join[stato, errori], ToLowerCase[l]] &&
											MemberQ[stato, "_"] &&
											Length[errori] < maxErrori,
										(* Cambio il colore al tasto premuto: verde se e' corretto, rosso se non e' corretto *)
										Background->Which[
											MemberQ[stato, ToLowerCase[l]], LightGreen,
											MemberQ[errori, ToLowerCase[l]], LightRed],
										ImageSize -> {40, 40}
									]
								],
								{lettera, #[[2]]} (* Itera sulle lettere della riga *)
							]
						],
						Spacer[5] 
					] &,
					righeTastiera
				], (* Applica la funzione creaRiga a ciascuna riga *)
				Spacings -> 1 
			]
		],
        Dynamic[
        If[
            stato === parola || Length[errori] >= maxErrori, (* Se la parola \[EGrave] indovinata o sono stati raggiunti troppi errori *)
            Column[{
              Module[{},
                If[!classificaMostrata, (* Controlla se la classifica \[EGrave] gi\[AGrave] stata mostrata *)
                  classificaMostrata = True;
                  MostraClassificaGUI[score]; (* Mostra la classifica al termine del gioco *)
                ];        
                Style[
                  If[stato === parola, (* Messaggio finale in base all'esito della partita *)
                    "Hai vinto!", (* Se la parola \[EGrave] indovinata, mostra il messaggio di vittoria *)
                    "Hai perso! La parola era: " <> StringJoin[parola] (* Altrimenti, mostra la parola corretta e il messaggio di sconfitta *)
                  ],
                  If[stato === parola, Green, Red], Bold (*Imposta lo stile della scritta*)
                ]        
              ],
              Button["\|01f9fdPulisci",
              (* Non viene aggiornata classificaMostrata in modo da non poter salvare il punteggio dopo aver rigiocata la stessa parola *)
                {stato, errori, score} = Pulisci[parola] (* Resetta lo stato del gioco con la funzione Pulisci *)
              ]
            }],
            ""        
          ]
        ],
        Button["\|01f504Nuova partita", (* Bottone per iniziare una nuova partita *)
          Module[{},
            fase = "selezione"; (* Torna alla fase di selezione per una nuova partita *)
            letteraUtente = ""; messaggio = ""; seedError = "";(* Resetta le variabili *)
          ]
        ]
      }]
    ]
  ]
];


End[];


EndPackage[]
