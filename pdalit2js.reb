vim: syn=rebol sw=2 ts=2 sts=2:
REBOL []
profile: function [
		time [word!]
		code [block!]
	] [
	start: stats/timer
	result: do code
	set :time (get time) + stats/timer - start
	result
]
quote: function [
		{Quote \ and " with \ in string s}
		s
	] [
	q: charset {\"}
	parse to string! s [any[to q insert "\" skip]]
	ajoin[{"} s {"}]
]
latin1-to-utf8: func [
		"Transcodes bin as a Latin-1 encoded string to UTF-8"
		bin [binary!] "Bytes of Latin-1 data"
		/local t
	] [
	t: make string! length? bin
	foreach b bin [append t to char! b ]
	t
]
fix-spaces: function [
		{replace tab,newline,&nbsp; with space}
		s [string!]
	] [
	spc: charset " ^/^-^M"
	spacer: [spc |"&nbsp;"| end]
	r: make string! 2
	parse s [ any[
		end
		| some spacer (append r " ")
		| copy t: to spacer (append r t)
	] ]
	r
]
html-markup: function [
		"Split HTML text in tags, text, entities"
		html [string!]
	] [
	te: charset "<&"
	res: copy []
	t: none
	rule: [any [
		"<" copy t
			[ "script" thru ">" thru "</script"
			| "style" thru ">" thru "</style"
			| "!--" thru "--" 
			| to ">" ] ">" (append res to tag! t)
		| copy t ["&" thru ";" | to te | to end]
			(append res t)
	] ]
	either parse html rule [res] [false]
]
clean-html: function [
		{Cleans HTML:
		- removes tags but B,I,P,BR
		- fixes spaces
		- collects BRs}
		html [block!]
	] [
	spc: charset " ^/^-^M"
	spacer: [into[some spc] |"&nbsp;"]
	void: [<b> opt void </b> | <i> opt void </i>]
	tbi: [and tag! [into [
		opt "/" ["b"|"i"]
	] ] ]
	tbr: [and tag! [into [
		"br" opt " " opt "/"
	] ] ]
	res: copy []
	s: copy []
	t: none
	br: 0 sp: false
	rule: [ any [
		some [tbr (++ br)
			| spacer (sp: true)
			| copy t tbi (append s t)
			| tag!
		] (
			if br > 1 [append res <br>]
			either br > 0
				[append res <br>]
				[if sp [append res " "]]
			br: 0 sp: false
			parse s [ any [
				remove void | skip
			] ]
			append res s
			s: copy []
			)
		| set t string! (
				append res fix-spaces t
			)
	] ]
	either parse html rule [res] [false]
]
to-lit-js: function [b filter] [
	res: copy []
	s: copy ""
	t: none
	label: resp: none
	sal: 0
	h: copy []
	item: func [lab] [
		if ! lab [return none]
		if all[filter ! find filter lab]
			[lab: 'skip]
		if label [
			if label != 'skip
				[append res reduce[label s]]
			s: copy ""
		]
		either find h lab
			[label: 'skip]
			[label: lab append h lab]
	]
	caseitem: func [t] [
		case [
			find/case t "Ant" [ case [
				find/case t "1^^" "ant1"
				find/case t "2^^" "ant2"
				find/case t "3^^" "ant3"
				find/case t "TERZA" "ant3a"
				find/case t "SESTA" "ant6a"
				find/case t "NONA" "ant9a"
				find/case t "Ben" "ant_ben"
				find/case t "Magn" "ant_magn"
				find/case t "Ingresso" "ant_ingr"
				find/case t "Comunione" "ant_comun"
				true [case [
					find/case/part label "sal" 1
						[ajoin["ant" sal]]
					label == "nunc_dimittis"
						"ant_nunc"
					true "antifona"
				] ]
			] ]
			find t "lettura" [ case [
				find/case t "Prima" [resp: "1" "lettura1"]
				find/case t "Seconda" [resp: "2" "lettura2"]
				find/case t "TERZA" [resp: "" "lettura3a"]
				find/case t "SESTA" [resp: "" "lettura6a"]
				find/case t "NONA" [resp: "" "lettura9a"]
				true [resp: "" "lettura"]
			] ]
			find t "inno" [ case [
				find/case t "TERZA" "inno3a"
				find/case t "SESTA" "inno6a"
				find/case t "NONA" "inno9a"
				true "inno"
			] ]
			find/case t "Colletta" "colletta"
			find/case t "Canto al Vangelo" "vangelo"
			find/case t "Sulle Offerte" "offerte"
			find/case t "Dopo la Comunione" "comunione"
			find/case t "CANTICO" [ case [
				find/case t "VERGINE" "magnificat"
				find/case t "ZACCARIA" "benedictus"
				find/case t "SIMEONE" "nunc_dimittis"
				true [++ sal ajoin["sal" sal]]
			] ]
			find/case t "SALMO" [++ sal ajoin["sal" sal]] 
			find/case t "TE DEUM" "tedeum"
			find t "responsorio" [ajoin["responsorio" resp]]
			find/case t "Intercessioni" "intercessioni"
			find/case t "Invocazioni" "invocazioni"
			all [find ["intercessioni" "invocazioni"] label
					find/case t "Padre Nostro"]
				"padre_nostro"
			find t "orazione" [ case [
				label == "lettura3a" "orazione3a"
				label == "lettura6a" "orazione6a"
				label == "lettura9a" "orazione9a"
				true "orazione"
			] ]
			find/case t "Versetto" "versetto"
			find/case t "INVITATORIO" [sal: -1 "invitatorio"]
			find/case t "ESAME" "esame"
		] 
	]
	rule: [(item 'header) any[
		[ copy t [<b> thru </b>]
			(item caseitem ajoin t)
			| copy t to [<b> | end]
				(t: ajoin t)
		] (append s t)
	] (item 'skip) ]
	either parse b rule
	[res]
	[false]
]
;MAIN
args: system/options/args
prior: 99 filter: none
t0: t1: t2: t3: t4: 0:0:0
parse args [ any [
	into["p:" copy prior to end]
	| into[ "k:" copy filter to end (
		filter: split filter ","
		replace filter
			"letture" ["lettura1" "responsorio1" "lettura2" "responsorio2" "orazione"]
		replace filter
			"memoria" ["inno" "lettura" "lettura2" "responsorio" "responsorio2" "ant3a" "ant6a" "ant9a" "ant_ben" "ant_magn" "invocazioni" "intercessioni" "orazione"]
		replace filter
			"domenica" ["ant_ben" "ant_magn" "orazione"]
	) ]
	| set inp skip (
		oup: copy inp
		parse oup [some thru "." change to end "js"]
		print [inp "=>" oup]
		text: latin1-to-utf8 read to file! inp
		b: html-markup text
		b: clean-html b
		b: to-lit-js b filter
		oup: open/new to file! oup
		if b [
			forskip b 2 [
				either head? b
					[write oup ajoin["STACK.pop()({prior:" prior ",^/"]]
					[write oup ",^/"]
				write oup ajoin[quote b/1 ":" quote b/2]
			] write oup "^/})^/"
		]
		close oup
	)
] ]
print t0
print t1
print t2
print t3
