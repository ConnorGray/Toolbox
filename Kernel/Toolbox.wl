BeginPackage["ConnorGray`Toolbox`"]

(* Declare your package's public symbols here. *)

StringStyleReplace

FileTreeDiagram

Begin["`Private`"]

Block[{$ContextPath = $ContextPath},
	(* For FileTreeCells *)
	Needs["DefinitionNotebookClient`"]
]

(******************************************************************************)

StringStyleReplace[string_?StringQ, rule : (Rule|RuleDelayed)[_, _]] :=
(* StringStyleReplace[string_?StringQ, rule : _] := *)
	StringStyleReplace[string, {rule}]

StringStyleReplace[string_?StringQ, rules : {(Rule|RuleDelayed)[_, _] ...}] := Module[{
	result
},
	result = StringReplace[string, rules];

	(* https://mathematica.stackexchange.com/questions/10990/how-to-join-two-styled-strings *)
	Replace[result, StringExpression[terms___] :> StringJoin[Map[
		Replace[term:Except[_?StringQ] :> ToString[term, StandardForm]],
		{terms}
	]]]
]

(******************************************************************************)

Options[FileTreeDiagram] = {
	Magnification -> Automatic,
	"IncludeHidden" -> False
}

FileTreeDiagram[{args___}, highlightQ : _ : (False &)] := Module[{
	cell, nbObj, image
},
	If[$VersionNumber < 13.0,
		Throw["FileTreeDiagram: requires v13.0 or newer."];
	];

	cell = DefinitionNotebookClient`FileTreeCells[args];

	rasterizeFileTreeCell[cell, Automatic]
]

(**************************************)

hiddenFileQ[path_?StringQ] := AnyTrue[FileNameSplit[path], StringStartsQ["."]];

FileTreeDiagram[
	root : (_?StringQ | File[_?StringQ]),
	(* TODO: Replace this with a ColorFunction option? *)
	highlightQ : Except[_?OptionQ] : (False &),
	OptionsPattern[]
] := Module[{
	paths,
	cells
},
	If[$VersionNumber < 13.0,
		Throw["FileTreeDiagram: requires v13.0 or newer."];
	];

	paths = FileNames[All, root, Infinity];

	paths = Map[
		path |-> FileNameDrop[path, Length[FileNameSplit[root]] - 1],
		paths
	];

	(* Filter out hidden ".name" files *)
	If[!TrueQ[OptionValue["IncludeHidden"]],
		paths = Select[
			paths,
			path |-> !hiddenFileQ[path]
		];
		,
		(* Force hidden files to render with the generic "file" icon. *)
		paths = Map[
			Replace[{
				path_?StringQ /; hiddenFileQ[path] :>
					<| "Path" -> path, "Type" -> File, "Icon" -> "" |>
			}],
			paths
		];
	];

	cells = DefinitionNotebookClient`FileTreeCells[
		paths,
		"IncludeStyles" -> True,
		"Interactive" -> False,
		"IncludeFooterCell" -> False,
		"CellGroupsOpen" -> True
	];

	cells = fixOrder[cells];

	(* Highlight the appropriate files. *)
	cells = cells /. {
		InterpretationBox[
			tagBox : TagBox[
				Alternatives[
					fileName_?StringQ,
					PaneBox[fileName_?StringQ, ___]
				] /; highlightQ[fileName],
				___
			],
			rest : PatternSequence[File[_], BaseStyle -> {"Text"}]
		] :> InterpretationBox[
			TemplateBox[{tagBox, FrameStyle -> None}, "Highlighted"],
			rest
		]
	};

	rasterizeFileTreeCell[cells, OptionValue[Magnification]]
]

(* Ensure that the PacletInfo.wl file comes at the top. *)
(* TODO: Add a more general mechanism for doing this kind of rearranging. *)
fixOrder[cells0_] := ReplaceAll[cells0, {
	CellGroupData[cells : {___Cell}, rest___] :> CellGroupData[
		Sort[
			(* Recurse *)
			fixOrder[cells],
			Function[{a, b}, Module[{aPath, bPath},
				aPath = Cases[a, InterpretationBox[_TagBox, File[path_], _] :> path, Infinity];
				bPath = Cases[b, InterpretationBox[_TagBox, File[path_], _] :> path, Infinity];

				aPath = Replace[aPath, {
					{} :> Return[0, Module],
					{first_, ___} :> first
				}];

				bPath = Replace[bPath, {
					{} :> Return[0, Module],
					{first_, ___} :> first
				}];

				(* If the paths are not at the same depth, we can't compare between them. *)
				If[Length[FileNameSplit[aPath]] =!= Length[FileNameSplit[bPath]],
					Return[0, Module];
				];

				(* Force PacletInfo files to sort to the top. *)
				If[StringMatchQ[FileNameTake[bPath], "PacletInfo." ~~ ("wl" | "m")],
					Return[-1, Module];
				];

				(* Force hidden files to sort to the end. *)
				If[AnyTrue[FileNameSplit[bPath], StringStartsQ["."]],
					Return[1, Module];
				];
				If[AnyTrue[FileNameSplit[aPath], StringStartsQ["."]],
					Return[-1, Module];
				];

				0
			]]
		],
		rest
	]
}]

(**************************************)

FileTreeDiagram[pacletObj_PacletObject, rest___] :=
	FileTreeDiagram[pacletObj["Location"], rest]

(**************************************)

rasterizeFileTreeCell[cell_, magnification_] := Module[{nbObj, image},
	nbObj = NotebookPut[Notebook[{cell}], Visible -> False];
	SetOptions[nbObj, WindowSize -> All];

	image = Rasterize[nbObj];

	NotebookClose[nbObj];

	(* https://mathematica.stackexchange.com/questions/8583/how-do-i-display-imported-images-at-actual-size *)
	Image[image, Magnification -> magnification]
]


End[] (* End `Private` *)

EndPackage[]
