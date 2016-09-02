-module(squad).

-export([ new/3
	    , new/4
	    , subject/1
	    , subject/2
	    , predicate/1
	    , predicate/2
	    , object/1
	    , object/2
	    , label/1
	    , label/2
	    ]).

-type squad3() ::   
  #{
    subject   => iodata(),
    predicate => iodata(),
    object    => iodata()
   }.

-type squad4() ::   
  #{
    subject   => iodata(),
    predicate => iodata(),
    object    => iodata(),
    label     => iodata()
   }.

-type squad() :: squad3() | squad4().

-export_type([squad/0]).

-spec new(iodata(), iodata(), iodata()) -> squad3().
new(Subject, Predicate, Object) ->
  #{
    subject   => Subject,
    predicate => Predicate,
    object    => Object
   }.

-spec new(iodata(), iodata(), iodata(), iodata()) -> squad4().
new(Subject, Predicate, Object, Label) ->
  #{
    subject   => Subject,
    predicate => Predicate,
    object    => Object,
    label     => Label
   }.

%% getters
-spec subject(squad()) -> iodata().
subject(#{subject := Subject}) -> Subject.

-spec predicate(squad()) -> iodata().
predicate(#{predicate := Predicate}) -> Predicate.

-spec object(squad()) -> iodata().
object(#{object := Object}) -> Object.

-spec label(squad4()) -> iodata().
label(#{label := Label}) -> Label.

%% setters
-spec subject(squad(), iodata()) -> iodata().
subject(Squad, Subject) -> 
  Squad#{subject => Subject}.

-spec predicate(squad(), iodata()) -> iodata().
predicate(Squad, Predicate) ->
  Squad#{predicate => Predicate}.

-spec object(squad(), iodata()) -> iodata().
object(Squad, Object) -> 
  Squad#{object => Object}.

-spec label(squad4(), iodata()) -> iodata().
label(Squad, Label) -> 
  Squad#{label => Label}.

