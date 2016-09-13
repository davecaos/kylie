-module(squad).

-author("David Cesar Hernan Cao <david.c.h.cao@gmail.com>").
-github("https://github.com/davecaos").
-license("MIT").

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
        , nquads_file/1
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

-export_type([squad/0, squad3/0, squad4/0]).

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
-spec subject(squad(), iodata()) -> squad().
subject(Squad, Subject) -> 
  Squad#{subject => Subject}.

-spec predicate(squad(), iodata()) -> squad().
predicate(Squad, Predicate) ->
  Squad#{predicate => Predicate}.

-spec object(squad(), iodata()) -> squad().
object(Squad, Object) -> 
  Squad#{object => Object}.

-spec label(squad(), iodata()) -> squad4().
label(Squad, Label) -> 
  Squad#{label => Label}.

-spec nquads_file([squad()]) -> binary().
nquads_file(Squads) -> 
  list_to_binary(lists:map(fun relp_quads/1, Squads)).

-spec relp_quads(squad()) -> binary().
relp_quads(Squad) ->
  Subject = maps:get(subject, Squad, <<"null">>),
  Predicate = maps:get(predicate, Squad,<<"null">>),
  Object = maps:get(object, Squad, <<"null">>),
  Label = maps:get(label, Squad, <<"null">>),
  << Subject/binary
   , <<" ">>/binary
   , Predicate/binary
   , <<" ">>/binary
   , Object/binary
   , <<" ">>/binary
   , Label/binary
   , <<".\n">>/binary
  >>.
