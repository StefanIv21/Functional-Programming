
vmpoints(Test, Points):-
        member(Test:Points,
               [
                        % teste de la etapa 1
                 at:0
               , atL:0
               , c2:0
               , ccw:0
               , rot:0
               , match:0
               , findrot:0

                        % etapa 2
               , setget:25
               , limits:10
               , canPlaceA:10
               , canPlace1:10
               , canPlace2:10
               , avPos:30
               , findPos:25
                    ]).
