pairs_x_dict(Pairs0, Pairs1) :-
    dict_pairs(Dict0, d, Pairs0),
    %do stuff with Dict0
    dict_pairs(Dict0, d, Pairs1).
