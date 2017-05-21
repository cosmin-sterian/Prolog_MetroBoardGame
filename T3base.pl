

% width(-W) Lățimea spațiului de joc
width(8).
% height(-H) Înălțimea spațiului de joc
height(8).
% limits(-X0, -Y0, -W, -H) Limitele coordonatelor din joc
limits(1, 1, W, H) :- width(W), height(H).
% exterior(-X0E, -Y0E, -WE, -HE) Coordonatele elementelor exterioare
exterior(0, 0, WL, HL) :- width(W), WL is W + 1, height(H), HL is H + 1.

% center_space(?X, ?Y)
% Predicatul este adevărat pentru coordonatele spațiilor
% indisponibile din centrul hărții.
center_space(X, Y) :- width(W), height(H),
    (X is W / 2 ; X is W / 2 + 1),
    (Y is H / 2 ; Y is H / 2 + 1).

% tile(?DeltaE, ?DeltaS, ?DeltaW, ?DeltaN, ?ID)
% Predicatul este adevărat pentru o descriere a unei
% cărți cu identificatorul ID, conținând conexiunile
% descrise de cele patru valori delta. Un delta = 0
% înseamnă că conexiunea pleacă și ajunge pe aceeași latură;
% un delta = 1 înseamnă că conexiunea ajunge pe următoarea
% latură în sens trigonometric, etc.
tile(0, 0, 0, 0, '#1').
tile(0, 0, 1, 3, '#2').
tile(0, 1, 1, 2, '#3').
tile(0, 2, 0, 2, '#4').
tile(0, 2, 3, 3, '#5').
tile(1, 1, 1, 1, '#6').
tile(1, 2, 2, 3, '#7').
tile(1, 3, 1, 3, '#8').
tile(2, 2, 2, 2, '#9').
tile(3, 3, 3, 3, '#10').

% rotation(?Value, ?ID)
% Predicatul dă posibilele rotații și identificatorii lor.
rotation(0, 'R0').
rotation(1, 'R1').
rotation(2, 'R2').
rotation(3, 'R3').

% entry_point(?X, ?Y, ?Dir)
% Predicatul este adevărat pentru un punct de intrare, descris
% prin coordonatele **din afara hărții** de unde vine intrarea
% și prin direcția spre care merge intrarea (de exemplu,
% intrările de pe latura de sus a hărții vor avea
% direcția s (south).
entry_point(X, B, s) :- limits(A, _, W, _), exterior(B, _, _, _), between(A, W, X).
entry_point(X, B, n) :- limits(A, _, W, _), exterior(_, _, B, _), between(A, W, X).
entry_point(B, Y, e) :- limits(_, A, _, H), exterior(_, B, _, _), between(A, H, Y).
entry_point(B, Y, w) :- limits(_, A, _, H), exterior(_, _, _, B), between(A, H, Y).

% exit_point(?X, ?Y, ?Dir)
% Predicatul este adevărat pentru un punct de ieșire, descris
% prin coordonatele **din afara hărții** către care este
% ieșirea, și prin direcția spre care este ieșirea (de exemplu,
% ieșirile de pe latura de sus a hărții vor avea
% direcția n (north).
exit_point(X, B, n) :- limits(A, _, W, _), exterior(B, _, _, _), between(A, W, X).
exit_point(X, B, s) :- limits(A, _, W, _), exterior(_, _, B, _), between(A, W, X).
exit_point(B, Y, w) :- limits(_, A, _, H), exterior(_, B, _, _), between(A, H, Y).
exit_point(B, Y, e) :- limits(_, A, _, H), exterior(_, _, _, B), between(A, H, Y).
exit_point(X, Y, s) :- height(H), center_space(X, Y), Y =< H / 2.
exit_point(X, Y, n) :- height(H), center_space(X, Y), Y > H / 2.
exit_point(X, Y, e) :- width(W), center_space(X, Y), X =< W / 2.
exit_point(X, Y, w) :- width(W), center_space(X, Y), X > W / 2.






