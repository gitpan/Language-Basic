10 REM Language::Basic test program
30 DATA 10, 12, 20, 27
40 DATA 15, 14, 11, 27, 2, 6, 7
50 DATA 2, 15, 23, 29, 6, 7, 39
60 DIM b(20)
90 p$ = "The Perl Journal just can't kill BASIC, huh?"
100 for i = 1 to 3
110 on i gosub 200, 220, 240
120 for j = 1 to l
125 read b(j)
130 c$ = c$ + mid$(p$,b(j),1)
140 next j
150 if i = 2 then gosub 260
160 c$ = c$ + " "
170 next i
180 print c$
190 end
200 l = 4
210 return
220 l = 7
230 return
240 l = 7
250 return
260 for q = 33 to 38
270 c$ = c$ + mid$(p$,q,1)
280 next q
290 return
