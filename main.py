import numpy as np
import pandas as pd
from math import sqrt,exp
from numba import jit
import time




@jit
def policz_payoff_w_kazdej_chwili(n,u,d,S_0,K,wersja):
    #liczenie ceny w kazdej chwili
    do_u = np.zeros((n + 1, n + 1))
    do_d = np.zeros((n + 1, n + 1))
    for i in np.arange(n , 0, -1):
        do_u[:(i+1),i] = np.arange(i,-1,-1)
        do_d[:(i+1),i] = np.arange(i+1)
    m = np.triu(S_0*u**do_u*(d**do_d))

    # liczenie payoff
    if wersja == "call":
        return np.maximum(m-K,np.zeros((n + 1, n + 1)))
    else:
        return np.triu(np.maximum(K-m,np.zeros((n + 1, n + 1))))

@jit
def policz_cala_maciez(matrix_do_licz,c_p,r,t,p,opcja):
    q = 1-p
    n = matrix_do_licz.shape[1]
    if opcja == "e":
        for i in np.arange(n-1,0,-1):
            matrix_do_licz[:i, i-1] = exp(-r*t)*(p*matrix_do_licz[:i,i]+
                                                  q*matrix_do_licz[1:(i+1), i])
    else:
        for i in np.arange(n-1,0,-1):
            S =  exp(-r*t)*(p*matrix_do_licz[:i,i]+q*matrix_do_licz[1:(i+1), i])
            matrix_do_licz[:i, i-1] = np.maximum(S , c_p[:i, i-1])

#opcja "a" lub "e"
#wesrja "put" lub "call"
@jit
def policz(odwr_t = 2,sigma = 0.3,S_0 = 50,r = 0.02,K = 48,T = 2,opcja = "a",wersja = "put"):
    t = 1/odwr_t
    n = T*odwr_t
    u = exp(sigma*sqrt(t))
    d = exp(-sigma*sqrt(t))
    p = 0.5 ## jak to liczyc!!!!
    matrix_do_liczenia = np.zeros((n+1,n+1))

    ##liczenie payoffuw  kazdej chwili
    cena_payoff = policz_payoff_w_kazdej_chwili(n,u,d,S_0,K,wersja)
    matrix_do_liczenia[:,-1] = cena_payoff[:,-1]



    policz_cala_maciez(matrix_do_liczenia,cena_payoff,r,t,p,opcja)
    do_usyniecia_ostarnej_kolumny = np.zeros((n+1,n+1))+1
    do_usyniecia_ostarnej_kolumny[:, -1] = 0
    temp = do_usyniecia_ostarnej_kolumny*(np.triu(matrix_do_liczenia==cena_payoff))
    gdzie_w_am_od_razu_wykonujemy = temp*cena_payoff!=0

    print(matrix_do_liczenia)
    print(cena_payoff)


policz(opcja = "e")
policz(opcja = "a")



