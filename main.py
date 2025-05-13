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
    p = (exp(r*t)-d)/(u-d)
    matrix_do_liczenia = np.zeros((n+1,n+1))

    ##liczenie payoffuw  kazdej chwili
    cena_payoff = policz_payoff_w_kazdej_chwili(n,u,d,S_0,K,wersja)
    matrix_do_liczenia[:,-1] = cena_payoff[:,-1]



    policz_cala_maciez(matrix_do_liczenia,cena_payoff,r,t,p,opcja)
    do_usyniecia_ostarnej_kolumny = np.zeros((n+1,n+1))+1
    do_usyniecia_ostarnej_kolumny[:, -1] = 0
    temp = do_usyniecia_ostarnej_kolumny*(np.triu(matrix_do_liczenia==cena_payoff))
    gdzie_w_am_od_razu_wykonujemy = temp*cena_payoff!=0 # gdzie uzywamy opcji amerykanskiej

    return matrix_do_liczenia


#policz(opcja = "e")
#policz(opcja = "a")

def zrob_nazwe_do_danych(O_t  = [2,3],Sigmas = [0.3,0.35],S_0s = [50,51],Rs = [0.02,0.03],Ks = [48,49],Ts = [2,3]):
    nazwa = "dane_"
    if min(O_t)==max(O_t):
        nazwa = nazwa+f"odwr_t_{min(O_t)}_"
    else:
        nazwa = nazwa + f"odwr_t_od_{min(O_t)}_do_{max(O_t)}_"

    if min(Sigmas)==max(Sigmas):
        nazwa = nazwa+f"sigma_{min(Sigmas)}_"
    else:
        nazwa = nazwa + f"sigma_od_{min(Sigmas)}_do{max(Sigmas)}_"

    if min(S_0s)==max(S_0s):
        nazwa = nazwa+f"S_0_{min(S_0s)}_"
    else:
        nazwa = nazwa + f"S_0_od_{min(S_0s)}_do_{max(S_0s)}_"

    if min(Rs)==max(Rs):
        nazwa = nazwa+f"r_{min(Rs)}_"
    else:
        nazwa = nazwa + f"r_od_{min(Rs)}_do_{max(Rs)}_"

    if min(Ks)==max(Ks):
        nazwa = nazwa+f"K_{min(Ks)}_"
    else:
        nazwa = nazwa + f"K_od_{min(Ks)}_do_{max(Ks)}_"

    if min(Ts)==max(Ts):
        nazwa = nazwa+f"T_{min(Ts)}_"
    else:
        nazwa = nazwa + f"T_od_{min(Ts)}_do_{max(Ts)}_"

    nazwa = nazwa+".csv"
    return nazwa

@jit
def policz_dla_roznych_danych(O_t  = [2,3],Sigmas = [0.3,0.35],S_0s = [50,51],Rs = [0.02,0.03],
                              Ks = [48,49],Ts = [2,3],opcja = "a",wersja = "put"):
    dane = np.empty((0,8))
    for odw_t in O_t:
        for sigma in Sigmas:
            for S_0 in S_0s:
                for r in Rs:
                    for K in Ks:
                        for T in Ts:
                            newrow = np.zeros((1,8))
                            newrow[0,:] = np.array([policz(odw_t,sigma,S_0,r,K,T,opcja,wersja)[0,0],odw_t,1/odw_t,sigma,
                                      S_0,r,K,T])
                            dane = np.append(dane, newrow, axis=0)
    return dane


print(policz_dla_roznych_danych())