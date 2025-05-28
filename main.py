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
def policz_dla_roznych_danych(O_t,Sigmas,S_0s,Rs,Ks,Ts,opcja,wersja):
    total = len(O_t) * len(Sigmas) * len(S_0s) * len(Rs) * len(Ks) * len(Ts)
    dane = np.zeros((total, 8))
    idx = 0
    for odw_t in O_t:
        for sigma in Sigmas:
            for S_0 in S_0s:
                for r in Rs:
                    for K in Ks:
                        for T in Ts:
                            result = policz(odw_t, sigma, S_0, r, K, T, opcja, wersja)
                            dane[idx, :] = np.array([
                                result[0, 0], odw_t, 1.0 / odw_t, sigma,
                                S_0, r, K, T
                            ])
                            idx += 1
    return dane




def policz_dane_i_zapisz(O_t,Sigmas,S_0s,Rs,Ks,Ts):
    df = pd.DataFrame(columns=['cena_opcji','odw_t','t','sigma','S_0','r','K','T','opcja','wersja'])
    for opcja in ["a","e"]:
        for wersja in ["call","put"]:
            df1 = pd.DataFrame(policz_dla_roznych_danych(O_t, Sigmas, S_0s, Rs, Ks, Ts,opcja,wersja),
                               columns=['cena_opcji','odw_t','t','sigma','S_0','r','K','T'])

            df1['opcja'] = opcja
            df1['wersja'] = wersja
            df = pd.concat([df,df1])
    nazwa = zrob_nazwe_do_danych(O_t,Sigmas,S_0s,Rs,Ks,Ts)
    df.to_csv(f"dane/{nazwa}", index=False)

O_t = np.array([12])
Sigmas = np.array([0.3])
S_0s = np.array([50])
Rs = np.array([0.02])
Ks = np.array([48])
Ts = np.array([2])


#policz_dane_i_zapisz(O_t, Sigmas, S_0s, Rs, Ks, Ts)



def policz_czas_liczenia_raz(odw_t,sigma,S_0,r,K,T,opcja,wersja):
    start = time.time()
    policz(odw_t,sigma,S_0,r,K,T,opcja,wersja)
    end = time.time()
    return end - start


def zbadaj_ile_liczy_srednio(O_t,sigma,S_0,r,K,T,opcja,wersja,N = 100):
    df = pd.DataFrame(columns=['sredni_czas', 'odw_t', 't', 'sigma', 'S_0', 'r', 'K', 'T', 'opcja', 'wersja'])
    policz_czas_liczenia_raz(O_t[0], sigma, S_0, r, K, T, opcja, wersja)
    for odw_t in O_t:
        sr = np.array([policz_czas_liczenia_raz(odw_t,sigma,S_0,r,K,T,opcja,wersja) for _ in range(N)])
        srednia = np.mean(sr)

        df1 = pd.DataFrame(columns=['sredni_czas', 'odw_t', 't', 'sigma', 'S_0', 'r', 'K', 'T', 'opcja', 'wersja'])
        df1.loc[0] = [srednia,odw_t,1/odw_t,sigma,S_0,r,K,T,opcja,wersja]
        df = pd.concat([df, df1])
    return df

def zbadaj_ile_liczy_srednio_wszystkie_mozliwosci(O_t,sigma,S_0,r,K,T,N = 100):
    df = pd.DataFrame(columns=['sredni_czas', 'odw_t', 't', 'sigma', 'S_0', 'r', 'K', 'T', 'opcja', 'wersja'])
    for opcja in ["a","e"]:
        for wersja in ["call","put"]:
            df1 = zbadaj_ile_liczy_srednio(O_t,sigma,S_0,r,K,T,opcja,wersja,N)
            df = pd.concat([df, df1])
    return df


def policz_delte(matrix_do_liczenia, cena_payoff, r, t, p, u, d, S_0, opcja):
    n = matrix_do_liczenia.shape[1] - 1
    delty = np.zeros((n+1, n+1))
    cash = np.zeros((n+1, n+1))
    q = 1 - p

    for i in np.arange(n-1, -1, -1): 
        for j in range(i+1):  # knots in given col
            V_u = matrix_do_liczenia[j, i+1]
            V_d = matrix_do_liczenia[j+1, i+1]
            
            S_curr = S_0 * (u ** j) * (d ** (i - j))
            S_u = S_curr * u
            S_d = S_curr * d
            
            delta = (V_u - V_d) / (S_u - S_d)
            B = exp(-r * t) * (p * V_u + q * V_d - delta * (p * S_u + q * S_d))
            
            # portfel replikującego
            wartosc_portfela = delta * S_curr + B

            if opcja == "e":
                matrix_do_liczenia[j, i] = wartosc_portfela
            else:
                matrix_do_liczenia[j, i] = max(wartosc_portfela, cena_payoff[j, i])

            delty[j, i] = delta
            cash[j, i] = B

    return matrix_do_liczenia, delty, cash

def policz_z_delta(odwr_t = 2, sigma = 0.3, S_0 = 50, r = 0.02, K = 48, T = 2, opcja = "a", wersja = "put"):
    t = 1 / odwr_t
    n = int(T * odwr_t)
    u = exp(sigma * sqrt(t))
    d = exp(-sigma * sqrt(t))
    p = (exp(r * t) - d) / (u - d)

    matrix_do_liczenia = np.zeros((n+1, n+1))
    cena_payoff = policz_payoff_w_kazdej_chwili(n, u, d, S_0, K, wersja)
    matrix_do_liczenia[:, -1] = cena_payoff[:, -1]

    matrix, delty, cash = policz_delte(matrix_do_liczenia, cena_payoff, r, t, p, u, d, S_0, opcja)
    dane = []
    for j in range(n+1):  # col ~ time
        czas = j * t
        for i in range(j+1):  # row ~ tree depth
            S = S_0 * (u ** i) * (d ** (j - i))
            dane.append({
                'czas': czas,
                'poziom': i,
                'S': S,
                'wartosc_opcji': matrix[i, j],
                'delta': delty[i, j],
                'cash': cash[i, j]
            })
    return pd.DataFrame(dane)

import matplotlib.pyplot as plt
import seaborn as sns

def narysuj_heatmape_delt(df):
    pivot = df.pivot(index="poziom", columns="czas", values="delta")
    plt.figure(figsize=(12, 6))
    sns.heatmap(pivot, cmap="coolwarm", center=0, annot=False, cbar_kws={'label': 'Delta'})
    plt.title("Mapa ciepła wartości ∆ (delta) w czasie i pozycjach drzewa")
    plt.xlabel("Czas (lata)")
    plt.ylabel("Poziom drzewa (liczba wzrostów)")
    plt.tight_layout()
    plt.show()
#print(policz_z_delta())
#print(zbadaj_ile_liczy_srednio_wszystkie_mozliwosci([100,500,1000],0.03,50,0.02,48,2,N = 4))
df = policz_z_delta(odwr_t=12, sigma=0.3, S_0=50, r=0.02, K=48, T=2, opcja="a", wersja="put")
narysuj_heatmape_delt(df)
