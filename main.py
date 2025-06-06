import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import numpy as np
import pandas as pd
from math import sqrt,exp
from numba import jit
from copy import deepcopy
import time
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.ticker as ticker

import sys

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
        return np.maximum(K-m,np.zeros((n + 1, n + 1)))

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

odwr_t = np.array([12])
sigma = np.array([0.3])
S_0 = np.array([50])
r = np.array([0.02])
K = np.array([48])
T = np.array([2])
#policz_dane_i_zapisz(np.array([12]), np.array([0.6]), np.array([50]),np.array([0.3]), np.array([48]), np.arange(1,20,1))



O_t = np.arange(2, 100, 2)
Sigmas = np.arange(0.1, 0.66, 0.05)
S_0s = np.arange(30, 80, 1)
Rs = np.arange(-0.03,0.2, 0.01)
Ks = np.arange(30, 80, 1)
Ts = np.arange(1,100,1)

# O_t = np.array([12,13])
# Sigmas = np.array([0.3,0.4])
# S_0s = np.array([50,51])
# Rs = np.array([0.02,0.03])
# Ks = np.array([48,49])
# Ts = np.array([2,3])



pojedyncze = [odwr_t,sigma,S_0,r,K,T]
wielokrotne = [O_t,Sigmas,S_0s,Rs,Ks,Ts]

##dla nowych Sigma
# co_liczymy = deepcopy(pojedyncze)
# co_liczymy[1] = Sigmas
# policz_dane_i_zapisz(co_liczymy[0], co_liczymy[1], co_liczymy[2], co_liczymy[3], co_liczymy[4], co_liczymy[5])


#dla wielu
# ===================================================================
#for i in [2,3,4,5]:
#    pass
#    co_liczymy = deepcopy(pojedyncze)
#    co_liczymy[1] = Sigmas
#    co_liczymy[i] = wielokrotne[i]
#    policz_dane_i_zapisz(co_liczymy[0], co_liczymy[1], co_liczymy[2], co_liczymy[3], co_liczymy[4], co_liczymy[5])

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

#N = 100
#df = zbadaj_ile_liczy_srednio_wszystkie_mozliwosci(np.arange(1,300,1),0.03,50,0.02,48,2,N = N)
#df.to_csv(f"dane/dlugosc_liczenia_N_{N}.csv", index=False)

# N = 1000
# jakie_badac = np.concatenate((np.arange(1,100,2),np.arange(100,500,10),np.arange(500,1000,50)))
# df = zbadaj_ile_liczy_srednio_wszystkie_mozliwosci(jakie_badac,0.03,50,0.02,48,2,N = N)
# df.to_csv(f"dane/dlugosc_liczenia_N_{N}.csv", index=False)


#<<<<<<< HEAD
#=======
# ==== hedging
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
            #delta = np.clip(delta, -1, 1)
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
    #print(delty)
    #sys.exit()
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


def narysuj_heatmape_delt(df):
    pivot = df.pivot(index="poziom", columns="czas", values="delta")
    plt.figure(figsize=(12, 6))
    ax = sns.heatmap(
        pivot,
        cmap="coolwarm",
        center=0,
        vmin=-1, vmax=1,  
        annot=False,
        cbar_kws={'label': 'Delta'}
    )
    ax.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{x:.1f}"))
    plt.title("Mapa ciepła wartości ∆ w czasie i lokacji na drzewie")
    plt.xlabel("Czas (Krok)")
    plt.ylabel("Poziom drzewa")
    plt.tight_layout()
    plt.savefig('test.png')
    plt.show()


def narysuj_porownanie_delt_call_put(odwr_t=12, sigma=0.3, S_0=50, r=0.02, K=48, T=2, opcja="a"):
    df_call = policz_z_delta(odwr_t=odwr_t, sigma=sigma, S_0=S_0, r=r, K=K, T=T, opcja=opcja, wersja="call")
    df_put = policz_z_delta(odwr_t=odwr_t, sigma=sigma, S_0=S_0, r=r, K=K, T=T, opcja=opcja, wersja="put")
    #print(df_call['delta'].to_string())
    #print(df_put['delta'].to_string())
    #sys.exit()
    pivot_call = df_call.pivot(index="poziom", columns="czas", values="delta")
    pivot_put = df_put.pivot(index="poziom", columns="czas", values="delta")
    pivot_put = pivot_put.iloc[:, :-1]
    pivot_call = pivot_call.iloc[:, :-1]

    fig, axs = plt.subplots(2, 1, figsize=(12, 10), sharex=True)

    # Wykres call
    sns.heatmap(pivot_call, cmap="coolwarm", center=0, vmin=-1, vmax=1,
                annot=False, cbar_kws={'label': 'Delta (call)'}, ax=axs[0])
    axs[0].set_title("Mapa ciepła ∆ – opcja CALL")
    axs[0].set_ylabel("Poziom drzewa")
    axs[0].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{x:.1f}"))

    # Wykres put
    sns.heatmap(pivot_put, cmap="coolwarm", center=0, vmin=-1, vmax=1,
                annot=False, cbar_kws={'label': 'Delta (put)'}, ax=axs[1])
    axs[1].set_title("Mapa ciepła ∆ – opcja PUT")
    axs[1].set_xlabel("Czas (Krok)")
    axs[1].set_ylabel("Poziom drzewa")
    axs[1].xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{x:.1f}"))

    plt.tight_layout()
    plt.savefig("wykres_delta.png")
    plt.show()



#policz_z_delta(odwr_t=24,wersja="call")
#policz_z_delta(wersja="put", opcja='a')
#narysuj_heatmape_delt(policz_z_delta(wersja="put"))
#narysuj_porownanie_delt_call_put()
#print(policz_z_delta())
#print(zbadaj_ile_liczy_srednio_wszystkie_mozliwosci([100,500,1000],0.03,50,0.02,48,2,N = 4))


# ============================================== NOWE FUNKCJE DO HEDGINGU ==============================================
def calculate_delta_matrix(S0, K, T, r, sigma, option_type="call", N=24):
    dt = T / N
    u = np.exp(sigma * np.sqrt(dt))
    d = np.exp(-sigma * np.sqrt(dt))
    p = (np.exp(r * dt) - d) / (u - d)
    
    stock_tree = np.zeros((N+1, N+1))
    for i in range(N+1):
        for j in range(i+1):
            stock_tree[j, i] = S0 * (u ** (i - j)) * (d ** j)
    
    option_tree = np.zeros_like(stock_tree)
    for j in range(N+1):
        if option_type == "call":
            option_tree[j, N] = max(stock_tree[j, N] - K, 0)
        else:
            option_tree[j, N] = max(K - stock_tree[j, N], 0)


    for i in range(N-1, -1, -1):
        for j in range(i+1):
            option_tree[j, i] = np.exp(-r * dt) * (
                p * option_tree[j, i+1] + (1 - p) * option_tree[j+1, i+1])

    delta_matrix = np.full_like(option_tree, np.nan)
    for i in range(N):
        for j in range(i+1):
            delta_matrix[j, i] = (
                option_tree[j, i+1] - option_tree[j+1, i+1]
            ) / (
                stock_tree[j, i+1] - stock_tree[j+1, i+1]
            )

    return delta_matrix[:N, :N]  
def plot_delta_heatmap(delta_matrix, _type = "CALL"):

    c_map = 'coolwarm'
    if _type == 'PUT':
        c_map = 'coolwarm_r'
     
    title = "Mapa ciepła ∆ - Opcja " + _type 
    plt.figure(figsize=(12, 6))
    sns.heatmap(delta_matrix, cmap=c_map, annot=False, fmt=".2f")
    plt.title(title)
    plt.xlabel("Czas (Krok)")
    plt.ylabel("Poziom drzewa")
    plt.show()
S0 = 50
K = 48
T = 2
r = 0.02
sigma = 0.3
N = 24  

opcyja = "CALL"

def heatmap_wartosc_akcji(odwr_t = 2, sigma = 0.3, S_0 = 50, r = 0.02, K = 48, T = 2, opcja = "a", wersja = "call"):
    t = 1 / odwr_t
    n = int(T * odwr_t)
    u = exp(sigma * sqrt(t))
    d = exp(-sigma * sqrt(t))
    p = (exp(r * t) - d) / (u - d)

    matrix_do_liczenia = np.zeros((n+1, n+1))
    cena_payoff = policz_payoff_w_kazdej_chwili(n, u, d, S_0, K, wersja)
    matrix_do_liczenia[:, -1] = cena_payoff[:, -1]
    matrix, delty, cash = policz_delte(matrix_do_liczenia, cena_payoff, r, t, p, u, d, S_0, opcja)
    plt.imshow(matrix,cmap='hot', interpolation='nearest')
    plt.colorbar()
    plt.title('mapa')
    plt.show()


#heatmap_wartosc_akcji(T=24)


#delta_mat = calculate_delta_matrix(S0, K, T, r, sigma, option_type='call', N=N)
#plot_delta_heatmap(delta_mat, opcyja)
def option_value_tree(S0, K, T, r, sigma, N=24, option_type="call", american=False):
    dt = T / N
    u = np.exp(sigma * np.sqrt(dt))
    d = np.exp(-sigma * np.sqrt(dt))
    p = (np.exp(r * dt) - d) / (u - d)
    
    # Drzewo cen akcji
    stock_tree = np.zeros((N+1, N+1))
    for i in range(N+1):
        for j in range(i+1):
            stock_tree[j, i] = S0 * (u ** (i - j)) * (d ** j)

    # Drzewo wartości opcji
    option_tree = np.zeros_like(stock_tree)
    for j in range(N+1):
        if option_type == "call":
            option_tree[j, N] = max(stock_tree[j, N] - K, 0)
        else:
            option_tree[j, N] = max(K - stock_tree[j, N], 0)

    # Wsteczna rekurencja
    for i in range(N-1, -1, -1):
        for j in range(i+1):
            hold = np.exp(-r * dt) * (
                p * option_tree[j, i+1] + (1 - p) * option_tree[j+1, i+1]
            )
            if option_type == "call":
                exercise = stock_tree[j, i] - K
            else:
                exercise = K - stock_tree[j, i]
            if american:
                option_tree[j, i] = max(hold, exercise)
            else:
                option_tree[j, i] = hold

    return option_tree[:N+1, :N+1]

def plot_option_value_heatmap(option_tree, title="Option Value Heatmap (Call)"):
    mask = np.full_like(option_tree, True, dtype=bool)
    for i in range(option_tree.shape[1]):
        mask[:i+1, i] = False  # tylko istniejące węzły odmaskować

    plt.figure(figsize=(12, 6))
    sns.heatmap(option_tree, mask=mask, cmap="viridis", annot=False)
    plt.title(title)
    plt.xlabel("Time step")
    plt.ylabel("Down moves")
    plt.show()

S0 = 50
K = 48
T = 2
r = 0.02
sigma = 0.3
N = 24
#plot_delta_heatmap(option_value_tree(50,48,2,0.02,0.3, american=True, option_type="call"))


import numpy as np
import math
import matplotlib.pyplot as plt
import seaborn as sns





def american_call_binomial_tree(S0, K, T, r, sigma, dt, _type="call", op ='A'):
    N = int(T / dt)
    u = np.exp(sigma * np.sqrt(dt))
    d = 1 / u
    q = (np.exp(r * dt) - d) / (u - d)
    discount = np.exp(-r * dt)

    S = np.zeros((N + 1, N + 1))
    V = np.zeros((N + 1, N + 1))

    for i in range(N + 1):
        for j in range(i + 1):
            S[j][i] = S0 * (u ** (i - j)) * (d ** j)

    for j in range(N + 1):
        if _type == 'put':
            V[j][N] = max(K - S[j][N], 0)  
        else:
            V[j][N] = max(S[j][N] - K, 0)

    for i in reversed(range(N)):
        for j in range(i + 1):
            continuation = discount * (q * V[j][i + 1] + (1 - q) * V[j + 1][i + 1])
            intrinsic = 0
            if _type == 'put':
                intrinsic = max(K-S[j][i], 0)
            else:
                intrinsic = max(S[j][i] - K, 0)
            
            if op == 'E':
                V[j][i] = continuation
            else:
                V[j][i] = max(intrinsic, continuation)
    return V

S0 = 50      
K = 48       
T = 2        
r = 0.02    
sigma = 0.3 
dt = 1/12    

V = american_call_binomial_tree(S0, K, T, r, sigma, dt, _type='put', op='E')
N = int(T / dt)


heatmap_data = np.full((N + 1, N + 1), np.nan)
for i in range(N + 1):      
    for j in range(i + 1):  
        heatmap_data[j, i] = V[j, i]


plt.figure(figsize=(12, 8))
sns.heatmap(
    heatmap_data,
    annot=True, 
    fmt=".2f",   
    cmap="viridis",
    cbar_kws={'label': 'Wartość opcji'},
    mask=np.isnan(heatmap_data)
)
plt.title("Heatmapa wartości europejskiej opcji PUT w czasie")
plt.xlabel("Czas (krok)")
plt.ylabel("Liczba spadków ceny (poziom w drzewie)")
#plt.gca().invert_yaxis()
plt.tight_layout()
plt.show()


