



# funkcja pomocna, wykorzystywana w funkcji option_values_from_bopm()
# funkcja zwracajaca payoff opcji w momencie zapadalnosci (dla opcji amerykanskich i europejskich)
# dla opcji amerykanskich zwraca rowniez macierz z wartosciami aktywa bazowego w danym momencie czasu

option_payoff_function = function(n, u, S, K, option_type, option_style) {
  
  # funkcja zwracajaca maximum po kolejnych miejscach w wektorze
  # uzyta do obliczenia payoffow dla opcji w ostatnich wezlach
  vectored_max = function(x1, x2) { 
    return(max(x1, x2))
  }
  vectored_max = Vectorize(vectored_max)
  
  
  # w przypadku opcji europejskiej obliczenia w modelu NIE wymagaja znania
  # ceny aktywa bazowego w kazdym wezle, zwracamy wiec jedynie wartosci
  # z ostatnich wezlow, tj. z momentu zapadniecia opcji
  if (option_style == 'european') { 
  
    up_or_down_counter = seq(from = n, to = -n, by = -2) # liczba razy skoku ceny aktywa w gore/dol
    underlying_asset_price_at_n = S*u^up_or_down_counter # cena aktywa w momencie zapadalnosci
    
    if (option_type == 'call') {
      return(vectored_max(underlying_asset_price_at_n - K, numeric(n + 1)))
    }
    else  {
      return(vectored_max(K - underlying_asset_price_at_n, numeric(n + 1)))
    }
  }
  
  # w przypadku opcji amerykanskiej obliczenia w modelu wymagaja znania
  # ceny aktywa bazowego w kazdym wezle, zwracamy macierz wartosci opcji w kazdym wezle
  else {
    
    # macierz zawierajaca skoki ceny aktywa
    # jesli element w macierzy jest rowny i, to w tym wierzcholku skok o u^i,
    # jesli element w macierzy jest rowny -i, to w tym wierzcholku skok o u^(-i)
    underlying_asset_prices_changes_matrix = matrix(numeric((n + 1)^2), nrow = n + 1)
    
    for (i in 2:(n + 1)) {
      underlying_asset_prices_changes_matrix[1:i, i] = seq(from = i - 1, to = -(i - 1), by = -2)
    }
    underlying_asset_prices_matrix = S*u^underlying_asset_prices_changes_matrix
    
    if (option_type == 'call') {
      return(list(
        vectored_max(underlying_asset_prices_matrix[, n + 1] - K, numeric(n + 1)),
        underlying_asset_prices_matrix
        )
      )
    }
    else  {
      return(list(
        vectored_max(K - underlying_asset_prices_matrix[, n + 1], numeric(n + 1)),
        underlying_asset_prices_matrix
        )
      )
    }
  }
}



# funkcja dotyczaca modelu dwumianowego wyceny opcji

# parametryzowana przez:
# expiration_time - czas zapadniecia opcji (T, liczba naturalna), t - 'krok' w czasie (delta t, ulamek jednostki czasu (roku)),
# sigma - wspolczynnik zmiennosci ceny opcji (>0), S - cena spot aktywa bazowego,
# K - cena wykonania opcji, r - stopa procentowa bez ryzyka,
# option_type - typ opcji (call/put), option_styl - rodzaj opcji (european/american)

# dla opcji europejskiej zwraca macierz gornotrojkatna zawierajaca wyznaczone 
# wartosci opcji w danym wierzcholku, odpowiadajacy pewnej zmianie ceny aktywa bazowego

# dla opcji amerykanskiej zwraca macierz gornotrojkatna zawierajaca wyznaczone 
# wartosci opcji w danym wierzcholku, odpowiadajacy pewnej zmianie ceny aktywa bazowego
# oraz macierz decyzji, w ktorej wartosc 1 oznacza wierzcholek, ktory odpowiada momentowi
# w ktorym wykonanie opcji jest optymalne

option_values_from_bopm = function(expiration_time, t, sigma, S, K, r, option_type, option_style) {
  
  if ((option_type != 'call' & option_type != 'put') || (option_style != 'european' & option_style != 'american')) {
    print('Wrong option type (call/put only) or option style (european/american only)')
  }
  
  n = expiration_time/t # liczba 'kroków' w modelu
  u = exp(sigma*sqrt(t))
  p = (exp(r*t) - 1/u)/(u - 1/u)
  
  # macierz zawierajaca wartosci opcji wynikajace z modelu
  result_matrix = matrix(rep(NA, (n + 1)^2), nrow=(n + 1))
  
  # w przypadku opcji europejskiej nie mamy mozliwosci przedterminowego wykonania opcji;
  # wartosc w kazdym wezle jest wartoscia oczekiwana przyszlej wartosci opcji
  if (option_style == 'european') {

    result_matrix[, n + 1] = option_payoff_function(n, u, S, K, option_type, option_style)
    
    if (n >= 2) {
      for (i in n:1) {
        for (k in 1:i) {
          result_matrix[k, i] = exp(-r*t)*(p*result_matrix[k, i + 1] + (1 - p)*result_matrix[k + 1, i + 1])
        }
      }
    }
    
    else {
      result_matrix[1, 1] = exp(-r*t)*(p*result_matrix[1, 2] + (1 - p)*result_matrix[2, 2])
    }
    return(result_matrix)
  }
  
  
  # w przypadku opcji amerykanskiej mamy mozliwosc przedterminowego wykonania opcji;
  # wartosc w kazdym wezle jest maksimum z:
  # (1) wartosci oczekiwanej przyszlej wartosci opcji
  # (2) wartosci wewnetrznej opcji, tj. payoffu przy natychmiastowym wykonaniu
  if (option_style == 'american') {
    
    # lista zawierająca: (1) payoffy opcji w ostatnich wezlach, (2) macierz wartosci aktywa bazowego
    prices_and_payoffs_results = option_payoff_function(n, u, S, K, option_type, option_style)
    underlying_asset_prices_matrix = prices_and_payoffs_results[[2]]

    result_matrix[, n + 1] = prices_and_payoffs_results[[1]]
    
    # macierz zawierajaca informacje dotyczaca oplacalnosci przedwczesnego wykonania opcji
    # 1 jesli w danym momencie oplaca sie wykonac opcje, 0 w przeciwnym przypadku
    decision_matrix = matrix(rep(NA, (n + 1)^2), nrow=(n + 1))
    
    # opcja typu call, wartosc wewnetrzna opcji = max(S - K, 0)
    if (option_type == 'call') {
      if (n >= 2) {
        for (i in n:1) {
          for (k in 1:i) {
            result_matrix[k, i] = max(exp(-r*t)*(p*result_matrix[k, i + 1] + (1 - p)*result_matrix[k + 1, i + 1]), underlying_asset_prices_matrix[k, i] - K)
            
            # jesli warunek jest spelniony, to znaczy, ze opcje oplaca sie wykonac w danym momencie
            if (exp(-r*t)*(p*result_matrix[k, i + 1] + (1 - p)*result_matrix[k + 1, i + 1]) < underlying_asset_prices_matrix[k, i] - K) {
              decision_matrix[k, i] = 1
            }
            else {decision_matrix[k, i] = 0}
          }
        }
      }
      
      # jesli n == 1, nie rozwazamy przypadku przedterminowego wykonania opcji,
      # pomijamy macierz dotyczaca decyzji
      else {
        result_matrix[1, 1] = max(exp(-r*t)*(p*result_matrix[1, 2] + (1 - p)*result_matrix[2, 2]), S - K)
      }
    }
    
    # opcja typu put, wartosc wewnetrzna opcji = max(K - S, 0)
    else {
      if (n >= 2) {
        for (i in n:1) {
          for (k in 1:i) {
            result_matrix[k, i] = max(exp(-r*t)*(p*result_matrix[k, i + 1] + (1 - p)*result_matrix[k + 1, i + 1]), K - underlying_asset_prices_matrix[k, i])
            
            # jesli warunek jest spelniony, to znaczy, ze opcje oplaca sie wykonac w danym momencie
            if (exp(-r*t)*(p*result_matrix[k, i + 1] + (1 - p)*result_matrix[k + 1, i + 1]) < K - underlying_asset_prices_matrix[k, i]) {
              decision_matrix[k, i] = 1
            }
            else {decision_matrix[k, i] = 0}
          }
        }
      }

      # jesli n == 1, nie rozwazamy przypadku przedterminowego wykonania opcji,
      # pomijamy macierz dotyczaca decyzji
      else {
        result_matrix[1, 1] = max(exp(-r*t)*(p*result_matrix[1, 2] + (1 - p)*result_matrix[2, 2]), K - S)
      }
    }
    
    if (n == 1) {return(result_matrix)}
    else {return(list(
      result_matrix, 
      decision_matrix
      ))
    }
  }
}












