#' @title Dynamic_Model
#'
#' @description
#' Calculation of cumulative chill according to the Dynamic Model
#'
#' This function calculates winter chill for temperate trees according to the
#' Dynamic Model.
#'
#' Chill Portions are calculated as suggested by Erez et al. (1990).
#'
#' @param HourTemp Vector of hourly temperatures in degree Celsius.
#' @param summ Boolean parameter indicating whether calculated metrics should
#' be provided as cumulative values over the entire record (TRUE) or as the
#' actual accumulation for each hour (FALSE).
#' @param A0 numeric. Parameter \eqn{A_0}{A0} of the dynamic model
#' @param A1 numeric. Parameter \eqn{A_1}{A1} of the dynamic model
#' @param E0 numeric. Parameter \eqn{E_0}{E0} of the dynamic model
#' @param E1 numeric. Parameter \eqn{E_1}{E1} of the dynamic model
#' @param slope numeric. Slope parameter for sigmoidal function
#' @param Tf numeric. Transition temperature (in degree Kelvin) for the
#' sigmoidal function.
#' @return Vector of length length(HourTemp) containing the cumulative Chill
#' Portions over the entire duration of HourTemp.
#' @author Eike Luedeling
#' @references Dynamic Model references:
#'
#' Erez A, Fishman S, Linsley-Noakes GC, Allan P (1990) The dynamic model for
#' rest completion in peach buds. Acta Hortic 276, 165-174
#'
#' Fishman S, Erez A, Couvillon GA (1987a) The temperature dependence of
#' dormancy breaking in plants - computer simulation of processes studied under
#' controlled temperatures. J Theor Biol 126(3), 309-321
#'
#' Fishman S, Erez A, Couvillon GA (1987b) The temperature dependence of
#' dormancy breaking in plants - mathematical analysis of a two-step model
#' involving a cooperative transition. J Theor Biol 124(4), 473-483
#' @keywords chill and heat calculation
#' @examples
#' weather<-fix_weather(KA_weather[which(KA_weather$Year>2006),])
#'
#' hourtemps<-stack_hourly_temps(weather,latitude=50.4)
#'
#' res <- Dynamic_Model(hourtemps$hourtemps$Temp)
#'
#' @export Dynamic_Model
Dynamic_Model <- function(HourTemp,
                          summ = TRUE,
                          E0 = 4153.5,
                          E1 = 12888.8,
                          A0 = 139500,
                          A1 = 2567000000000000000,
                          slope = 1.6,
                          Tf = 277)
{
  if(missing(HourTemp)) {
    stop("HourTemp must be present! Aborting.")
  }
  ## compute temperatures in Kelvin
  TK <- HourTemp + 273
  ## pre-compute some constants
  aa <- A0/A1
  ee <- E1-E0
  sr <- exp(slope*Tf*(TK-Tf)/TK)
  xi <- sr/(1+sr)
  xs <- aa*exp(ee/TK)
  eak1 <- exp(-A1*exp(-E1/TK))

  ## initialise PDBF with zero
  x=0
  for (l in c(2:length(HourTemp)))  {
    S <- x[l-1]
    if(x[l-1] >= 1) {
      S <- S*(1-xi[l-2])
    }
    x[l] <- xs[l-1]-(xs[l-1]-S)*eak1[l-1]
  }
  ## chill portions
  delta <- rep(0,length(HourTemp))
  ii <- which(x >= 1)
  delta[ii] <- x[ii]*xi[ii-1]
  if (summ) return(cumsum(delta))
  return(delta)
}
