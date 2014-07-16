module FundDB where

import Database.HDBC
import Data.Time.Format
import System.Locale
import Data.Time
import Data.List
import Streams
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

cstring :: String
cstring = "DSN=OracleDB;PWD=tdees_6"

prepareAndGrab :: IConnection conn => conn -> String -> MaybeT IO [[SqlValue]]
prepareAndGrab conn sql = do
	preparedsql <- lift $ prepare conn sql
	_ <- lift $ execute preparedsql []
	out <- lift $ fetchAllRows preparedsql
	case out of
		[] -> fail "empty"
		_ -> return out

prepareAndExecute :: IConnection conn => conn -> String -> IO ()
prepareAndExecute conn sql = do
	preparedsql <- prepare conn sql
	_ <- execute preparedsql []
	commit conn

oracleDateBuilder :: FormatTime t => t -> String
oracleDateBuilder date = "TO_DATE('"++""++timerep++"','yyyymmdd')"
	where timerep = formatTime defaultTimeLocale "%Y%m%d" date

numReader :: SqlValue -> Double
numReader x = if head a=='.' then read ('0':a) :: Double else read a :: Double
	where a = fromSql x :: String

loadFund :: IConnection conn => conn -> Int -> MaybeT IO ReturnStream
loadFund conn fundnum = do
		let sql = "SELECT NAVDATE, NAVVAL, DIV FROM TDEES.NAVS WHERE FUNDNUM="++show fundnum++ " ORDER BY NAVDATE;"
		outdata <- prepareAndGrab conn sql
		let (dt, quotes, divs) = unzip3 [(fromSql a :: Day, numReader b, numReader c) | [a,b,c] <- outdata]
		return $ streamFromNAVDivs dt quotes divs
		

getMarketIndexes :: IConnection conn => conn -> MaybeT IO [MarketData]
getMarketIndexes conn = do
	let sql = "SELECT VALUATION_DT, SPTR, LBUSTRUU, RU20INTR, GDDUEAFE, CASH "++
				" FROM ODSACT.ACT_RSL_EQTY_PRICE_HIST ORDER BY VALUATION_DT;"
	outvals <- prepareAndGrab conn sql
	let
		vals = [(fromSql a :: Day, numReader b, numReader c, numReader d,
				 numReader e, numReader f) | [a,b,c,d,e,f] <- outvals]
		(dates, spx, agg, rty, eafe, cash) = unzip6 vals
		outtext = ["spx", "agg", "rty", "eafe", "cash"]
	return [MarketData name (streamFromNAVs dates returns) | 
	         (name,returns) <- zip outtext [spx, agg, rty, eafe, cash]]
	

getMapping :: IConnection conn => conn -> Day -> Int -> MaybeT IO [Mapping]
getMapping conn date fundnum = do
	let 
		dte = oracleDateBuilder date
		sql = "SELECT CASH, BOND, SMALL_CAP, LARGE_CAP, INTERNATIONAL " ++
			"FROM ODSACT.ACT_SRC_FUND_MAPPING WHERE FUND_NO="++show fundnum ++
			" AND START_DATE<="++dte++" AND END_DATE>="++dte++";"
	outvals <- prepareAndGrab conn sql
	let
		strings = ["cash", "agg", "rty", "spx", "eafe"]
		weights = map numReader $ head outvals
	return [Mapping st wt | (st, wt) <- zip strings weights]
			

projStream :: IConnection conn => conn -> Int -> MaybeT IO ReturnStream
projStream conn fundnum = do
	funddata <- loadFund conn fundnum
	marketdata <- getMarketIndexes conn
	let
		oldates = overlapDates $ funddata : map indexStream marketdata
		proj (d1:d2:dts) (m:ms) = createProjected marketdata d1 d2 m : proj (d2:dts) ms
		proj _ _ = []
	mappings <- mapM (\x->getMapping conn x fundnum) $ tail oldates
	let rets = proj oldates mappings
	return $ ReturnStream (init oldates) (tail oldates) rets

getActExp :: IConnection conn => conn -> Int -> MaybeT IO (ReturnStream, ReturnStream)
getActExp conn num = do
	actual <- loadFund conn num
	expected <- projStream conn num
	return (actual, expected)
		