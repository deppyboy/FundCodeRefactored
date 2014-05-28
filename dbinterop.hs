module DBInterOp where

import Data.Time.Calendar
import Database.HDBC
import Database.HDBC.ODBC
import System.Locale
import Data.Time.Format
import Control.Monad
import YC
import HardCodes
import VolSurf

prepareAndGrab :: IConnection conn=>conn->String->IO [[SqlValue]]
prepareAndGrab conn sql = do
	preparedsql <- prepare conn sql
	execute preparedsql []
	fetchAllRows preparedsql

mesh :: [a]->[b]->[[(a,b)]]
mesh _ [] = []
mesh x (y:ys) = map (\b->(b,y)) x : mesh x ys

oracleDateBuilder :: FormatTime t=>t->String
oracleDateBuilder date = "TO_DATE('"++""++timerep++"','yyyymmdd')"
	where timerep = formatTime defaultTimeLocale "%Y%m%d" date

numReader :: SqlValue->Double
numReader x = if head a=='.' then read ('0':a) :: Double else read a :: Double
	where a = fromSql x :: String

loadYC :: FormatTime t=>t->IO (YieldCurve Double)
loadYC date = do
		conn <- connectODBC cstring
		rates <- mapM (getRateFromDB conn date) yccodes
		return $ stripcurvecubicparlogdisc ycterms rates
			where 
				getRateFromDB conn date ticker = do
					let sql = "SELECT LAST_RATE FROM ODSACT.ACT_PRC_BLM_LBR_SWP WHERE VALUATION_DT="
						++oracleDateBuilder date++" AND TICKER='"++ticker++"';"
					preparedsql <- prepare conn sql
					execute preparedsql  []
					outval <- fetchAllRows preparedsql
					return $ numReader (head $ head outval) / 100.0


getDivFromDB :: String-> --Index Name
		Day->    --Date
		IO (YieldCurve Double)
getDivFromDB idx date = do
		ioconn <- connectODBC cstring
		let sql = "SELECT maturity_date, implied_spot, market_discount_factor, market_forward \
			\ from ODSACT.ACT_PRC_MRKT_DLY_VLTY WHERE valuation_date="++oracleDateBuilder date++
			" and market_name='"++idx++"' group by maturity_date, implied_spot, \
			\ market_discount_factor, market_forward order by maturity_date ASC;"
		outval <- prepareAndGrab ioconn sql
		let allvals = map f outval
			where
				f (dt:spot:mdf:mktfwd:_) = (t, -log (fwd*disc/sp) / t)
					where 
						t = fromIntegral (diffDays mat date)/365.0
						mat = fromSql dt
						sp = numReader spot
						disc = numReader mdf
						fwd = numReader mktfwd
		return $ stripcurvecubicparlogdisc (map fst allvals) (map snd allvals)

getVolDates :: Connection->Day->IO [Day]
getVolDates ioconn date = do
	let sql = "SELECT maturity_date from ODSACT.ACT_PRC_MRKT_DLY_VLTY \
		\  WHERE valuation_date="++oracleDateBuilder date++
		" GROUP BY MATURITY_DATE;"
	outval <- prepareAndGrab ioconn sql
	let dates = map (fromSql . head) outval
	return dates


getVols :: String-> --Index Name
	   Day-> --Date
	   IO (VolSurf Double)
getVols idx date = do
	ioconn <- connectODBC cstring
	dates <- getVolDates ioconn date
	let meshes = mesh volstrikes dates
	let volfunc (strike, dt) = getVolByDateStrike dt strike
		where getVolByDateStrike mat strike = do
			let sql = "SELECT market_volatility_pct from ODSACT.ACT_PRC_MRKT_DLY_VLTY \
				  \ WHERE maturity_date="++oracleDateBuilder mat++
				  " and valuation_date="++oracleDateBuilder date++
				  " and market_name='"++idx++"' and strike_number="++show strike++";"
			preparedsql <- prepare ioconn sql
			execute preparedsql  []
			outval <- prepareAndGrab ioconn sql
			let vol = head $ map (numReader . head) outval
			return vol
	strikevols <- mapM (mapM volfunc) meshes
	let mats =  map (\x->fromIntegral (diffDays x date)/365.0) dates
	return $ VolSurf strikevols mats (map (\x->fromIntegral x / 100) volstrikes)
	

dt2013 = fromGregorian 2013 12 31
myyc = loadYC dt2013
mydiv = getDivFromDB "MSCI EAFE" dt2013
myvol = getVols "MSCI EAFE" dt2013
