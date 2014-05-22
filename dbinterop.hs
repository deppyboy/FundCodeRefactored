module DBInterOp where

import Data.Maybe
import Data.Time.Calendar
import Database.HDBC
import Database.HDBC.ODBC
import System.Locale
import Data.Time.Format
import Control.Monad
import YC
import HardCodes

oracledatebuilder :: FormatTime t=>t->[Char]
oracledatebuilder date = "TO_DATE('"++""++timerep++"','yyyymmdd')"
	where timerep = formatTime defaultTimeLocale "%Y%m%d" date

numreader :: SqlValue->Double
numreader x = if head a=='.' then read ('0':a) :: Double else read a :: Double
	where a = fromSql x :: String

loadyc :: FormatTime t=>t->IO (YieldCurve Double)
loadyc date = do
		conn <- connectODBC cstring
		rates <- mapM (\x->getratefromdb conn date x) yccodes
		return $ stripcurvecubicparlogdisc ycterms rates
			where 
				getratefromdb conn date ticker = do
					let sql = "SELECT LAST_RATE FROM ODSACT.ACT_PRC_BLM_LBR_SWP WHERE VALUATION_DT="
						++oracledatebuilder date++" AND TICKER='"++ticker++"';"
					preparedsql <- prepare conn sql
					execute preparedsql  []
					outval <- fetchAllRows preparedsql
					return $ (numreader $ head $ head $ outval) / 100.0


getdivfromdb :: [Char]->Day->IO (YieldCurve Double)
getdivfromdb idx date = do
		ioconn <- connectODBC cstring
		let sql = "SELECT maturity_date, implied_spot, market_discount_factor, market_forward \
			\ from ODSACT.ACT_PRC_MRKT_DLY_VLTY WHERE valuation_date="++oracledatebuilder date++
			" and market_name='"++idx++"' group by maturity_date, implied_spot, \
			\ market_discount_factor, market_forward order by maturity_date ASC;"
		preparedsql <- prepare ioconn sql
		execute preparedsql  []
		outval <- fetchAllRows preparedsql
		let allvals = map f outval
			where
				f (dt:spot:mdf:mktfwd:_) = (t, -(log $ fwd*disc/sp) / t)
					where 
						t = (fromIntegral $ diffDays mat date)/365.0
						mat = fromSql dt
						sp = numreader spot
						disc = numreader mdf
						fwd = numreader mktfwd
		return $ stripcurvecubicparlogdisc (map fst allvals) (map snd allvals)


myyc = loadyc $ fromGregorian 2013 12 31
mydiv = getdivfromdb "MSCI EAFE" $ fromGregorian 2013 12 31

