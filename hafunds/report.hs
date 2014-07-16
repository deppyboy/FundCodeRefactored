module Report where

import FundDB
import Streams
import Database.HDBC
import Database.HDBC.ODBC
import Data.Time
import Data.List
import Data.Maybe
import Control.Monad.Trans.Maybe

report :: Day -> Day -> IO ()
report stdt eddt = do
	conn <- connectODBC cstring
	let sql = "SELECT fund_no from odsact.act_src_fund_codes group by fund_no order by fund_no;"
	result <- runMaybeT $ prepareAndGrab conn sql
	let 
		fundlist = map ((\x->fromSql x :: Int) . head) (fromJust result)
		f x = reportfund stdt eddt x conn
	mapM_ f fundlist


reportfund :: IConnection conn => Day -> Day -> Int -> conn -> IO ()
reportfund stdt eddt fundnum conn = do
	putStrLn $ "Running report for #" ++ show fundnum
	let sql = "delete from quickreport where fundnum="++show fundnum++";"
	_ <- prepareAndExecute conn sql
	outdata <- runMaybeT $ getActExp conn fundnum
	case outdata of
		Just (a, e) -> do
			let
				aslice = getSlice a stdt eddt
				eslice = getSlice e stdt eddt
				trackerr = te aslice eslice
				aret = totalret aslice
				eret = totalret eslice
				vol = stdev aslice
				bta = beta aslice eslice
				bta' = if isNaN bta then 0 else bta
				sql' = "INSERT INTO QUICKREPORT VALUES("++show fundnum++","++
					intercalate "," (map show [aret, eret, aret-eret, trackerr, vol, bta']) ++ ");"
			prepareAndExecute conn sql'
		Nothing -> putStrLn "No data for fund."