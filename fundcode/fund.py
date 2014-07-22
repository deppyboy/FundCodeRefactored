import scipy
import os
import pylab
import datetime
import streams
import scipy.optimize
import xlrd
import pyodbc

DELTACACHE = {}  # initialization of cache for cachedelta function
AVCACHE = {}  # initialization of cache for avcache function
FILENAME = 'H:\dat\PFUVFILE.TXT'  # location of fund data file
ORACLESTRING = streams.ORACLESTRING

def calcspxpct(evaldate):
    """
    Calculates the percentage of account value allocated to SPX as of evaldate.
    """
    cnxn = pyodbc.connect(ORACLESTRING)
    c = cnxn.cursor()
    sql = "SELECT Sum(CASH) as BILL, Sum(BOND) as BND, Sum(SMALL_CAP) as RTY, Sum(LARGE_CAP) as SPX, "
    sql += "Sum(INTERNATIONAL) as EAFE, Sum(FIXED) as FXD, Sum(DCA_PLUS) as DCA FROM ODSACT.ACT_RSL_SERIATIM WHERE "
    sql += "GENERATION_DATE=" + oracledatebuilder(evaldate) + " GROUP BY GENERATION_DATE;"
    c.execute(sql)
    row = c.fetchone()
    cnxn.close()
    return row.SPX / (row.BILL + row.BND + row.RTY + row.SPX + row.EAFE + row.FXD + row.DCA)

def getdelta(deltadate):
    """
    Calculates the delta for a 1% shock to the account value as of deltadate.
    """
    shocklocation = '\\\\anpdnas1\\chp_prod$\\Trading\Model\\' + str(deltadate.year) + '\\' + deltadate.strftime('%Y%m') + '\\' + deltadate.strftime('%Y%m%d') + '\\'
    files = os.listdir(shocklocation)
    filename = filter(lambda x: x[-3:] == 'xls', files)[0]
    wb = xlrd.open_workbook(shocklocation + filename)
    sht = wb.sheet_by_name('VAHA_Output')
    startval = 2 if sht.cell(0, 1).value == 'CSA' else 1
    upshock = sum([sht.cell(2, i).value for i in range(startval, startval + 6)])
    downshock = sum([sht.cell(3, i).value for i in range(startval, startval + 6)])
    spxdelta = -(upshock - downshock) / 2.0
    scalefactor = calcspxpct(deltadate)
    return spxdelta / scalefactor

def oracledatebuilder(mydate):
    """Converts a Python datetime to an Oracle date string."""
    return "TO_DATE('" + mydate.strftime('%Y%m%d') + "','yyyymmdd')"

def importdata(filename=FILENAME, dbstring=ORACLESTRING, baseonly=True):
    """
    Imports fund data into the Oracle database.
    
    Parameters
    ----------
    filename : string (default FILENAME)
        filename of text file to parse 
    dbstring : string (default ORACLESTRING)
        ODBC connection string for database
    baseonly : bool (default True)
        if True, only import the base NAV,
        and the NAV for the PNDY mnemonic;
        otherwise, import all records (slow)
    """
    conn = pyodbc.connect(dbstring)
    c = conn.cursor()
    c.execute('delete from funddata;')
    f = open(filename)
    row = f.readline()
    while row:
        if row[0] == 'U':
            company = int(row[1:4])
            mnemonic = row[4:12].strip()
            if mnemonic == '': mnemonic = 'BASENAV'
            date = datetime.datetime(int(row[12:16]), int(row[16:18]), int(row[18:20]))
            date = oracledatebuilder(date)
            fundcount = int(row[20:23])
            for i in range(0, fundcount):
                fundnum = int(row[23 + i * 12:23 + i * 12 + 3])
                nav = int(row[26 + i * 12:26 + i * 12 + 9]) / 1000000.0
                sql = "INSERT INTO funddata VALUES(%s, '%s', %s, %s, %s);" % (str(company), mnemonic, date, str(fundnum), str(nav))
                if baseonly and (mnemonic == 'BASENAV' or mnemonic == 'PNDY'):
                	c.execute(sql)
                elif not(baseonly):
					c.execute(sql)
        row = f.readline()
    conn.commit()
    conn.close()

def getfundmapping(fundnum, date):
    cnxn = pyodbc.connect(ORACLESTRING)
    c = cnxn.cursor()
    sql = 'SELECT * FROM ODSACT.ACT_SRC_FUND_MAPPING WHERE FUND_NO=' + str(fundnum) + ';'
    c.execute(sql)
    for row in c.fetchall():
        if date <= row.END_DATE and date >= row.START_DATE:
            return    {'TBILL'  : float(row.CASH),
                        'AGG'   : float(row.BOND),
                        'RTY'   : float(row.SMALL_CAP),
                        'SPX'   : float(row.LARGE_CAP),
                        'EAFE'  : float(row.INTERNATIONAL)}
    return None


def cachedelta(date):
    """Cached wrapper for getdelta."""
    if date in DELTACACHE.keys():
		return DELTACACHE[date]
    else:
        DELTACACHE[date] = getdelta(date)
        return DELTACACHE[date]

def avcache(funddate):
    """Cached function to get the total AV as of funddate"""
    if funddate in AVCACHE.keys():
        return AVCACHE[funddate]
    else:
        cnxn = pyodbc.connect(ORACLESTRING)
        cursor = cnxn.cursor()
        sql = "SELECT Sum(Fund_Val) AS FundTotal FROM ODSACT.ACT_PRC_CONTRACT_FUND_VALUE WHERE GENERATION_DATE="
        sql += oracledatebuilder(funddate) + ";"
        cursor.execute(sql)
        row = cursor.fetchone()
	if row:
            AVCACHE[funddate] = float(row[0])
	else:
	    AVCACHE[funddata] = 0.0
        cnxn.close()
        return AVCACHE[funddate]

class Fund:
    def __init__(self, fundcode, mapping=None, freq='D', forcedates=True):
        """
        Constructor for fund class.
        
        Parameters
        ----------
        fundcode : int
            internal code for fund
        mapping : dict (default None)
            if None, mapping is pulled from database,
            otherwise mapping can be specified.
        freq : char (default 'D')
            frequency of return calc for fund.
            'D' for daily, 'W' for weekly, 'M' for monthly
        forcedates : bool (default True)
            if changefreq != 'D' then forcedates will ensure
            that a return is reported on a weekly/monthly basis.
        
        Note
        ----
        Will dividend adjust the return stream.  You're welcome.
        """
        conn = pyodbc.connect(ORACLESTRING)
        c = conn.cursor()
        sql = "SELECT * from TDEES.NAVS WHERE fundnum=%s ORDER BY navdate;" % str(fundcode)
        c.execute(sql)
        rows = c.fetchall()
        dates = [row[1] for row in rows]
        navs = scipy.array([float(row[2]) for row in rows])
        divs = scipy.array([float(row[3]) for row in rows])
        self.stream = streams.ReturnStream(dates[:-1], dates[1:], navs[1:]/navs[:-1]+divs[1:]-1.0)
        if freq != 'D': self.stream = self.stream.changefreq(freq, forcedates=forcedates)
        self.freq = freq
        self.fundcode = fundcode
        self.plot = self.stream.plot
        conn.close()


    def project(self, mktbasket, mappingoverride = None):
    	"""
    	Projects the fund value based on the fund mappings for each period.

    	Parameters
    	----------
    	mktbasket : dict of Streams
    		market data
    	mappingoverride : None or mapping dictionary
    		mapping to use, or None if from db

    	Returns 
    	-------
    	act : ReturnStream
    		actual return stream
    	exp : ReturnStream
    		expected return stream
    	"""
        inputmatrix, fundreturns, indexes, daterange, dates = self.align(datetime.datetime(2000,1,1), datetime.datetime(2099,1,1), mktbasket)
        if inputmatrix is None: return None, None
        outdates, actuals, projecteds = [], [], []
        for i, dte in enumerate(dates):
            if not(mappingoverride):
                mapping = getfundmapping(self.fundcode, dte)
            else:
                mapping = mappingoverride
            if mapping:
                outdates.append(dte)
                actuals.append(fundreturns[i])
                weightarray = scipy.array([mapping[key] for key in indexes])
                projecteds.append(sum(weightarray*inputmatrix[i]))
        prior = outdates[0]-datetime.timedelta(days=1)
        return streams.ReturnStream([prior]+outdates[:-1], outdates, scipy.array(actuals).flatten()), \
            streams.ReturnStream([prior]+outdates[:-1], outdates, scipy.array(projecteds).flatten())


    def backtest(self, trainstart, trainend, backteststart, backtestend, mktbasket):
        """
        Regress and then backtest the regression.
        
        Parameters
        ----------
        trainstart : datetime
            beginning of regression period
        trainend : datetime
            end of regression period
        backteststart : datetime
            beginning of backtesting period
        backtestend : datetime
            end of backtesting period
        mktbasket : dict
            dictionary of market streams
            
        Returns
        -------
        dictionary of statistics for backtesting period
        """
        mapping = self.regress(trainstart, trainend, mktbasket)
        return self.stats(backteststart, backtestend, mktbasket, mappingoverride=mapping)     
   
    def av(self, date):
        """Returns the fund's AV for a given date."""
        cnxn = pyodbc.connect(ORACLESTRING)
        cursor = cnxn.cursor()
        sql = "SELECT Sum(Fund_Val) AS FundTotal FROM ODSACT.ACT_PRC_CONTRACT_FUND_VALUE WHERE GENERATION_DATE="
        sql += oracledatebuilder(date) + " AND GENERATION_TYPE='M' AND FUND_NO='" + str(self.fundcode) + "';"
        cursor.execute(sql)
        row = cursor.fetchone()
        cnxn.close()
        if row[0]:
            return float(row[0])
        else:
            return 0.0

    def deltaestimate(self, date):
        """Estimates the fund's delta on a date."""
        return cachedelta(date) * self.av(date) / avcache(date)

    def align(self, startdate, enddate, mktbasket):
        """
        Aligns the funds returns with the market returns
        to ensure that there are the same number of returns
        for all indices, and that they are properly aligned.
        
        Parameters
        ----------
        startdate : datetime
            beginning of alignment period
        enddate : datetime
            end of alignment period
        mktbasket : dict
            dictionary of market streams
        
        Returns
        -------
        inputmatrix : 2-d array
            matrix that holds the aligned
            index returns
        fundreturns : 1-d array
            holds the aligned fund returns
        indexes : list
            a list of the indexes in inputmatrix
        daterange : list
            beginning and end dates of the aligned
            data
        
        Note
        ----
        Will return None, None, None, None if
        no data is available between startdate and enddate.
        """
        hybridstream, indexes = [], []
        for index in mktbasket.keys():
            hybridstream.append(mktbasket[index][startdate:enddate])
            indexes.append(index)
        getdates = self.stream.overlap(hybridstream)
        if not(getdates):
            return None, None, None, None
        daterange = [getdates[0], getdates[-1]]
        fundreturns = self.stream.datereturns(getdates).returns
        indexreturns = [indexstream.datereturns(getdates).returns for indexstream in hybridstream]
        inputmatrix = scipy.vstack(indexreturns).T
        fundreturns = fundreturns.reshape(fundreturns.size, 1)
        return inputmatrix, fundreturns, indexes, daterange, getdates

    def regress(self, startdate, enddate, mktbasket):
        """
        Regresses a fund against the market indices.
        
        Parameters
        ----------
        startdate : datetime
            beginning of regression period
        enddate : datetime
            end of regression period
        mktbasket : dict
            dictionary of market streams
        
        Returns
        -------
        mapping : dict
            new mapping
        """
        inputmatrix, fundreturns, indexes, daterange, _ = self.align(startdate, enddate, mktbasket)
        if inputmatrix is None:
            return None
        def SSE(beta):
            return scipy.sum((scipy.dot(inputmatrix, beta.reshape(len(indexes), 1)) - fundreturns) ** 2.0)
        sumconstraint = lambda beta : 1.0 - sum(beta)
        guess = scipy.asarray([1.0] + [0.0] * (len(indexes) - 1))
        bounds = [(0.0, 1.0) for i in range(0, len(indexes))]
        finalbeta = scipy.optimize.fmin_slsqp(SSE, guess, eqcons=[sumconstraint], bounds=bounds, iprint=0, acc=1E-20)
        mapping = {}
        for i, idx in enumerate(indexes):
            mapping[idx] = finalbeta[i]
        return mapping

    def stats(self, startdate, enddate, mktbasket, avdate, output=False, mappingoverride=None):
        """
        Calculates statistics for a fund over a period.
        
        Parameters
        ----------
        startdate : datetime
            beginning of statistic period
        enddate : datetime
            end of statistic period
        mktbasket : dict
            dictionary of market streams
        output : bool
            if True, output results to db
        mappingoverride : None or mapping dictionary
        	whether to override the db mapping
        
        Returns
        -------
        stats : dict
            dictionary of statistics
        """
        actualstream, projstream = self.project(mktbasket, mappingoverride)
        if actualstream[startdate:enddate] is None: return None
        if projstream[startdate:enddate] is None: return None 
        actual = actualstream[startdate:enddate].returns
        projected = projstream[startdate:enddate].returns
        diff = actual - projected
        outdata = {
                 'TE'     : scipy.std(diff) * 100.0 * 100.0,
                 'BETA'   : scipy.cov(projected, actual, bias=1)[1, 0] / scipy.var(projected),
                 'ALPHA'  : (scipy.product(diff + 1.0)) ** (1.0 / diff.size) - 1.0,
                 'VOL'    : scipy.std(actual) * scipy.sqrt(252.0),
                 'PROJ'   : scipy.product(1.0 + projected) - 1.0,
                 'ACT'    : scipy.product(1.0 + actual) - 1.0,
                 'R2'     : 0.0 if scipy.all(actual == 0.0) else scipy.corrcoef(projected, actual)[1, 0] ** 2.0,
                 'AV'     : self.av(avdate),
                 'DELTA'  : self.deltaestimate(avdate)
                }
        outdata['DIFF'] = outdata['ACT'] - outdata['PROJ']
        outdata['PL'] = outdata['DELTA'] * outdata['DIFF'] * 100.0 
        if output:
            cnxn = pyodbc.connect(ORACLESTRING)
            cursor = cnxn.cursor()
            sql = 'INSERT INTO FUNDOUTPUT VALUES ({0!s},{1!s},{2!s},{3!s},{4!s},{5!s},{6},{7},{8!s},{9!s},{10!s},{11!s},{12!s},{13!s});'
            sql = sql.format(self.fundcode, outdata['PROJ'], outdata['ACT'], outdata['DIFF'],
                       outdata['DELTA'], outdata['PL'], oracledatebuilder(startdate),
                       oracledatebuilder(enddate), outdata['TE'], outdata['R2'], outdata['BETA'],
                       outdata['ALPHA'], outdata['VOL'], outdata['AV'])
            cursor.execute(sql)
            cnxn.commit()
            cnxn.close()
        return outdata
   
def graballandoutput(startdate, enddate, avdate):
    """
    Loads in all of the funds and runs statistics on them.
    Outputs results to the database.
    
    Parameters
    ----------
    startdate : datetime
        beginning of statistic period
    enddate : datetime
        end of statistic period
    mktbasket : dict
        dictionary of market streams
    """
    mktbasket = streams.getmarketdatadb()
    cnxn = pyodbc.connect(ORACLESTRING)
    cursor = cnxn.cursor()
    sql = 'delete from fundoutput;'
    cursor.execute(sql)
    cnxn.commit()
    sql = 'select fundnum from navs group by fundnum;'
    cursor.execute(sql)
    fundnums = [int(row[0]) for row in cursor.fetchall()]
    for fundnum in fundnums:
        print fundnum
        f = Fund(fundnum)
        f.stats(startdate, enddate, mktbasket, avdate, output=True)
    cnxn.close()
    
if __name__ == '__main__':
    startdt = datetime.datetime(2013,12,31)
    enddt = datetime.datetime(2014,3,31)
    graballandoutput(startdt, enddt, enddt)