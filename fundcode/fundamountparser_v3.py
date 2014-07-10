import pyodbc, datetime, csv, sqlite3
from fundparser import createallfundstable
import fund, streams
import os, xlrd

DBLOC = 'j:\\valuation\\output\\'
#PWD = 'tdees_2'
#DBNAME = 'OracleDB'
#ORACLESTRING = 'DSN=%s;PWD=%s' % (DBNAME, PWD)

FILENAME = 'H:\dat\PFUVFILE.TXT'
#SQLLITEDB = 'c:\\sqlite\\funddata2.db'
SQLLITEDB = 'funddata.db'

def importdata(filename=FILENAME,sqllitedb=SQLLITEDB):
    conn = sqlite3.connect(sqllitedb)
    c = conn.cursor()
    try:
        c.execute('DROP TABLE funds;')
    except:
        pass
    c.execute('CREATE TABLE funds (company INTEGER, mnemonic TEXT, date TEXT, fundnum INTEGER, nav REAL);')
    f = open(filename)
    row = f.readline()
    while row:
        if row[0]=='U':
            company = int(row[1:4])
            mnemonic = row[4:12].strip()
            date = row[12:16]+'-'+row[16:18]+'-'+row[18:20]
            fundcount = int(row[20:23])
            for i in range(0,fundcount):
                fundnum = int(row[23+i*12:23+i*12+3])
                nav=int(row[26+i*12:26+i*12+9])/1000000.0
                sql = "INSERT INTO funds VALUES(%s, '%s', '%s', %s, %s);" % (str(company), mnemonic, date, str(fundnum),str(nav))
                c.execute(sql)
        row = f.readline()
    c.execute('CREATE UNIQUE INDEX pkey on funds(company ASC,mnemonic ASC, fundnum ASC, date ASC);')
    conn.commit()
    conn.close()


def oracledatebuilder(mydate):
    return "TO_DATE('"+mydate.strftime('%Y%m%d')+"','yyyymmdd')"

def getmarketreturns(startdate,enddate):
    marketdata = streams.getmarketdatadb(ORACLESTRING)
    indexnames = ['SPX','AGG','EAFE','RTY','TBILL']
    returndict = {}
    for index in indexnames:
        start, end = marketdata[index].dayquote(startdate),marketdata[index].dayquote(enddate)
        returndict[index] = end/start-1.0
    return returndict

def calcspxpct(evaldate):
    cnxn = pyodbc.connect(ORACLESTRING)
    c = cnxn.cursor()
    sql = "SELECT Sum(AV_INDX4) as BILL, Sum(AV_INDX2) as BND, Sum(AV_INDX1) as RTY, Sum(AV_INDX0) as SPX, "
    sql += "Sum(AV_INDX3) as EAFE FROM ODSACT.act_rsl_vaha_inforce_fltr WHERE "
    sql += "GENERATION_DATE="+oracledatebuilder(evaldate)+" GROUP BY GENERATION_DATE;"
    c.execute(sql)
    row = c.fetchone()
    return row.SPX / (row.BILL + row.BND + row.RTY + row.SPX + row.EAFE)

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

"""
def getdelta(deltadate):
    #shockdblocation = DBLOC+deltadate.strftime('%Y%m%d')+"\\VAHAShocks_LiabilityValues.accdb"
    #shockdblocation = DBLOC+deltadate.strftime('%Y%m%d')+"\\VAHA_Prod_LOVAggregationTool.accdb"
    #cnxn = pyodbc.connect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=%s;" % shockdblocation)
    cnxn = pyodbc.connect(ORACLESTRING)
    c = cnxn.cursor()
    #sql = "SELECT ShockID_Ref_Prod.Code AS Code, Sum(VAHAOuputDBF_RunResults.rlov) AS rlov "
    #sql += "FROM ShockID_Ref_Prod INNER JOIN (VAHAInforceDBF_GroupLookupTable INNER JOIN "
    #sql += "VAHAOuputDBF_RunResults ON VAHAInforceDBF_GroupLookupTable.[Group Id] = VAHAOuputDBF_RunResults.group) "
    #sql += "ON ShockID_Ref_Prod.Rsn = VAHAOuputDBF_RunResults.rsn GROUP BY ShockID_Ref_Prod.Code;"
    sql = "select o.ws_out_run_shock_code as code, (o.ws_out_lblty_opt_val_grp_a+o.ws_out_lblty_opt_val_grp_b+o.ws_out_lblty_opt_val_grp_c+o.ws_out_lblty_opt_val_grp_d+o.ws_out_lblty_opt_val_grp_e) as rlov "
    sql += "from ODSACT.act_rsl_out_vaha o where o.etl_generation_date = "+oracledatebuilder(deltadate)+";"
    c.execute(sql)
    for row in c.fetchall():
        if row.CODE==101:
            deltaup = row.RLOV
        elif row.CODE==102:
            deltadown = row.RLOV
    spxdelta = -(deltaup-deltadown)/2.0
    scalefactor = calcspxpct(deltadate)
    return spxdelta/scalefactor"""
    
def getmappings(date):
    cnxn = pyodbc.connect(ORACLESTRING)
    c=cnxn.cursor()
    datestring = oracledatebuilder(date)
    sql = "SELECT * FROM ODSACT.ACT_SRC_FUND_MAPPING WHERE START_DATE<="+datestring
    sql += " AND END_DATE>="+datestring + ";"
    c.execute(sql)
    mappings,trans = {}, {}
    for row in c.fetchall():
        if row.CASH!=None:
            mappings[int(row.FUND_NO)] = {  'TBILL' : float(row.CASH),
                                            'AGG' : float(row.BOND),
                                            'RTY' : float(row.SMALL_CAP),
                                            'SPX' : float(row.LARGE_CAP),
                                            'EAFE' : float(row.INTERNATIONAL)}
            trans[int(row.FUND_NO)] = row.FUND_DESC
    return mappings,trans

def calchlfundperformance(startdate,enddate):
    delta = getdelta(startdate)
    returns = streams.getmarketdatadb()
    mappings,trans = getmappings(startdate)
    conn = sqlite3.connect(SQLLITEDB)
    c = conn.cursor()
    try:
        sql = "DELETE FROM fundperf WHERE startdate='"+startdate.strftime('%Y-%m-%d')+"' and enddate='"+enddate.strftime('%Y-%m-%d')+"';"
        c.execute(sql)
    except:
        sql = 'CREATE TABLE fundperf(mnemonic TEXT, fundnum INTEGER, startdate TEXT, enddate TEXT, fundname TEXT, fundamount REAL, actual REAL, expected REAL, diff REAL, delta REAL, pl REAL);'
        c.execute(sql)	
    sql = 'SELECT fundnum from funds group by fundnum;'
    c.execute(sql)
    funds = [int(row[0]) for row in c.fetchall()]
    for mynum in funds:
        print mynum
        if mynum in mappings.keys():
            myfund = fund.DivFund(mynum,mapping=mappings[mynum],freq='D')
            if myfund:
                stats = myfund.stats(startdate,enddate,returns,startdate)
                if stats:
                    actual = stats['ACT']
                    expected = stats['PROJ']
                    diff=actual-expected
                    myamt = stats['AV']
                    sql = "INSERT INTO fundperf VALUES('',"+str(mynum)+",'"+startdate.strftime('%Y-%m-%d')+"','"+enddate.strftime('%Y-%m-%d')+"','"+trans[mynum]+"',"+str(myamt)+","+str(actual)+","+str(expected)+","+str(diff)+","+str(stats['DELTA'])+","+str(stats['PL'])+");"
                    c.execute(sql)
    conn.commit()
    conn.close()


def outputall(filename='out.csv'):
    conn = sqlite3.connect(SQLLITEDB)
    c = conn.cursor()
    sql = 'select * from fundperf;'
    c.execute(sql)
    output = "Mnemonic,Fund Number,Start Date,End Date,Fund Name,AV,Actual,Expected,Diff,Delta,P&L\n"
    for row in c.fetchall():
        stringrow = [str(field) for field in row]
        output += ",".join(stringrow)+"\n"
    f = open(filename,'w')
    f.write(output)
    f.close()
    
datelist = [datetime.datetime(2012,12,31),
	datetime.datetime(2013,1,4),
	datetime.datetime(2013,1,11),
	datetime.datetime(2013,1,18),
	datetime.datetime(2013,1,25),
	datetime.datetime(2013,2,1),
	datetime.datetime(2013,2,8),
	datetime.datetime(2013,2,15),
	datetime.datetime(2013,2,22),
	datetime.datetime(2013,3,1),
	datetime.datetime(2013,3,8),
	datetime.datetime(2013,3,15),
	datetime.datetime(2013,3,22),
	datetime.datetime(2013,3,28),
	datetime.datetime(2013,4,5),
	datetime.datetime(2013,4,12),
	datetime.datetime(2013,4,19),
	datetime.datetime(2013,4,26),
	datetime.datetime(2013,5,3),
	datetime.datetime(2013,5,10),
	datetime.datetime(2013,5,17),
	datetime.datetime(2013,5,24),
	datetime.datetime(2013,5,31),
	datetime.datetime(2013,6,7),
	datetime.datetime(2013,6,14),
	datetime.datetime(2013,6,21),
	datetime.datetime(2013,6,28),
	datetime.datetime(2013,7,5),
	datetime.datetime(2013,7,12),
	datetime.datetime(2013,7,19),
	datetime.datetime(2013,7,26),
	datetime.datetime(2013,8,2),
	datetime.datetime(2013,8,9),
	datetime.datetime(2013,8,16),
	datetime.datetime(2013,8,23),
	datetime.datetime(2013,8,30),
	datetime.datetime(2013,9,6),
	datetime.datetime(2013,9,13),
	datetime.datetime(2013,9,20),
	datetime.datetime(2013,9,27),
	datetime.datetime(2013,9,30),
	datetime.datetime(2013,10,4),
	datetime.datetime(2013,10,11),
	datetime.datetime(2013,10,18),
	datetime.datetime(2013,10,25),
	datetime.datetime(2013,11,1),
	datetime.datetime(2013,11,8),
	datetime.datetime(2013,11,15),
	datetime.datetime(2013,11,22),
	datetime.datetime(2013,11,29),
	datetime.datetime(2013,12,6),
	datetime.datetime(2013,12,13),
	datetime.datetime(2013,12,20),
	datetime.datetime(2013,12,27),
	datetime.datetime(2013,12,31),
	datetime.datetime(2014,1,3),
	datetime.datetime(2014,1,10),
	datetime.datetime(2014,1,17),
	datetime.datetime(2014,1,24),
	datetime.datetime(2014,1,31),
	datetime.datetime(2014,2,7),
	datetime.datetime(2014,2,14),
	datetime.datetime(2014,2,21),
	datetime.datetime(2014,2,28),
	datetime.datetime(2014,3,7),
	datetime.datetime(2014,3,14),
	datetime.datetime(2014,3,21),
	datetime.datetime(2014,3,28),
	datetime.datetime(2014,3,31),
	datetime.datetime(2014,4,4),
	datetime.datetime(2014,4,11),
	datetime.datetime(2014,4,17),
	datetime.datetime(2014,4,25),
	datetime.datetime(2014,4,30),
	datetime.datetime(2014,5,2),
	datetime.datetime(2014,5,9),
	datetime.datetime(2014,5,16),
	datetime.datetime(2014,5,23),
	datetime.datetime(2014,5,30),
	datetime.datetime(2014,6,6),
	datetime.datetime(2014,6,13),
	datetime.datetime(2014,6,20),
	datetime.datetime(2014,6,27),
	datetime.datetime(2014,6,30),
	datetime.datetime(2014,7,3)]

datelist = [datetime.datetime(2014,4,30),datetime.datetime(2014,5,2)]
#initialdate = raw_input('Input start date (YYYYMMDD): ')
#enddate = raw_input('Input end date (YYYYMMDD): ')
#DBNAME = raw_input('Input Oracle DB name: ')
DBNAME = 'OracleDB'
PWD = 'tdees_6'
#PWD = raw_input('Input Oracle DB password: ')
dates = zip(datelist[:-1], datelist[1:])
ORACLESTRING = 'DSN=%s;PWD=%s' % (DBNAME, PWD)
strp = lambda k : datetime.datetime.strptime(k,'%Y%m%d')
#dates = [(strp(initialdate),strp(enddate))]
#importdata()
createallfundstable(SQLLITEDB)
for mydate in dates:
    print 'RUNNING: ' + mydate[1].strftime('%Y%m%d')
    calchlfundperformance(mydate[0],mydate[1])
outputall()
#print getmarketreturns(datetime.datetime(2013,11,29),datetime.datetime(2013,12,6))

