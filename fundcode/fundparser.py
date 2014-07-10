import sqlite3,datetime

FILENAME = 'H:\dat\PFUVFILE.TXT'
SQLLITEDB = 'c:\\sqlite\\funddata2.db'

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

def rerun(sqllitedb=SQLLITEDB):
	conn = sqlite3.connect(sqllitedb)
	c = conn.cursor()
	c.execute("UPDATE runs SET finished='N'")
	conn.commit()
	conn.close()

def createallfundstable(sqllitedb=SQLLITEDB):
	cnxn = sqlite3.connect(sqllitedb)
	c = cnxn.cursor()
	try:
		sql = 'delete from allfunds;'
		c.execute(sql)
	except:
		sql = 'create table allfunds (company TEXT, mnemonic TEXT, fundnum REAL);'
		c.execute(sql)
	sql = 'select company, mnemonic, fundnum from funds group by company, mnemonic, fundnum;'
	c.execute(sql)
	for row in c.fetchall():
		sql = "insert into allfunds values (" + str(row[0]) + ",'" + row[1] + "'," + str(row[2]) +");"
		c.execute(sql)
	cnxn.commit()
	cnxn.close()
	
def getallfunds(sqllitedb=SQLLITEDB):
	conn = sqlite3.connect(sqllitedb)
	c = conn.cursor()
	sql = "SELECT company, mnemonic, fundnum FROM funds GROUP BY company, mnemonic, fundnum;"
	c.execute(sql)
	funds = []
	for row in c.fetchall():
		funds.append([row[0],row[1],row[2]])
	return funds

def grabunfinished(sqllitedb=SQLLITEDB,reverse=True):
	conn = sqlite3.connect(sqllitedb)
	c = conn.cursor()
	sql = "SELECT company, mnemonic, fundnum FROM runs WHERE finished='N' GROUP BY company, mnemonic, fundnum;"
	c.execute(sql)
	funds = []
	for row in c.fetchall():
		funds.append([row[0],row[1],row[2]])
	c.close()
	if reverse:
		funds.reverse()
	return funds
	
def createfinishedfunds(sqllitedb=SQLLITEDB):
	myfunds = getallfunds()
	conn = sqlite3.connect(sqllitedb)
	c = conn.cursor()
	try:
		c.execute('DROP TABLE runs;')
	except:
		pass
	sql = 'CREATE TABLE runs (company INTEGER, mnemonic TEXT, fundnum INTEGER, finished TEXT, spx REAL, rty REAL, eafe REAL, agg REAL, tbill REAL, regressstart TEXT, regressend TEXT, backteststart TEXT, backtestend TEXT, te REAL, beta REAL, r2 REAL, alpha REAL, freq TEXT);'
	c.execute(sql)
	for fund in myfunds:
		sql = "INSERT INTO runs VALUES(%s,'%s',%s,'N',0.0,0.0,0.0,0.0,0.0,'','','','',0.0,0.0,0.0,0.0,'')" % (str(fund[0]),fund[1],str(fund[2]))
		c.execute(sql)
	conn.commit()
	conn.close()	

def createfinishedfundsearliest(tablename='runs',sqllitedb=SQLLITEDB):
	conn = sqlite3.connect(sqllitedb)
	c = conn.cursor()
	try:
		c.execute('DROP TABLE '+tablename+';)')
	except:
		pass
	sql = 'CREATE TABLE '+tablename+' (company INTEGER, mnemonic TEXT, fundnum INTEGER, finished TEXT, spx REAL, rty REAL, eafe REAL, agg REAL, tbill REAL, regressstart TEXT, regressend TEXT, backteststart TEXT, backtestend TEXT, te REAL, beta REAL, r2 REAL, alpha REAL, freq TEXT);'
	c.execute(sql)
	sql = "SELECT fundnum, min(date) from funds where mnemonic<>'' group by fundnum;"
	c.execute(sql)
	for record in c.fetchall():
		sql = "select * from funds where fundnum=%s and date='%s';" % (str(record[0]),record[1])
		c.execute(sql)
		fund = c.fetchone()
		while fund[1]=="":
			fund=c.fetchone()
		sql = "INSERT INTO "+tablename+" VALUES(%s,'%s',%s,'N',0.0,0.0,0.0,0.0,0.0,'','','','',0.0,0.0,0.0,0.0,'')" % (str(fund[0]),fund[1],str(fund[3]))
		c.execute(sql)
	conn.commit()
	conn.close()	

def exportdata(date,sqllitedb=SQLLITEDB,outfile='out.csv'):
	conn = sqlite3.connect(sqllitedb)
	c=conn.cursor()
	sql = "SELECT * FROM funds WHERE date='%s';" % (date.strftime('%Y-%m-%d'))
	c.execute(sql)
	outstring = ""
	for row in c.fetchall():
		outstring += ",".join((str(row[0]),row[1],row[2],str(row[3]),str(row[4])))+"\n"
	f=open(outfile,'w')
	f.write(outstring)
	f.close()
	conn.close()
	