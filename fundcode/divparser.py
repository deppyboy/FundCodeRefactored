import pyodbc
import datetime
from streams import ORACLESTRING

DIVFILE1 = 'H:\dat\VPAAUV.TXT'
FILENAME = 'H:\dat\PFUVFILE.TXT'
DIVFILE2 = 'N:\Technology\Project Services\Actuarial team\VPNAUV.TXT'

def importfunddata(ostring = ORACLESTRING, mne='BONE'):
    foundnavs = {}
    fundcodemap = {}
    cnxn = pyodbc.connect(ORACLESTRING)
    c = cnxn.cursor()
    c.execute('delete from navs;')
    outstring = ''
    f = open(DIVFILE1)
    row = f.readline()
    while row:
        date = row[:8]
        company = int(row[8:11])
        fundcode = row[11:17].strip()
        series = int(row[17:19])
        endnav = float(row[83:94])
        if company==101 and endnav!=0.0:
            if fundcode not in foundnavs.keys():
                foundnavs[fundcode] = []
            if date not in foundnavs[fundcode]:
                foundnavs[fundcode].append(date)
                if fundcode not in fundcodemap.keys():
                    c.execute("select * from divcode where systemmapping='%s'" % fundcode)
                    newrow = c.fetchone()
                    newcode = newrow[2]
                    fundcodemap[fundcode] = newcode
                else:
                    newcode = fundcodemap[fundcode]
                div = float(row[267:280])/100.0
                sql = "INSERT INTO NAVS VALUES (" + str(newcode) + ",TO_DATE("+date+",'yyyymmdd'),"+str(endnav)+','+str(div)+');'
                c.execute(sql)
        row = f.readline()
    f.close()
    f = open(DIVFILE2)
    row = f.readline()
    while row:
        date = row[:8]
        company = int(row[8:11])
        fundcode = row[11:17].strip()
        series = int(row[17:19])
        endnav = float(row[83:94])
        if company==101 and endnav!=0.0:
            if fundcode not in foundnavs.keys():
                foundnavs[fundcode] = []
            if date not in foundnavs[fundcode]:
                foundnavs[fundcode].append(date)
                if fundcode not in fundcodemap.keys():
                    c.execute("select * from divcode where systemmapping='%s'" % fundcode)
                    newrow = c.fetchone()
                    newcode = newrow[2]
                    fundcodemap[fundcode] = newcode
                else:
                    newcode = fundcodemap[fundcode]
                div = float(row[267:280])/100.0
                sql = "INSERT INTO NAVS VALUES (" + str(newcode) + ",TO_DATE("+date+",'yyyymmdd'),"+str(endnav)+','+str(div)+');'
                c.execute(sql)
        row = f.readline()

    c.commit()
    cnxn.close()
    
def parsefundcodes(ostring = ORACLESTRING):
    f = open(FILENAME)
    row = f.readline()
    cnxn = pyodbc.connect(ORACLESTRING)
    c = cnxn.cursor()
    c.execute('delete from divcode;')
    while row:
        #row[4:12].strip()=='' 
        if row[0]=='F' and row[15]=='V':
            comp = str(row[1:4])
            mnemonic = row[4:12].strip()
            fundcode = row[12:15].strip()
            newcode = row[16:22].strip()
            c.execute("INSERT INTO DIVCODE VALUES ('%s', %s, %s, '%s');" % (mnemonic, comp, fundcode, newcode) )
        row = f.readline()
    c.commit()
    cnxn.close()


if __name__=='__main__':
    parsefundcodes()
    importfunddata()