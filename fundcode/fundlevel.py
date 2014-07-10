import fund
import streams
import pyodbc
import numpy
import datetime

PORTOPTS = [972,973,974,975,976]


def calcdata(fundnum, startdate, enddate):
    cnxn = pyodbc.connect(streams.ORACLESTRING)
    c = cnxn.cursor()
    f = fund.DivFund(fundnum)
    mktbasket = streams.getmarketdatadb()
    actual, proj = f.project(mktbasket)
    actret = actual[startdate:enddate].returns
    projret = proj[startdate:enddate].returns
    diffstream = actual[startdate:enddate]-proj[startdate:enddate]
    basete  = numpy.var(diffstream.returns)
    baseact = numpy.product(1.0+actret)-1.0
    baseproj = numpy.product(1.0+projret)-1.0
    baseperf = baseact-baseproj
    c.execute('delete from portoptoutput where portopt='+str(fundnum)+';')
    sql = 'insert into portoptoutput values(%s, 0, %s, 1.0, %s, %s, 1.0);' % \
        (str(fundnum), str(basete), str(baseact), str(baseproj))
    c.execute(sql)

    sql = 'SELECT subfund from weights where portopt='+str(fundnum)+' group by subfund order by subfund;'
    c.execute(sql)
    subfunds = [row[0] for row in c.fetchall()]
    for subfund in subfunds:
        if subfund not in (1000, 1001):
            subact, subproj = buildreturns(subfund, fundnum, startdate, enddate, mktbasket)
            sactret, sprojret = [x.returns for x in [subact, subproj]]
            sdiffstream = subact-subproj
            oldates = diffstream.overlap([sdiffstream])
            diffol, sdiffol = diffstream.datereturns(oldates).returns, sdiffstream.datereturns(oldates).returns
            adjte = numpy.cov(diffol, sdiffol, bias = 1)[1,0] / numpy.var(diffol)
            percentexp = 1.0-adjte/basete
            adjactual = numpy.product(1.0+subact.returns)-1.0
            adjproj = numpy.product(1.0+subproj.returns)-1.0
            retexp = 1.0-(adjactual-adjproj)/baseperf
            sql = 'insert into portoptoutput values(%s, %s, %s, %s, %s, %s, %s);' % \
                (str(fundnum), str(subfund), str(adjte),str(percentexp), str(adjactual), str(adjproj), str(retexp))
            c.execute(sql)
    c.commit()
    cnxn.close()

def buildreturns(subfundnum, portoptnum, startdate, enddate, basket):
    cnxn = pyodbc.connect(streams.ORACLESTRING)
    c = cnxn.cursor()
    actual, proj = fund.DivFund(subfundnum).project(basket)
    startdates = actual[startdate:enddate].startdates
    enddates = actual[startdate:enddate].enddates
    actrets = actual[startdate:enddate].returns
    projrets = proj[startdate:enddate].returns
    actadj, projadj = [], []
    for enddate, actret, projret in zip(enddates, actrets, projrets):
        sql = "select weight from weights where period='"+enddate.strftime('%Y-%m')+\
            "' and subfund="+str(subfundnum)+" and portopt="+str(portoptnum) + ";"
        c.execute(sql)
        row = c.fetchone()
        weight = float(row[0]) if row else 0.0
        actadj.append(weight*actret)
        projadj.append(weight*projret)
    cnxn.close()
    return streams.ReturnStream(startdates, enddates, numpy.array(actadj)) , \
        streams.ReturnStream(startdates, enddates, numpy.array(projadj))

if __name__=='__main__':
    st = datetime.datetime(2012,12,31)
    ed = datetime.datetime(2014,6,30)
    for po in PORTOPTS:
        print 'RUNNING PORT OPT #', po
        calcdata(po, st, ed)