import ystockquote
import streams
import scipy
import datetime

def getyahoovals(ticker, startdate=datetime.datetime(1980,1,1), enddate=datetime.datetime(2040,12,31)):
    strp = lambda x : x.strftime('%Y%m%d')
    vals = ystockquote.get_historical_prices(ticker, strp(startdate), strp(enddate))
    dates = [datetime.datetime.strptime(val[0],'%Y-%m-%d') for val in vals[1:][::-1]]
    quotes = [float(val[-1]) for val in vals[1:][::-1]]
    return streams.BasicStream(dates, scipy.array(quotes))


STREAMBASKET = ['^RUT','^GSPC','EFA','AGG','BIL']
def ymarketbasket(startdate=datetime.datetime(1980,1,1),enddate=datetime.datetime(2040,12,31),basket=STREAMBASKET):
    return dict([(index,getyahoovals(index,startdate,enddate)) for index in basket])

def fillbill(streambasket):
    bilstream = streambasket['BIL']
    initdate = bilstream.startdates[0]
    fillstarts = [initdate+datetime.timedelta(days=-i) for i in range(1,5000)][::-1]
    fillends =  [initdate+datetime.timedelta(days=-i) for i in range(0,4999)][::-1]
    returns = scipy.zeros(len(fillstarts))
    tstream = streams.ReturnStream(fillstarts+bilstream.startdates,
                                   fillends+bilstream.enddates,
                                   scipy.hstack((returns,bilstream.returns)))
    streambasket['BIL'] = tstream
    return True

def outbasket():
    basket = ymarketbasket()
    fillbill(basket)
    return basket