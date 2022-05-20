#ifndef TICKH
#define TICKH

#include <math.h>
#include <vector>

// use g++ -std=c++11 -c tick.h to compile

// data structure for basic tick information
class BasicTick {
 public:
    BasicTick() {
        m_price=0;
        m_qty=0;
        m_turnover=0;
        m_bid=0;
        m_ask=0;
        m_bidQty=0;
        m_askQty=0;
    }
    BasicTick(const BasicTick& bt) {
        m_price=bt.m_price;
        m_qty=bt.m_qty;
        m_turnover=bt.m_turnover;
        m_bid=bt.m_bid;
        m_ask=bt.m_ask;
        m_bidQty=bt.m_bidQty;
        m_askQty=bt.m_askQty;
    }
 BasicTick(int price, int qty, int turnover, int bid, int ask, int bidQty, int askQty):
    m_price(price), m_qty(qty), m_turnover(turnover), m_bid(bid), m_ask(ask), m_bidQty(bidQty),
        m_askQty(askQty) {
            if (bid==0) m_bid=ask;
            if (ask==0) m_ask=bid;
        }
 public:
    int m_price;
    int m_qty;
    int m_turnover;
    int m_bid;
    int m_ask;
    int m_bidQty;
    int m_askQty;
};

// data structure for more tick information
class ExtendedTick : public BasicTick {
 public:
    ExtendedTick(){};
 ExtendedTick(const ExtendedTick& et) : BasicTick(et) {
        m_buyTrade=et.m_buyTrade;
        m_sellTrade=et.m_sellTrade;
        m_buy2Trade=et.m_buy2Trade;
        m_sell2Trade=et.m_sell2Trade;
    }
 ExtendedTick(int price, int qty, int turnover, int bid, int ask, int bidQty,
                 int askQty):
    BasicTick(price,qty,turnover,bid,ask,bidQty,askQty),
        m_buyTrade(0), m_sellTrade(0), m_buy2Trade(0), m_sell2Trade(0){}
 ExtendedTick(const BasicTick& base):BasicTick(base),
        m_buyTrade(0), m_sellTrade(0), m_buy2Trade(0), m_sell2Trade(0){}
    ~ExtendedTick(){}
    void setMid(float x) {m_mid=x;}
    void setBuyTrade(int x) {m_buyTrade=x;}
    void setSellTrade(int x) {m_sellTrade=x;}
    void setBuy2Trade(int x) {m_buy2Trade=x;}
    void setSell2Trade(int x) {m_sell2Trade=x;}
 public:
    float m_mid;
    int m_buyTrade;// estimated active buy volume at best ask
    int m_sellTrade;// estimated active sell volume at best bid
    int m_buy2Trade;// estimated active buy volume at higher ask
    int m_sell2Trade;//  estimated active sell volume at lower bid
};



#endif
