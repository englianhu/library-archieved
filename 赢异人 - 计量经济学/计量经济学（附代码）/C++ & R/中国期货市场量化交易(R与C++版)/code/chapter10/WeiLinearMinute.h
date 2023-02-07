#ifndef WEILINEARMINUTEH
#define WEILINEARMINUTEH

#include "minuteSignal.h"
#include <sstream>

struct coreTick {
    int date;
    string time;
    int milli;
    double price;
    double turnover;
    double openInt;
    double bid;
    double ask;
    int bidQty;
    int askQty;
    int qty;
};

inline int roundPrice(int price, int multiple) {
    return ((price+multiple/2)/multiple)*multiple;
}


class WeiLinearMinute {
 public:
 WeiLinearMinute(const string minuteFile, const string stratFile, const string threFile, const string tickFile, const string nightEnd,  const int strat, const float openThre, const float closeThre, const float sarInitGap, const int minPeriod, const int maxPeriod) : m_minuteFile(minuteFile), m_stratFile(stratFile), m_threFile(threFile), m_tickFile(tickFile), m_nightEnd(nightEnd), m_strat(strat), m_openThre(openThre), m_closeThre(closeThre), m_SarInitGap(sarInitGap), m_minPeriod(minPeriod), m_maxPeriod(maxPeriod) {
        setup();
    }
  WeiLinearMinute(const string productFile, const string path);
        
    void setup();
    void processBook(const string& bookFile);
    void processMinute(const string& bookFile, const bool cut=false);
    void openBook(const string& bookFile);
    void processBar();
    void setNewTick(int date, string time, int milli, int price, int qty, int turnover, int openInt, int bid, int ask, int bidQty, int askQty) {
        m_tick=MinuteTick(price, qty, turnover, bid, ask, bidQty, askQty, time);
    }
    void updateBar();
    void nextBar();
    void processTick();
    void processTickSimple();
    void outputMinute(ostream& out, const MinuteTick& item);
    void outputTick(ostream& out, const coreTick& item);
    void getAction();
    void cutMinute(ofstream& out, const string bookFile, const bool cut);
 public:
    //int m_multiplier; // multiplier of the contract
    int m_spread; // spread of the contract
    
    //vector<string> timeStamp;
    vector<MinuteTick> m_book;
    int m_tickNum; // number of ticks from start
    float m_SarInitGap;
    inline void getTickData(const coreTick& newTick) {
      //outputTick(m_outputTick, newTick);
        m_tick.m_date=newTick.date;
        m_tick.m_time=newTick.time;
        m_tick.m_milli=newTick.milli;
        m_tick.m_price=roundPrice(newTick.price*m_intFactor, m_spread);
        m_tick.m_qty=newTick.qty;
        m_tick.m_turnover=roundPrice(newTick.turnover*m_intFactor, m_spread);
        m_tick.m_openInt=roundPrice(newTick.openInt*m_intFactor, m_spread);
        m_tick.m_bid=roundPrice(newTick.bid*m_intFactor, m_spread);
        m_tick.m_ask=roundPrice(newTick.ask*m_intFactor, m_spread);
        m_tick.m_bidQty=newTick.bidQty;
        m_tick.m_askQty=newTick.askQty;
    }
    string m_historyFile;
    string m_timeFile;
    string m_minuteFile;
    string m_stratFile;
    string m_threFile;
    string m_tickFile;
    string m_predFile;
    string m_indiFile;
    string m_weightFile;
    string m_curTime;
    string m_nightEnd;
    int m_numStrat;
    int m_intFactor;
    int m_curTimeInt;
    string m_preTime;
    int m_preTimeInt;
    int m_preMilli;
    MinuteTick m_tick;
    MinuteTick m_entryTick;
    int m_tickCount;
    ofstream m_outputMinute;
    ofstream m_outputTick;
    ofstream m_outputPred;
    ofstream m_outputIndi;
    ifstream m_input;
    MinIndMat m_middleResult;
    MinItr m_startItr;
    int m_strat;
    float m_openThre;
    float m_closeThre;
    int m_minPeriod;
    int m_maxPeriod;
    int m_totalTick;
    vector<int> m_position;
    vector<string> m_indicatorName;
    int m_tradeQty;
    vector<int> m_weight;
    MinuteSignal<MinIndItr, MinItr, MinuteTick, MinMiddleItr> m_minuteSignal;
    ~WeiLinearMinute() {
      //m_outputTick.close();
      //m_outputMinute.close();
      //m_outputPred.close();
    }
public:
    static const string MINUTE_HEAD;
    static const string TICK_HEAD;
    static const string PRED_HEAD;
    static const string TWENTY_FOUR_HOURS;
    static const string ZERO_HOURS;
    static const string EVENING_START;
    static const string MORNING_START;
    static const string NEXT_DAY_START;
    static const string MORNING_BREAK;
    static const string MORNING_RESUME;
    static const string MORNING_END;
    static const string AFTERNOON_START;
    static const string AFTERNOON_END;
    static const string NIGHT_AUCTION;
    static const string DAY_AUCTION;
    static const string PASS_END;
    static const string NIGHT_AUCTION_END;
    static const string DAY_AUCTION_END;
    //static const string NIGHT_END;
    static const int TICK_NUM=300;
};   

#endif
