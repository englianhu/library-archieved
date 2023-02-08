#ifndef MINUTETICKH
#define MINUTETICKH

#include <math.h>
#include <vector>
#include "tick.h"
#include <string>
#include <array>
#include <iostream>
#include <fstream>

using namespace std;

inline bool goodTime(const string& time) {
    return (time[6]=='0' && time[7]=='0' && (time[4]=='0' || time[4]=='5'));
}

inline int timeToInt(const string& time) {
    return ((time[0]-'0')*36000+(time[1]-'0')*3600+(time[3]-'0')*600+(time[4]-'0')*60+(time[6]-'0')*10+(time[7]-'0'));
}

string intToTime(const int& time);

string getBarEnd(const string& time);

class MinuteTick : public BasicTick {
public:
    string m_time;
    int m_open;
    int m_high;
    int m_low;
    int m_close;
    int m_date;
    int m_openInt;
    int m_milli;
    float m_wpr;
    float m_hlc;
    string m_contract;
    bool m_upperLimit;
    bool m_lowerLimit;
    int m_timeInt;
 MinuteTick(int price, int qty, int turnover, int bid, int ask, int bidQty,int askQty, string time):
    BasicTick(price,qty,turnover,bid,ask,bidQty,askQty), m_time(time) {
      m_upperLimit=false;
      m_lowerLimit=false;
      if (bid==0 && ask>0) m_lowerLimit=true;
      if (bid>0 && ask==0) m_upperLimit=true;
    }
 MinuteTick(const BasicTick& base, string time):BasicTick(base), m_time(time){
      m_upperLimit=false;
      m_lowerLimit=false;
      if (base.m_bid==0 && base.m_ask>0) m_lowerLimit=true;
      if (base.m_bid>0 && base.m_ask==0) m_upperLimit=true;
    }
    MinuteTick(){}
    ~MinuteTick(){}
};


const int minuteNum=2000;
typedef array<float,minuteNum> MinIndVec;
typedef vector<MinIndVec> MinIndMat;


typedef vector<MinuteTick>::iterator MinItr;
typedef array<float,minuteNum>::iterator MinMiddleItr;


#endif
