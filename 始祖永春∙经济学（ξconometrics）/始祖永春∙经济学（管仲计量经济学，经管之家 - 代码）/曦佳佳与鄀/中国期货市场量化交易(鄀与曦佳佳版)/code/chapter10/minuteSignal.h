#ifndef MINUTESIGNALH
#define MINUTESIGNALH

#include <vector>
#include <map>
#include <string>
#include <memory>
#include <iterator>
#include <cmath>
#include <numeric>
#include <array>
#include <tr1/memory>
#include <functional>
#include <set>

#include "minuteTick.h"

typedef vector<float>::iterator MinIndItr;

typedef shared_ptr<MinIndMat> MinIndMatPtr;
typedef map<string, float> StratCoef;
struct StratThre {
  float open;
  float close;
  float sharp;
  int weight;
};


const int IndicatorNum=100;
const int HelperNum=100;
const int MiddleNum=200;

// general indicator function

template<typename IndicatorItr, typename TickItr, typename TickType, typename MiddleItr>
  class MinuteSignal {
 public:

  MinIndMat m_middleResult;
  void setThre();
  void getAction();
  void setup();
  int m_chosenStrat;
  int m_position;
  vector<int> m_action;
  float m_openThre;
  float m_closeThre;
  vector<float> m_pred;
  float m_SarInitGap;
  int m_minPeriod;
  int m_maxPeriod;
  int m_numStrat;
  string m_stratFile;
  string m_threFile;
  string m_weightFile;
  
 MinuteSignal(TickItr startItr, const string& stratFile, const string& threFile, const string& weightFile, const int& strat, const float& openThre, const float& closeThre, const float& sarInitGap, const int minPeriod, const int maxPeriod) : m_startItr(startItr), m_indicators(IndicatorNum,0.0), m_helpers(HelperNum, 0.0), m_bar(0), m_chosenStrat(strat), m_openThre(openThre), m_closeThre(closeThre), m_SarInitGap(sarInitGap), m_minPeriod(minPeriod), m_maxPeriod(maxPeriod), m_stratFile(stratFile), m_threFile(threFile), m_weightFile(weightFile) {
        setup();
    }
 MinuteSignal(): m_indicators(IndicatorNum,0.0), m_helpers(HelperNum,0.0), m_bar(0) {}
    inline float getPred(const int strat) {
        float result=0.0;
        //cout << m_strat[strat].size() << endl;
        //cout << m_minPeriod << " " << m_maxPeriod << endl;
        for (auto item:m_strat[strat]) {
          //cout << item.first << " " << item.second << " " << m_mapIndicator[item.first] << endl;
          if (isnan(m_mapIndicator[item.first]) || !isfinite(m_mapIndicator[item.first])) m_mapIndicator[item.first]=0.0;
            result+=(m_mapIndicator[item.first]*item.second);
        }
        //cout << "result " << result << endl;
        return (result);
    }
    ~MinuteSignal(){}
    void calTickIndicator();
    vector<float> m_indicators;
    vector<float> m_helpers;
    vector<string> m_indicatorName;
    map<string, float> m_mapIndicator;
    
    vector<StratCoef> m_strat;
    //vector<vector<StratThre> > m_thre;
    vector<StratThre> m_thre;
    TickItr m_startItr;
    int m_bar;
    inline void setValue(string signal, int period, float value) {
        string name=signal;
        name+=string(".");
        name+=to_string(period);
        m_mapIndicator[name]=value;
    }
            
    
    inline float calWpr(TickItr tick) {
        return (static_cast<float>(tick->m_bid*tick->m_askQty+tick->m_ask*tick->m_bidQty)/(tick->m_bidQty+tick->m_askQty));
    };
    
    void calEMA(IndicatorItr ema, float x, int period, bool wilder=false) {
        float ratio=2.0/(static_cast<float>(period)+1);
        if (wilder) ratio=1.0/(static_cast<float>(period));
        *ema=*ema*(1-ratio)+x*ratio;
    }
    void  calMACD(TickItr start, int tick, IndicatorItr fastMACD, IndicatorItr slowMACD,  IndicatorItr signal, int nFast, int nSig=9);
};    
#endif








