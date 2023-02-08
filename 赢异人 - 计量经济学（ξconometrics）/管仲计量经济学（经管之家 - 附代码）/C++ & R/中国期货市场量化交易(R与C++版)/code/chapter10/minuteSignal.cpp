#include "minuteSignal.h"

#include <algorithm>
#include <functional>
#include <cmath>
#include <set>
#include <sstream>
#include <fstream>

using namespace std;

// 1: buy open
// 2: sell open
// 3. buy close
//4. sell close
//5. all close
template<typename IndicatorItr, typename TickItr, typename TickType, typename MiddleItr>
void MinuteSignal<IndicatorItr, TickItr, TickType, MiddleItr>::getAction() {
  for (auto iStrat=0; iStrat!=m_strat.size(); ++iStrat) {
    m_pred[iStrat]=getPred(iStrat);
    if (m_pred[iStrat]>m_thre[iStrat].open)  m_action[iStrat]=1; // buy open
    else if (m_pred[iStrat]< -m_thre[iStrat].open) m_action[iStrat]=2; // sell open
    else if (m_pred[iStrat]>= -m_thre[iStrat].close && m_pred[iStrat]<=m_thre[iStrat].close) m_action[iStrat]=5; // buy close and sell close
    else if (m_pred[iStrat]>= -m_thre[iStrat].close) m_action[iStrat]=3; // buy close
    else if (m_pred[iStrat]<=m_thre[iStrat].close) m_action[iStrat]=4; // sell close
    else m_action[iStrat]=0;
  }
}


template<typename IndicatorItr, typename TickItr, typename TickType, typename MiddleItr>
void MinuteSignal<IndicatorItr, TickItr, TickType, MiddleItr>::setThre() {}

template<typename IndicatorItr, typename TickItr, typename TickType, typename MiddleItr>
void MinuteSignal<IndicatorItr, TickItr, TickType, MiddleItr>::setup() {
    m_middleResult.resize(IndicatorNum);
    ifstream input(m_stratFile.c_str());
    string head;
    getline(input, head);
    string item;
    string signal;
    float coef;
    int strat;
    int maxStrat=0;
    StratCoef stratCoef;
    while (true) {
        getline(input, item);
        if (input.fail()) break;
        stringstream str;
        str << item;
        str >> signal >> coef >> strat;
        m_mapIndicator[signal]=0;
        if (strat==maxStrat) stratCoef[signal]=coef;
        else {
            m_strat.push_back(stratCoef);
            stratCoef.clear();
            stratCoef[signal]=coef;
            maxStrat=strat;
        }
    }
    input.close();
    m_strat.push_back(stratCoef);
    
    for (auto item:m_mapIndicator)
      m_indicatorName.push_back(item.first);
    cout << "name len " << m_indicatorName.size() << endl;
    m_pred.resize(m_strat.size(), 0.0);
    m_action.resize(m_strat.size(), 0);
    ifstream inputThre(m_threFile.c_str());
    getline(inputThre, head);
    struct StratThre stratThre;
    ifstream inputWeight(m_weightFile.c_str());
    if (inputWeight.fail()) {
      stratThre.open=m_openThre;
      stratThre.close=m_closeThre;
      stratThre.sharp=1;
      m_thre.push_back(stratThre);
      return;
    }

    while (true) {
        getline(inputThre, item);
        if (inputThre.fail()) break;
        stringstream str;
        str << item;
        str >> stratThre.open >> stratThre.close >> stratThre.sharp >> strat;
        m_thre.push_back(stratThre);
    }
    inputThre.close();
    // get weight
    m_numStrat=0;
    getline(inputWeight, head);
    int i=0;
    while (true) {
      stringstream str;
      string item;
      getline(inputWeight, item);
      if (inputWeight.fail()) break;
      int weight;
      str << item;
      str >> weight;
      m_thre[i++].weight=weight;
    }
    m_numStrat=m_strat.size();
    /*
    for (int chosen=0; chosen!=m_strat.size(); ++chosen) 
      for (auto item:m_strat[chosen])
        cout << item.first << " " << item.second << " " << chosen << endl;
    for (auto item:m_thre)
        cout << item.open << " " << item.close << " " << item.sharp << " " << item.weight << endl;
    */
    inputWeight.close();
    //setThre();
}


template<typename IndicatorItr, typename TickItr, typename TickType, typename MiddleItr>
void MinuteSignal<IndicatorItr, TickItr, TickType, MiddleItr>::calTickIndicator() {
    IndicatorItr indStart=m_indicators.begin();
    IndicatorItr helperStart=m_helpers.begin();
int middle=0;
    auto cur=m_startItr+m_bar;
    cur->m_wpr=calWpr(cur);
    cur->m_hlc=static_cast<float>(cur->m_high+cur->m_low+cur->m_close)/3;
    for (int period=m_minPeriod; period<=m_maxPeriod; period*=2) {
        calMACD(m_startItr, m_bar, helperStart++, helperStart++, indStart++, period);
    }
}




template<typename IndicatorItr, typename TickItr, typename TickType, typename MiddleItr>
void MinuteSignal<IndicatorItr, TickItr, TickType, MiddleItr>::calMACD(TickItr start, int tick, IndicatorItr fastMACD, IndicatorItr slowMACD,  IndicatorItr signal, int nFast, int nSig) {
    float wpr=(start+tick)->m_wpr;
    int nSlow=nFast*2;
    if (tick<nFast-1) *fastMACD=0.0;
    else if (tick==nFast-1) {
        *fastMACD=0.0;
        for (int itr=0; itr!=nFast; ++itr)
            (*fastMACD)+=(start+itr)->m_wpr;
        (*fastMACD)/=nFast;
    } else calEMA(fastMACD,wpr,nFast);
    if (tick<nSlow-1) *slowMACD=0.0;
    else if (tick==nSlow-1) {
        *slowMACD=0.0;
        for (int itr=0; itr!=nSlow; ++itr)
            (*slowMACD)+=(start+itr)->m_wpr;
        (*slowMACD)/=nSlow;
    } else calEMA(slowMACD,wpr,nSlow);
    float macd=0.0;
    if (tick>=nSlow-1) {
        macd=100*((*fastMACD)/(*slowMACD)-1);
        calEMA(signal,macd,nSig);
    }
    setValue("macd", nFast, *signal);
}


template void MinuteSignal<MinIndItr, MinItr, MinuteTick, MinMiddleItr>::calMACD(MinItr start, int tick, MinIndItr fastMACD, MinIndItr slowMACD,  MinIndItr signal, int nFast, int nSig);

template void MinuteSignal<MinIndItr, MinItr, MinuteTick, MinMiddleItr>::calTickIndicator(); 

template void MinuteSignal<MinIndItr, MinItr, MinuteTick, MinMiddleItr>::setup(); 

template void MinuteSignal<MinIndItr, MinItr, MinuteTick, MinMiddleItr>::getAction();
