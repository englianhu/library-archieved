#include "WeiLinearMinute.h"
#include <sstream>

using namespace std;

const string WeiLinearMinute::MINUTE_HEAD="date time timeInt milli price open.int qty bid ask bid.qty ask.qty open high low close";
const string WeiLinearMinute::TICK_HEAD="date time milli price qty turnover open.int bid ask bid.qty ask.qty";
const string WeiLinearMinute::PRED_HEAD="date time price bid ask"; 
const string WeiLinearMinute::TWENTY_FOUR_HOURS="24:00:00";
const string WeiLinearMinute::ZERO_HOURS="00:00:00";
const string WeiLinearMinute::EVENING_START="21:05:00";
const string WeiLinearMinute::MORNING_START="09:05:00";
const string WeiLinearMinute::NEXT_DAY_START="00:05:00";
const string WeiLinearMinute::MORNING_BREAK="10:20:00";
const string WeiLinearMinute::MORNING_RESUME="10:35:00";
const string WeiLinearMinute::MORNING_END="11:30:00";
const string WeiLinearMinute::AFTERNOON_START="13:35:00";
const string WeiLinearMinute::AFTERNOON_END="15:00:00";
const string WeiLinearMinute::NIGHT_AUCTION="20:59:00";
const string WeiLinearMinute::DAY_AUCTION="08:59:00";
const string WeiLinearMinute::PASS_END="15:05:00";
const string WeiLinearMinute::NIGHT_AUCTION_END="21:00:00";
const string WeiLinearMinute::DAY_AUCTION_END="09:00:00";

//const string WeiLinearMinute::m_nightEnd="01:00:00";

void WeiLinearMinute::getAction() {
  m_minuteSignal.calTickIndicator();
  m_minuteSignal.getAction();
  for (auto iStrat=0; iStrat!=m_minuteSignal.m_strat.size(); ++iStrat) {
    if (m_entryTick.m_lowerLimit && m_entryTick.m_ask>0 && m_minuteSignal.m_action[iStrat]==1) m_minuteSignal.m_action[iStrat]=3;
    if (m_entryTick.m_bid>0 && m_entryTick.m_upperLimit && m_minuteSignal.m_action[iStrat]==2) m_minuteSignal.m_action[iStrat]=4;
    if (m_entryTick.m_time==m_nightEnd || m_entryTick.m_time==AFTERNOON_END) m_minuteSignal.m_action[iStrat]=0;
    switch(m_minuteSignal.m_action[iStrat]) {
    case 1:
      if (m_entryTick.m_ask>0 && m_entryTick.m_bid>0 && !m_entryTick.m_upperLimit && !m_entryTick.m_lowerLimit) m_position[iStrat]=m_minuteSignal.m_thre[iStrat].weight;
      else if (m_entryTick.m_ask>0 && m_position[iStrat]<0 && m_entryTick.m_lowerLimit) m_position[iStrat]=0;
      break;
    case 2:
      if (m_entryTick.m_ask>0 && m_entryTick.m_bid>0 && !m_entryTick.m_upperLimit && !m_entryTick.m_lowerLimit) m_position[iStrat]=-m_minuteSignal.m_thre[iStrat].weight;
      else if (m_entryTick.m_bid>0 && m_position[iStrat]>0 && m_entryTick.m_upperLimit) m_position[iStrat]=0;
      break;
    case 3:
      if (m_position[iStrat]<0 && m_entryTick.m_ask>0 && !m_entryTick.m_upperLimit) m_position[iStrat]=0;
      break;
    case 4:
      if (m_position[iStrat]>0 && m_entryTick.m_bid>0 && !m_entryTick.m_lowerLimit) m_position[iStrat]=0;
      break;
    case 5:
      m_position[iStrat]=0;
      break;
    }
    if (abs(m_position[iStrat])>m_minuteSignal.m_thre[iStrat].weight) m_position[iStrat]=0;
  }
}


WeiLinearMinute::WeiLinearMinute(const string productFile, const string path) {
    ifstream input(productFile.c_str());
    m_minPeriod=8;
    m_maxPeriod=64;
    m_numStrat=0;
    while (true) {
        string item;
        getline(input, item);
        if (input.fail()) break;
        stringstream str;
        string var;
        string temp;
        str << item;
        str >> var;
        if (var=="minuteFile") {
          str >> temp;
          m_minuteFile=path+"/"+temp;
        } else if (var=="stratFile") {
          str >> temp;
          m_stratFile=path+"/"+temp;
        } else if (var=="threFile") {
          str >> temp;
          m_threFile=path+"/"+temp;
        } else if (var=="tickFile") {
          str >> temp;
          m_tickFile=path+"/"+temp;
        }
        //else if (var=="predFile") str >> m_predFile;
        else if (var=="nightEnd") str >> m_nightEnd;
        else if (var=="strat") str >> m_strat;
        else if (var=="openThre") str >> m_openThre;
        else if (var=="closeThre") str >> m_closeThre;
        else if (var=="sarInitGap") str >> m_SarInitGap;
        else if (var=="intFactor") str >> m_intFactor;
        else if (var=="spread") str >> m_spread;
        else if (var=="minPeriod") str >> m_minPeriod;
        else if (var=="maxPeriod") str >> m_maxPeriod;
        else if (var=="numStrat") str >> m_numStrat;
    }
    size_t pos=m_tickFile.find("tick");
    m_predFile=m_tickFile;
    m_predFile.replace(pos, 4, "pred");
    pos=m_predFile.find("tick");
    m_predFile.replace(pos, 4, "pred");
    pos=m_tickFile.find("tick");
    // get indiFile
    m_indiFile=m_tickFile;
    m_indiFile.replace(pos, 4, "indi");
    pos=m_indiFile.find("tick");
    m_indiFile.replace(pos, 4, "indi");
    // get weightFile
    m_weightFile=m_tickFile;
    m_weightFile.replace(pos, 4, "weight");
    pos=m_weightFile.find("tick");
    m_weightFile.replace(pos, 4, "weight");
    cout << m_weightFile << endl;
    input.close();
    setup();
}

void WeiLinearMinute::cutMinute(ofstream& out, const string bookFile, const bool cut) {
    out << MINUTE_HEAD << endl;
    ifstream input(bookFile.c_str());
    string head;
    getline(input,head);
    string item;
    int dayCount=0;
    int timeInt;
    while (true) {
        getline(input,item);
        if (input.fail()) break;
        stringstream str;
        str << item;
        str >> m_entryTick.m_date >> m_entryTick.m_time >> m_entryTick.m_timeInt  >> m_entryTick.m_milli >> m_entryTick.m_price >> m_entryTick.m_openInt >> m_entryTick.m_qty >> m_entryTick.m_bid >> m_entryTick.m_ask >> m_entryTick.m_bidQty >> m_entryTick.m_askQty >> m_entryTick.m_open >> m_entryTick.m_high >> m_entryTick.m_low >> m_entryTick.m_close; // from historical record
        if (m_entryTick.m_time==AFTERNOON_END) {
          dayCount++;
          if (dayCount==1 && cut) continue;
        }
        if (dayCount==0 && cut) continue;
        out << m_entryTick.m_date << " " << m_entryTick.m_time << " " << m_entryTick.m_timeInt  << " " << m_entryTick.m_milli << " " << m_entryTick.m_price << " " << m_entryTick.m_openInt << " " << m_entryTick.m_qty << " " << m_entryTick.m_bid << " " << m_entryTick.m_ask << " " << m_entryTick.m_bidQty << " " << m_entryTick.m_askQty << " " << m_entryTick.m_open << " " << m_entryTick.m_high << " " << m_entryTick.m_low << " " << m_entryTick.m_close << endl;
    }
    out.close();
}

void WeiLinearMinute::setup() {
    m_entryTick.m_high=m_tick.m_price;
    m_entryTick.m_low=m_tick.m_price;
    if (m_entryTick.m_price==0) {
        m_entryTick.m_low=1e+7;
        m_entryTick.m_high=0;
    }
    m_entryTick.m_turnover=0;
    m_entryTick.m_openInt=0;
    m_entryTick.m_qty=0;
    //m_curTime=EVENING_START;
    //m_curTimeInt=timeToInt(m_curTime);
    m_outputMinute.open(m_minuteFile);
    m_outputMinute << MINUTE_HEAD << endl;
    //m_outputTick.open(m_tickFile);
    // m_outputTick << TICK_HEAD << endl;
    m_outputPred.open(m_predFile);
    m_outputPred << PRED_HEAD;
    // main program
    m_middleResult.resize(MiddleNum);
    cout << "start minute signal" << endl;
    m_minuteSignal=MinuteSignal<MinIndItr, MinItr, MinuteTick, MinMiddleItr>(m_startItr, m_stratFile, m_threFile, m_weightFile, m_strat, m_openThre, m_closeThre, m_SarInitGap, m_minPeriod, m_maxPeriod);
    cout << "finish minute signal" << endl;
    for (auto item:m_minuteSignal.m_mapIndicator)
      m_indicatorName.push_back(item.first);
    m_position.resize(m_minuteSignal.m_strat.size(), 0);
    for (size_t i=0; i!=m_minuteSignal.m_strat.size();  ++i)
      m_outputPred << " action." << i << " pred." << i << " position." << i;
    m_outputPred << endl;
    //if (numStrat==0) m_position.resize(1, 0);
    cout << "trade qty " << m_tradeQty << endl;
    if (m_numStrat==0) m_minuteSignal.m_thre[0].weight=m_tradeQty;
    m_outputIndi.open(m_indiFile);
     m_outputIndi << "date time";
    for (auto item:m_indicatorName)
      m_outputIndi << " " << item;
    m_outputIndi << endl;
    //cout << endl;
    m_tickNum=0;
    m_tickCount=0;
    m_totalTick=0;
}    

void WeiLinearMinute::outputMinute(ostream& out, const MinuteTick& item) {
  int ask=item.m_ask;
  int bid=item.m_bid;
  if (item.m_upperLimit) ask=0;
  if (item.m_lowerLimit) bid=0;
    out << item.m_date << " " << item.m_time << " " << item.m_timeInt << " " << item.m_milli << " " << item.m_price << " " << item.m_openInt << " " << item.m_qty << " " << bid << " "
        << ask << " " << item.m_bidQty << " " << item.m_askQty << " "
        << item.m_open << " " << item.m_high << " " << item.m_low << " "
        << item.m_close << endl;
}

//void WeiLinearMinute::outputTick(ostream& out, const coreTick& item) {
//    out << item.date << " " << item.time << " " << item.milli << " " << item.price << " " << item.qty << " "
//        << item.turnover << " " << item.openInt << " " << item.bid << " " << item.ask << " " << item.bidQty
//        << " " << item.askQty << endl;
//}

void WeiLinearMinute::nextBar() {
    if (m_tick.m_time==DAY_AUCTION) {
        m_curTime=MORNING_START;
        m_curTimeInt=timeToInt(m_curTime);
    } else if (m_tick.m_time==NIGHT_AUCTION) {
        m_curTime=EVENING_START;
        m_curTimeInt=timeToInt(m_curTime);
    } else {
        int timeInt=m_tick.m_timeInt;
        while (timeInt % TICK_NUM!=0) timeInt++;
        if (timeInt==86400) {
            m_curTimeInt=0;
            m_curTime=TWENTY_FOUR_HOURS;
        } else {
            m_curTimeInt=timeInt;
            m_curTime=intToTime(m_curTimeInt);
        }
    }
}

void WeiLinearMinute::updateBar() {
  if (m_book.size()>1) {
    auto itr=m_book.end()-1;
    if (itr->m_time==m_entryTick.m_time) return;
    if (m_entryTick.m_time==NIGHT_AUCTION_END || m_entryTick.m_time==DAY_AUCTION_END) return;
  }
  if (m_entryTick.m_time.size()==8 && m_entryTick.m_time!=NIGHT_AUCTION_END
      && m_entryTick.m_time!=DAY_AUCTION_END) {
    m_entryTick.m_lowerLimit=false;
    m_entryTick.m_upperLimit=false;
    if (m_entryTick.m_bid==0 && m_entryTick.m_ask>0) {
      m_entryTick.m_lowerLimit=true;
      m_entryTick.m_bid=m_entryTick.m_ask-m_spread;
    }
    if (m_entryTick.m_bid>0 && m_entryTick.m_ask==0) {
      m_entryTick.m_upperLimit=true;
      m_entryTick.m_ask=m_entryTick.m_bid+m_spread;
    }
    outputMinute(m_outputMinute, m_entryTick);
    m_book.push_back(m_entryTick);
    m_minuteSignal.m_startItr=m_book.begin();
    m_minuteSignal.m_bar=m_book.size()-1;
    getAction();
    m_outputPred << m_entryTick.m_date << " " <<  m_entryTick.m_time  << " "  << m_entryTick.m_price << " " << m_entryTick.m_bid << " " << m_entryTick.m_ask;
    for (size_t i=0; i!=m_minuteSignal.m_strat.size();  ++i)
      m_outputPred << " " << m_minuteSignal.m_action[i] << " " << m_minuteSignal.m_pred[i] << " " << m_position[i];
    m_outputPred << endl;
    m_outputIndi << m_entryTick.m_date << " " << m_entryTick.m_time;
    for (auto item:m_minuteSignal.m_mapIndicator)
      m_outputIndi << " " << item.second;
    m_outputIndi << endl;
  }
  m_tickCount=0;
  m_entryTick.m_high=0;
  m_entryTick.m_low=1e7;
  m_entryTick.m_turnover=0;
  m_entryTick.m_openInt=0;
  m_entryTick.m_qty=0;
}


void WeiLinearMinute::processTick() {
    if (m_tick.m_bid==0 && m_tick.m_ask==0 && m_tick.m_bidQty==0 && m_tick.m_askQty==0) return;
    if (m_tick.m_time>="15:00:01" && m_tick.m_time<="16:00:00") return;
    if (m_tick.m_time==m_preTime && m_tick.m_milli==m_preMilli) return;
    m_tick.m_timeInt=timeToInt(m_tick.m_time);
    m_preTime=m_tick.m_time;
    m_preTimeInt=m_tick.m_timeInt;
    m_preMilli=m_tick.m_milli;
    if (m_tickNum==0) {
          nextBar();
    }
    m_tickNum++;
    // pass by last day's end, put in that tick and start a new bar
    if (m_curTime==PASS_END) {
        if (m_tick.m_time==NIGHT_AUCTION || m_tick.m_time==DAY_AUCTION) {
            m_tickCount=0;
            m_entryTick.m_high=m_tick.m_price;
            m_entryTick.m_low=m_tick.m_price;
            m_entryTick.m_turnover=0;
            m_entryTick.m_openInt=0;
            m_entryTick.m_qty=0;
            if (m_tick.m_time==NIGHT_AUCTION) m_curTime=EVENING_START;
            else if (m_tick.m_time==DAY_AUCTION) m_curTime=MORNING_START;
            m_curTimeInt=timeToInt(m_curTime);
        } else if (m_tick.m_time!=AFTERNOON_END) {
            m_tickCount=0;
            m_entryTick.m_high=m_tick.m_price;
            m_entryTick.m_low=m_tick.m_price;
            m_entryTick.m_turnover=0;
            m_entryTick.m_openInt=0;
            m_entryTick.m_qty=0;
            nextBar();
            //m_curTimeInt=timeToInt(m_curTime);
        }
    }
    // supposed to get to end of day, but miss it
    if (m_curTime==AFTERNOON_END && (m_tick.m_time==NIGHT_AUCTION || m_tick.m_time==DAY_AUCTION)) {
        m_entryTick.m_time=m_curTime;
        m_entryTick.m_timeInt=0;
        updateBar();
        if (m_tick.m_time==NIGHT_AUCTION) m_curTime=EVENING_START;
        else if (m_tick.m_time==DAY_AUCTION) m_curTime=MORNING_START;
        m_curTimeInt=timeToInt(m_curTime);
    }

    // at the end of midnight, put in last bar without that tick, start a new bar
    if (m_curTime==TWENTY_FOUR_HOURS && m_tick.m_time==ZERO_HOURS && m_tick.m_milli==0) {
        m_entryTick.m_time=m_curTime;
        m_entryTick.m_timeInt=0;
        updateBar();
        m_curTime=NEXT_DAY_START;
        m_curTimeInt=timeToInt(m_curTime);
    }
    // exactly at the end of a bar, not start a new bar, must return
    if (m_curTime!=m_nightEnd  && m_tick.m_time!=ZERO_HOURS && goodTime(m_tick.m_time) && m_tick.m_milli==0 && m_tick.m_time==m_curTime) {
        m_entryTick.m_date=m_tick.m_date;
        m_entryTick.m_milli=m_tick.m_milli;
        m_entryTick.m_price=m_tick.m_price;
        m_entryTick.m_close=m_tick.m_price;
        m_entryTick.m_openInt+=m_tick.m_openInt;
        m_entryTick.m_qty+=m_tick.m_qty;
        m_entryTick.m_bid=m_tick.m_bid;
        m_entryTick.m_ask=m_tick.m_ask;
        m_entryTick.m_bidQty=m_tick.m_bidQty;
        m_entryTick.m_askQty=m_tick.m_askQty;
        if (m_tick.m_price>m_entryTick.m_high) m_entryTick.m_high=m_tick.m_price;
        if (m_tick.m_price<m_entryTick.m_low && m_tick.m_price>0) m_entryTick.m_low=m_tick.m_price;
        m_entryTick.m_turnover+=m_tick.m_turnover;
        m_tickCount=0;
        m_entryTick.m_time=m_curTime;
        m_entryTick.m_timeInt=m_tick.m_timeInt;
        updateBar();
        if (m_entryTick.m_time==m_nightEnd) {
            m_curTime=MORNING_START;
            m_curTimeInt=timeToInt(m_curTime);
        } else {
            m_curTimeInt=m_entryTick.m_timeInt+TICK_NUM;
            m_curTime=intToTime(m_curTimeInt);
        }
        if (m_curTime==TWENTY_FOUR_HOURS) m_curTimeInt=0;
        else if (m_curTime==MORNING_BREAK) {
            m_curTime=MORNING_RESUME;
            m_curTimeInt=timeToInt(m_curTime);
        } else if (m_entryTick.m_time==MORNING_END) {
            m_curTime=AFTERNOON_START;
            m_curTimeInt=timeToInt(m_curTime);
        }
        return;
    }
    // pass by a bar
    //if (m_curTime==m_nightEnd) cout << m_tick.m_time << endl;
    if (m_tick.m_time>m_curTime ||  (m_tick.m_time==m_curTime && m_tick.m_milli>0 && m_curTime!=m_nightEnd) || (m_tick.m_time<"20" && m_curTime>"20")) {
        if (m_tick.m_time<"20" && m_curTime==TWENTY_FOUR_HOURS) {
            m_curTime=ZERO_HOURS;
            m_curTimeInt=0;
        }
        // while (m_tick.m_time>m_curTime || m_tick.m_time==m_curTime && m_tick.m_milli>0) {
            m_entryTick.m_time=m_curTime;
            if (m_curTime!=m_nightEnd) {
                m_curTimeInt+=TICK_NUM;
                m_curTime=intToTime(m_curTimeInt);
            } else {
                m_curTime=MORNING_START;
                m_curTimeInt=timeToInt(m_curTime);
            }
            if (m_curTime==TWENTY_FOUR_HOURS) m_curTimeInt=0;
            else if (m_curTime==MORNING_BREAK) {
                m_curTime=MORNING_RESUME;
                m_curTimeInt=timeToInt(m_curTime);
            } else if (m_entryTick.m_time==MORNING_END) {
                m_curTime=AFTERNOON_START;
                m_curTimeInt=timeToInt(m_curTime);
            }
            updateBar();
    }
    // the same bar
    m_entryTick.m_date=m_tick.m_date;
    m_entryTick.m_close=m_tick.m_price;
    m_entryTick.m_milli=m_tick.m_milli;
    m_entryTick.m_price=m_tick.m_price;
    m_entryTick.m_openInt+=m_tick.m_openInt;
    m_entryTick.m_qty+=m_tick.m_qty;
    m_entryTick.m_bid=m_tick.m_bid;
    m_entryTick.m_ask=m_tick.m_ask;
    m_entryTick.m_bidQty=m_tick.m_bidQty;
    m_entryTick.m_askQty=m_tick.m_askQty;
    if (m_tickCount==0) m_entryTick.m_open=m_tick.m_price;
    if (m_tick.m_price>m_entryTick.m_high) m_entryTick.m_high=m_tick.m_price;
    if (m_tick.m_price<m_entryTick.m_low) m_entryTick.m_low=m_tick.m_price;
    m_entryTick.m_turnover+=m_tick.m_turnover;
    m_tickCount++;
}


void WeiLinearMinute::processBook(const string& bookFile) {
    ifstream input(bookFile.c_str());
    string head;
    getline(input,head);
    string item;
    while (true) {
        getline(input,item);
        if (input.fail()) break;
        stringstream str(item);
        coreTick newTick;
        str >> newTick.date >> newTick.time >> newTick.milli >> newTick.price >> newTick.qty >> newTick.turnover >> newTick.openInt >> newTick.bid >> newTick.ask >> newTick.bidQty >> newTick.askQty; // from historical record
        /*
          Your code is here.
          Write a code to transmit real tick data to newTick;

        */
        //cout << newTick.date << " " << newTick.time << endl;
        getTickData(newTick);
        processTick();
    }
    input.close();
    //m_outputTick.close();
    m_outputMinute.close();
    m_outputPred.close();
    m_outputIndi.close();
    
}

void WeiLinearMinute::openBook(const string& bookFile) {
  m_input.open(bookFile.c_str());
  string head;
  getline(m_input,head);
}


void WeiLinearMinute::processBar() {
  string item;
  getline(m_input,item);
  if (m_input.fail()) {
    m_input.close();
    return;
  }
  stringstream str;
  str << item;
  str >> m_entryTick.m_date >> m_entryTick.m_time >> m_entryTick.m_timeInt  >> m_entryTick.m_milli >> m_entryTick.m_price >> m_entryTick.m_openInt >> m_entryTick.m_qty >> m_entryTick.m_bid >> m_entryTick.m_ask >> m_entryTick.m_bidQty >> m_entryTick.m_askQty >> m_entryTick.m_open >> m_entryTick.m_high >> m_entryTick.m_low >> m_entryTick.m_close; // from historical record
  m_entryTick.m_upperLimit=false;
  m_entryTick.m_lowerLimit=false;
  if (m_entryTick.m_bid==0 && m_entryTick.m_ask>0) {
    m_entryTick.m_lowerLimit=true;
    m_entryTick.m_bid=m_entryTick.m_ask-m_spread;
  }
  if (m_entryTick.m_bid>0 && m_entryTick.m_ask==0) {
    m_entryTick.m_upperLimit=true;
    m_entryTick.m_ask=m_entryTick.m_bid+m_spread;
  }
  m_book.push_back(m_entryTick);
  m_minuteSignal.m_startItr=m_book.begin();
  m_minuteSignal.m_bar=m_book.size()-1;
  m_minuteSignal.calTickIndicator();
}

void WeiLinearMinute::processMinute(const string& bookFile, const bool cut) {
    ifstream input(bookFile.c_str());
    string head;
    getline(input,head);
    string item;
    int dayCount=0;
    int timeInt;
    int itemCount=0;
    while (true) {
        getline(input,item);
        if (input.fail()) break;
        stringstream str;
        str << item;
        str >> m_entryTick.m_date >> m_entryTick.m_time >> m_entryTick.m_timeInt  >> m_entryTick.m_milli >> m_entryTick.m_price >> m_entryTick.m_openInt >> m_entryTick.m_qty >> m_entryTick.m_bid >> m_entryTick.m_ask >> m_entryTick.m_bidQty >> m_entryTick.m_askQty >> m_entryTick.m_open >> m_entryTick.m_high >> m_entryTick.m_low >> m_entryTick.m_close; // from historical record
        if (m_entryTick.m_time==AFTERNOON_END) {
          dayCount++;
          if (dayCount==1 && cut) continue;
        }
        if (dayCount==0 && cut) continue;
        m_outputMinute << m_entryTick.m_date << " " << m_entryTick.m_time << " " << m_entryTick.m_timeInt  << " " << m_entryTick.m_milli << " " << m_entryTick.m_price << " " << m_entryTick.m_openInt << " " << m_entryTick.m_qty << " " << m_entryTick.m_bid << " " << m_entryTick.m_ask << " " << m_entryTick.m_bidQty << " " << m_entryTick.m_askQty << " " << m_entryTick.m_open << " " << m_entryTick.m_high << " " << m_entryTick.m_low << " " << m_entryTick.m_close << endl;
        m_entryTick.m_upperLimit=false;
        m_entryTick.m_lowerLimit=false;
        if (m_entryTick.m_bid==0 && m_entryTick.m_ask>0) {
          m_entryTick.m_lowerLimit=true;
          m_entryTick.m_bid=m_entryTick.m_ask-m_spread;
        }
        if (m_entryTick.m_bid>0 && m_entryTick.m_ask==0) {
          m_entryTick.m_upperLimit=true;
          m_entryTick.m_ask=m_entryTick.m_bid+m_spread;
        }
        m_book.push_back(m_entryTick);
        m_minuteSignal.m_startItr=m_book.begin();
        m_minuteSignal.m_bar=m_book.size()-1;
        getAction();
        int totalPos=accumulate(m_position.begin(), m_position.end(), 0);
        m_outputPred << m_entryTick.m_date << " " <<  m_entryTick.m_time  << " "  << m_entryTick.m_price << " " << m_entryTick.m_bid << " " << m_entryTick.m_ask;
        itemCount++;
        for (size_t i=0; i!=m_minuteSignal.m_strat.size();  ++i)
          m_outputPred << " " << m_minuteSignal.m_action[i] << " " << m_minuteSignal.m_pred[i] << " " << m_position[i];
        m_outputPred << endl;
        m_outputIndi << m_entryTick.m_date << " " << m_entryTick.m_time;
        for (auto item:m_minuteSignal.m_mapIndicator)
          m_outputIndi << " " << item.second;
        m_outputIndi << endl;
    }
    input.close();
    //    m_outputPred.close();
}







