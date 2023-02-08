#include "minuteTick.h"
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>
using namespace std;



// USE G++ -std=c++11 -c minuteTick.cpp to compile
string intToTime(const int& time) {
    int x=time;
    int second=x%60;
    (x-=second)/=60;
    int minute=x%60;
    int hour=(x-=minute)/60;
    string str="";
    if (hour<10) str+="0";
    str+=to_string(hour);
    str+=":";
    if (minute<10) str+="0";
    str+=to_string(minute);
    str+=":";
    if (second<10) str+="0";
    str+=to_string(second);
    return (str);
}
