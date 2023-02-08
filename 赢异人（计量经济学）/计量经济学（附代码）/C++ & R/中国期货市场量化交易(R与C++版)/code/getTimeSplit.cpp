#include <Rcpp.h> // 包含Rcpp.h的头文件
using namespace Rcpp; // 使用Rcpp命名空间

#include <vector> // 使用C++的vector库
#include <algorithm> // 使用C++的algorithm库

// [[Rcpp::export]]

NumericVector getTimeSplit(CharacterVector dataTime, CharacterVector splitTime) {
// dataTime和splitTime都是字符型向量，与R语言程序一致
  int j=0; // C++开始位置是0
  int totalBar=dataTime.size(); // 行情数目
  int nBar=splitTime.size(); // K线数目
  Rcpp::NumericVector chosenLine(nBar); // 切割位置
  for (int i=0; i!=nBar; ++i) { // 寻找下一个位置
    while (j<totalBar && dataTime[j]<=splitTime[i]) j++;
    chosenLine[i]=j; // 设置切割位置
  }
  return chosenLine; // 返回切割为主
}
