#include <Rcpp.h> // 调用Rcpp头文件
using namespace Rcpp; // 使用命名空间

#include <vector>
#include <algorithm>

// [[Rcpp::export]]

NumericVector getVolumeSplit(NumericVector qty, int chunk) { // 程序参数列表
  int len=qty.size();
  Rcpp::NumericVector yEnd(len);
  for (int tick=0; tick!=len; ++tick) { // 遍历全部行情
    int total=0; // 总成交量
    int i=tick; // 当且行情
    while (i<len-1 && total<chunk) { // 查找分割点
      i++;
      total+=qty[i];
    }
    yEnd[tick]=i+1;
  }
  return yEnd;
}
