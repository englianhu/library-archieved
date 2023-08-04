#!/usr/bin/env python3


import os
import sys


def getPWDFileAndDirs():
    return os.listdir(os.getcwd())


def findPos(dir, fileAndDirs):
    for i in range(len(fileAndDirs)):
        if dir == fileAndDirs[i]:
            return i

def PrintTitle(fileObject):
    fileObject.write('# 书籍\n个人书籍收藏\n\n无论闲暇还是烦躁都可以坐下来感受纸上之美。:smile:\n')

        
def GetBookList(fileObject, EmojiMap):
    directorys = {}
    for eachFileAndDir in getPWDFileAndDirs():
        if os.path.isdir(eachFileAndDir) and (not eachFileAndDir.startswith(".")):
            directorys[eachFileAndDir] = []
            for eachPDF in os.listdir(os.getcwd()+"/"+eachFileAndDir):
                directorys[eachFileAndDir].append("[" + eachPDF + "](https://github.com/SuperCV/Book/blob/master/"
                                                  + eachFileAndDir + "/" + eachPDF + ")")

    for key, value in directorys.items():
        fileObject.write("\n**"+key+"**\n")
        for eachfile in value:
            fileObject.write("* " + EmojiMap[key] + eachfile + "\n")
        fileObject.write("\n")

if __name__ == '__main__':
    Emoji = {"AI" : ":runner:",
             "CXX" : ":rose:",
             "Git" : ":octocat:",
             "Python" : ":snake:",
             "Qt" : ":battery:",
             "前端" : ":elephant:",
             "技术杂项" : ":unlock:",
             "操作系统" : ":computer:",
             "数学" : ":triangular_ruler:",
             "数据结构和算法" : ":anchor:",
             "朗道理论物理教程" : ":bomb:",
             "计算机视觉" : ":see_no_evil:",
             "设计模式" : ":hocho:",
             "Boost库" : ":rocket:",
             "鸡血" : ":four_leaf_clover:",
             "CMake" : ":paperclip:",
             "量化分析" : ":euro:",
    }
    fileObject = open("README.md", "tw")
    PrintTitle(fileObject)
    GetBookList(fileObject, Emoji)
